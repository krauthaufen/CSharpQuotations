using Roslyn.Compilers.CSharp;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.Quotations
{
    
    /// <summary>
    /// Syntax visitor compiling to expressions.
    /// the returned type is a function expecting a sequence of expressions as argument which will be treated as continuation.
    /// This special return-type is necessary for some scenarios like variable-binds/etc. (where the continuation has to be nested in the declaration itself)
    /// The CompileVisitor (at the moment) carries state which means that for each compilation a unique visitor must be used.
    /// TODO: this visitor could use ComputationExpressions provided by F# whereas currently we have to write monadic code in C#. (rather cumbersume)
    /// </summary>
    public class CompileVisitor : SyntaxVisitor<Func<IEnumerable<Expression>, Expression>>
    {
        
        private MethodBase m_method;
        private Type m_type;
        private Dictionary<string, Type> m_genericArguments;

        private ParameterExpression m_this;
        private CompiledNamespaceAttribute[] m_namespaces;

        private Dictionary<string, ParameterExpression> m_knownVariables = new Dictionary<string, ParameterExpression>();
        private LabelTarget m_currentBreakLabel;
        private LabelTarget m_currentContinueLabel;
        private LabelTarget m_functionEndLabel;

        #region Compile

        //private helper functions for compiling several types of Roslyn's syntax nodes

        /// <summary>
        /// compiles an arbitrary snytax node using the overriden visitor-methods
        /// </summary>
        private Func<IEnumerable<Expression>, Expression> Compile(SyntaxNode node)
        {
            return node.Accept(this);
        }

        /// <summary>
        /// compiles a block and provides the ability to ignore the returned value
        /// this is useful in situations where two blocks are known to be void
        /// </summary>
        private Func<IEnumerable<Expression>, Expression> Compile(BlockSyntax node, bool ignore = false)
        {
            return Bind(node.Accept(this), r =>
                {
                    return Return(ignore ? Expression.Block(r, Expression.Empty()) : r);
                });
        }

        /// <summary>
        /// visits a CatchClauseSyntax and produces a CatchBlock
        /// </summary>
        private Func<IEnumerable<Expression>, CatchBlock> Compile(CatchClauseSyntax node)
        {
            var exceptionType = Resolve(node.Declaration.Type);
            return e =>
                    {
                        var body = Compile(node.Block, true)(Enumerable.Empty<Expression>());
                        return Expression.MakeCatchBlock(exceptionType, null, body, null);
                    };
        }

        /// <summary>
        /// visits a FinallyClauseSyntax and produces an Expression
        /// </summary>
        private Func<IEnumerable<Expression>, Expression> Compile(FinallyClauseSyntax node, bool ignore = false)
        {
            //TODO: implement ignore
            return Compile((SyntaxNode)node);
        }

        /// <summary>
        /// visits a SwitchSectionSyntax and produces a SwitchCase
        /// </summary>
        private Func<IEnumerable<Expression>, SwitchCase> Compile(SwitchSectionSyntax node)
        {
            return Bind(node.Statements.Select(Compile).ToArray(), b =>
                    Bind<Expression, SwitchCase>(node.Labels.Select(l => Compile(l.Value)).ToArray(), l =>
                    _ => Expression.SwitchCase(Expression.Block(b), l)));
        }

        #endregion

        #region Constructors

        /// <summary>
        /// creates a new CompileVisitor for the given method and type arguments
        /// types may be null if the method does not need further type arguments
        /// </summary>
        private CompileVisitor(MethodBase method, Type[] types)
        {
            m_method = method;
            m_type = m_method.DeclaringType;
            m_namespaces = Expr.GetNamespaces(m_method).ToArray();
            var returnType = ((MethodInfo)method).ReturnType;
            m_functionEndLabel = Expression.Label(returnType, "ret");
            m_this = null;
            var mi = (MethodInfo)method;

            if (method.IsGenericMethod)
            {
                m_genericArguments = new Dictionary<string, Type>();
                
                var names = mi.GetGenericMethodDefinition().GetGenericArguments();
                if(types.Length == 0)
                    types = mi.GetGenericArguments();
                
                for (int i = 0; i < names.Length; i++)
                {
                    if (i < types.Length)
                    {
                        m_genericArguments[names[i].Name] = types[i];
                    }
                }
            }

        }

        #endregion

        #region Types

        //TODO: maybe some issues resolving generic types
        //TODO: add support for alias-usings like "using A = ..." in "new A.SomeType()"

        private Type Resolve(PredefinedTypeSyntax pd)
        {
            switch (pd.Keyword.ValueText)
            {
                case "bool": return typeof(bool);

                case "byte": return typeof(byte);
                case "sbyte": return typeof(sbyte);

                case "char": return typeof(char);
                case "short": return typeof(short);
                case "ushort": return typeof(ushort);

                case "int": return typeof(int);
                case "uint": return typeof(uint);
                case "float": return typeof(float);

                case "long": return typeof(long);
                case "ulong": return typeof(ulong);
                case "double": return typeof(double);


                case "decimal": return typeof(decimal);
                case "string": return typeof(string);
                case "void": return typeof(void);

                default:
                    throw new Exception(pd.Keyword.ValueText);
            }
        }

        private Type Resolve(IdentifierNameSyntax type)
        {
            return Resolve(type.Identifier.ToString());
        }

        private Type Resolve(QualifiedNameSyntax type)
        { 
            var name = type.ToString();
            var direct = Type.GetType(name);
            if (direct != null) return direct;

            throw new NotImplementedException("should consider aliases etc. here");
        }

        private Type Resolve(TypeSyntax type)
        {
            var pd = type as PredefinedTypeSyntax;
            var nameType = type as IdentifierNameSyntax;
            var qualType = type as QualifiedNameSyntax;
            var gen = type as GenericNameSyntax;
            var arr = type as ArrayTypeSyntax;

            if (type == null) return null;
            else if (pd != null) return Resolve(pd);
            else if (type.IsVar) return null;
            else if (arr != null) return Resolve(arr.ElementType).MakeArrayType();
            else if (nameType != null) return Resolve(nameType);
            else if (qualType != null) return Resolve(qualType);
            else if (gen != null) return Resolve(string.Format("{0}`{1}", gen.Identifier, gen.Arity)).MakeGenericType(gen.TypeArgumentList.Arguments.Select(a => Resolve(a)).ToArray());
            else throw new NotImplementedException();
        }

        private Type Resolve(string name)
        {
            var direct = Type.GetType(name);
            if (direct != null) return direct;

            var mine = m_type.Assembly.GetType(m_type.Namespace + "." + name);
            if (mine != null) return mine;

            var me = Assembly.GetEntryAssembly();
            if (me == null) me = Assembly.GetCallingAssembly();

            //TODO: ensure that all assemblies are loaded (should be incremental)
            foreach (var r in me.GetReferencedAssemblies())
            {
                Assembly.Load(r);
            }

            var assemblies = AppDomain.CurrentDomain.GetAssemblies();

            foreach (var a in assemblies)
            {
                foreach (var n in m_namespaces)
                {
                    if (n.Alias == null)
                    {
                        var probe = a.GetType(n.Namespace + "." + name);
                        if (probe != null) return probe;
                    }
                }
            }

            foreach (var n in m_namespaces)
            {
                if (n.Alias == null)
                {
                    var probe = Type.GetType(n.Namespace + "." + name);
                    if (probe != null) return probe;
                }
            }

            Type argType;
            if (m_genericArguments != null && m_genericArguments.TryGetValue(name, out argType))
            {
                return argType;
            }


            throw new TypeLoadException(name);
        }

        #endregion

        #region Methods

        //TODO: implement correct resolve for generic methods (since not provided by DefaultBinder)

        private static bool TryUnify(Type parameters, Type arguments, Dictionary<Type, Type> assignment)
        {
            Type result;
            if (assignment.TryGetValue(parameters, out result)) return true;
            else
            {
                if (parameters.IsGenericParameter)
                {
                    assignment[parameters] = arguments;
                    return true;
                }
                else if (parameters.GetGenericTypeDefinition() == arguments.GetGenericTypeDefinition())
                {
                    var gp = parameters.GetGenericArguments();
                    var ap = arguments.GetGenericArguments();
                    for (int i = 0; i < gp.Length; i++)
                    {
                        if (!TryUnify(gp[i], ap[i], assignment))
                            return false;
                    }

                    return true;
                }
                else return false;
            }
        }

        private static MethodInfo Unifiy(MethodInfo gen, Type[] argumentTypes)
        {
            var assignment = new Dictionary<Type, Type>();

            var parameterTypes = gen.GetParameters().Select(pi => pi.ParameterType).ToArray();

            for (int i = 0; i < parameterTypes.Length; i++)
            {
                if (!TryUnify(parameterTypes[i], argumentTypes[i], assignment))
                    throw new Exception();
            }

            var ordered = gen.GetGenericArguments().Select(pi => assignment[pi]).ToArray();

            return gen.MakeGenericMethod(ordered);
        }

        private MethodInfo GetMethod(Type type, NameSyntax name, IEnumerable<Expression> arguments)
        {
            
                var mi = type.GetMethod(
                    name.ToString(),
                    BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static,
                    Type.DefaultBinder,
                    arguments.Select(a => a.Type).ToArray(),
                    arguments.Select(a => new ParameterModifier()).ToArray());

                if (mi != null)
                    return mi;
                else
                {
                    mi = type.GetMethods()
                         .Where(mx => mx.Name == name.ToString())
                         .SingleOrDefault(mx => mx.GetParameters().Length == arguments.Count());

                    if (mi != null)
                    {
                        if (mi.IsGenericMethod)
                        {
                            mi = Unifiy(mi, arguments.Select(a => a.Type).ToArray());
                        }

                        return mi;
                    }
                    else
                    {

                        var extensions = AppDomain.CurrentDomain.GetAssemblies()
                                                                .SelectMany(a =>
                                                                 {
                                                                     try { return a.GetTypes(); }
                                                                     catch (ReflectionTypeLoadException e) { return e.Types.Where(t => t != null).ToArray(); }
                                                                 })
                                                                .SelectMany(t => t.GetMethods(BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public))
                                                                .Where(m => m.IsDefined(typeof(ExtensionAttribute)))
                                                                .Where(m => m.Name == name.ToString())
                                                                .Select(e => e)
                                                                .ToArray();


                        var args = arguments.ToArray();
                        mi = (MethodInfo)Type.DefaultBinder.SelectMethod(
                                                BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public,
                                                extensions,
                                                new[] { type }.Concat(args.Select(a => a.Type)).ToArray(),
                                                new ParameterModifier[args.Length + 1]);
                        return mi;
                    }

                }
        }


        #endregion

        #region Values
        
        //TODO: add parsers for other literal-types

        private static object ParseNumeric(string numeric)
        {
            var n = numeric.ToLower();

            if (n.Contains("."))
            {
                if (n.EndsWith("f")) return float.Parse(n.Substring(0, n.Length -1), CultureInfo.InvariantCulture.NumberFormat);
                else return double.Parse(n, CultureInfo.InvariantCulture.NumberFormat);
            }
            else
            {
                if (n.EndsWith("ul")) return ulong.Parse(n);
                else if (n.EndsWith("l")) return long.Parse(n);
                else return int.Parse(n);
            }
        }


        #endregion

        #region Variables / Parameters

        private ParameterExpression CompileParameter(ParameterSyntax pi, Type t = null)
        {
            var name = pi.Identifier.ToString();
            ParameterExpression pe;

            var attributes = pi.AttributeLists.SelectMany(al => al.Attributes)
                               .Select(al => Compile(al)(Enumerable.Empty<Expression>()))
                               .ToArray();
           
            if (!m_knownVariables.TryGetValue(name, out pe))
            {
                pe = Expression.Parameter(t ?? Resolve(pi.Type), name);
                
                m_knownVariables[name] = pe;
            }

            return pe.WithAttributes(attributes);
        }

        private ParameterExpression CompileParameter(Type type, string name)
        {
            ParameterExpression pe;

            if (!m_knownVariables.TryGetValue(name, out pe))
            {
                pe = Expression.Parameter(type, name);
                m_knownVariables[name] = pe;
            }

            return pe;
        }


        private TypeSyntax expectedType = null;
        private Tuple<IEnumerable<ParameterExpression>, IEnumerable<Expression>> CompileVariable(VariableDeclarationSyntax d)
        {
            
            var type = Resolve(d.Type);
            var tuples = d.Variables.Select(vd =>
                {
                    var oldExpectedType = expectedType;
                    expectedType = d.Type;
                    var value = Compile(vd.Initializer)(Enumerable.Empty<Expression>());
                    var p = CompileParameter(type ?? value.Type, vd.Identifier.ToString());

                    expectedType = oldExpectedType;

                    return Tuple.Create(p, Expression.Assign(p, value));
                }).ToArray();


            return Tuple.Create(tuples.Select(t => t.Item1), tuples.Select(t => (Expression)t.Item2));
        }

        #endregion

        #region Monad

        //methods representing a classical Monad-API making compositions of compilation-code easier

        private Func<IEnumerable<Expression>, TOut> Bind<TIn, TOut>(Func<IEnumerable<Expression>, TIn> m, Func<TIn, Func<IEnumerable<Expression>, TOut>> f)
        {
            return e => f(m(Enumerable.Empty<Expression>()))(e);
        }

        private Func<IEnumerable<Expression>, TOut> Bind<TIn, TOut>(Func<IEnumerable<Expression>, TIn>[] m, Func<TIn[], Func<IEnumerable<Expression>, TOut>> f)
        {
            return e => f(m.Select(mi => mi(Enumerable.Empty<Expression>())).ToArray())(e);
        }


        private static Func<IEnumerable<Expression>, Expression> Return(Expression e)
        {
            return cont =>
                {
                    if (cont.Any())
                        return Expression.Block(new[] { e }.Concat(cont));
                    else
                        return e;
                };
        }

        private static Func<IEnumerable<Expression>, Expression> ReturnFrom(Func<IEnumerable<Expression>, Expression> e)
        {
            return e;
        }

        private Func<IEnumerable<Expression>, Expression> WithLabels(LabelTarget breakLabel, LabelTarget continueLabel, Func<IEnumerable<Expression>, Expression> m)
        {
            return e =>
            {
                var oldBreak = m_currentBreakLabel;
                var oldContinue = m_currentContinueLabel;

                m_currentBreakLabel = breakLabel;
                m_currentContinueLabel = continueLabel;

                var result = m(e);

                m_currentBreakLabel = oldBreak;
                m_currentContinueLabel = oldContinue;

                return result;
            };
        }

        #endregion

        #region Functions

        //TODO: add missing operators

        private static Func<Expression, Expression, Expression> BinaryOperator(SyntaxKind kind)
        {
            switch (kind)
            {
                case SyntaxKind.AddAssignExpression:
                    return (a, b) => Expression.AddAssign(a, b);
                case SyntaxKind.AddExpression:
                    return (a, b) => Expression.Add(a, b);
                case SyntaxKind.AndAssignExpression:
                    return (a, b) => Expression.AddAssign(a, b);
                case SyntaxKind.AssignExpression:
                    return (a, b) => Expression.Assign(a, b);
                case SyntaxKind.BitwiseAndExpression:
                    return (a, b) => Expression.And(a, b);
                case SyntaxKind.BitwiseOrExpression:
                    return (a, b) => Expression.Or(a, b);
                case SyntaxKind.DivideAssignExpression:
                    return (a, b) => Expression.DivideAssign(a, b);
                case SyntaxKind.DivideExpression:
                    return (a, b) => Expression.Divide(a, b);
                case SyntaxKind.EqualsExpression:
                    return (a, b) => Expression.Equal(a, b);
                case SyntaxKind.GreaterThanExpression:
                    return (a, b) => Expression.GreaterThan(a, b);
                case SyntaxKind.GreaterThanOrEqualExpression:
                    return (a, b) => Expression.GreaterThanOrEqual(a, b);
                case SyntaxKind.LeftShiftAssignExpression:
                    return (a, b) => Expression.LeftShiftAssign(a, b);
                case SyntaxKind.LeftShiftExpression:
                    return (a, b) => Expression.LeftShift(a, b);
                case SyntaxKind.LessThanExpression:
                    return (a, b) => Expression.LessThan(a, b);
                case SyntaxKind.LessThanOrEqualExpression:
                    return (a, b) => Expression.LessThanOrEqual(a, b);
                case SyntaxKind.MultiplyAssignExpression:
                    return (a, b) => Expression.MultiplyAssign(a, b);
                case SyntaxKind.MultiplyExpression:
                    return (a, b) => Expression.Multiply(a, b);
                case SyntaxKind.NotEqualsExpression:
                    return (a, b) => Expression.NotEqual(a, b);
                case SyntaxKind.OrAssignExpression:
                    return (a, b) => Expression.OrElse(a, b);
                case SyntaxKind.PlusExpression:
                    return (a, b) => Expression.Add(a, b);
                case SyntaxKind.RightShiftAssignExpression:
                    return (a, b) => Expression.RightShiftAssign(a, b);
                case SyntaxKind.RightShiftExpression:
                    return (a, b) => Expression.RightShift(a, b);
                case SyntaxKind.SubtractAssignExpression:
                    return (a, b) => Expression.SubtractAssign(a, b);
                case SyntaxKind.SubtractExpression:
                    return (a, b) => Expression.Subtract(a, b);
                case SyntaxKind.LogicalAndExpression:
                    return (a, b) => Expression.AndAlso(a, b);
                case SyntaxKind.LogicalOrExpression:
                    return (a, b) => Expression.OrElse(a, b);
                case SyntaxKind.ModuloExpression:
                    return (a, b) => Expression.Modulo(a, b);
                default:
                    throw new NotImplementedException();
            }
        }

        #endregion

        #region Visitors

        private ParameterExpression This
        {
            get
            {
                if (m_this != null) return m_this;
                else
                {
                    m_this = Expression.Parameter(m_type, "self");
                    return m_this;
                }
            }
        }

        private bool HasThis
        {
            get { return m_this != null; }
        }

        //Expressions
        public override Func<IEnumerable<Expression>, Expression> VisitBinaryExpression(BinaryExpressionSyntax node)
        {
            var op = BinaryOperator(node.Kind);

            var l = Compile(node.Left);
            var r = Compile(node.Right);

            return Bind(l, li => Bind(r, ri => Return(op(li, ri))));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            var parameters = node.ParameterList.Parameters.Select(pi => CompileParameter(pi)).ToArray();
            var body = Compile(node.Body);

            return Bind(body, b => Return(Expression.Lambda(Expression.Block(b, Expression.Label(m_functionEndLabel, Expression.Default(m_functionEndLabel.Type))), parameters)));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitParameter(ParameterSyntax node)
        {
            return Return(CompileParameter(node));
        }


        public override Func<IEnumerable<Expression>, Expression> VisitThisExpression(ThisExpressionSyntax node)
        {
            return Return(This);
        }

        public override Func<IEnumerable<Expression>, Expression> VisitIdentifierName(IdentifierNameSyntax node)
        {
            var flags = BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance;
 
            var name = node.Identifier.ToString();


            ParameterExpression e;
            if (m_knownVariables.TryGetValue(name, out e)) return Return(e);
            else
            {
                var types = new HashSet<Type>(){ m_type };

                while (types.Count > 0)
                {
                    var nextSet = new HashSet<Type>();
                    foreach (var type in types)
                    {
                        var field = type.GetField(name, flags);
                        var prop = type.GetProperty(name, flags);

                        if (field != null)
                        {
                            if (field.IsStatic) return Return(Expression.Field(null, field));
                            else return Return(Expression.Field(This, field));
                        }
                        else if (prop != null)
                        {
                            var get = prop.GetMethod;
                            if (get.IsStatic) return Return(Expression.Property(null, get));
                            else return Return(Expression.Property(This, get));
                        }
                        else
                        {
                            if(type.BaseType != null) nextSet.Add(type.BaseType);
                            nextSet.UnionWith(type.GetInterfaces());
                        }
                    }


                    types = nextSet;

                }
                

            }


            throw new NotImplementedException();
        }

        public override Func<IEnumerable<Expression>, Expression> VisitBlock(BlockSyntax node)
        {
            Func<IEnumerable<Expression>, IEnumerable<Expression>> seed = a => a;

            var compiled = node.Statements.Select(si => Compile(si)).ToList();
            
            var result = compiled.Aggregate(seed, (l, s) => c => l(new[]{ s(c) }));

            return Bind(result, l => Return(Expression.Block(l)));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitReturnStatement(ReturnStatementSyntax node)
        {
            //return Compile(node.Expression);
            return Bind(Compile(node.Expression), e =>
                    Return(Expression.Return(m_functionEndLabel, e)));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            if (node.Kind == SyntaxKind.NumericLiteralExpression) return Return(Expression.Constant(ParseNumeric(node.Token.ToString())));
            else throw new NotImplementedException();
        }

        public override Func<IEnumerable<Expression>, Expression> VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            var p = CompileVariable(node.Declaration);
            return c =>
                {
                    return Expression.Block(p.Item1, p.Item2.Concat(c));
                };
        }

        public override Func<IEnumerable<Expression>, Expression> VisitEqualsValueClause(EqualsValueClauseSyntax node)
        {
            return node.Value.Accept(this);
        }

        public override Func<IEnumerable<Expression>, Expression> VisitConditionalExpression(ConditionalExpressionSyntax node)
        {
            return Bind(Compile(node.Condition), c =>
                   Bind(Compile(node.WhenTrue), t =>
                   Bind(Compile(node.WhenFalse), f =>
                        Return(Expression.Condition(c, t, f))
                   )));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitCastExpression(CastExpressionSyntax node)
        {
            var type = Resolve(node.Type);

            return Bind(Compile(node.Expression), e =>
                   Return(Expression.Convert(e, type)));

        }

        public override Func<IEnumerable<Expression>, Expression> VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
        {
            var operand = Compile(node.Operand)(Enumerable.Empty<Expression>());
            switch (node.Kind)
            {
                case SyntaxKind.PostIncrementExpression:
                    return Return(Expression.PostIncrementAssign(operand));
                case SyntaxKind.PostDecrementExpression:
                    return Return(Expression.PostDecrementAssign(operand));
                default:
                    throw new NotImplementedException();
            }
        }

        
        public override Func<IEnumerable<Expression>, Expression> VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            return Bind(Compile(node.Expression), e => 
                {
                    if(e.Type == typeof(void))return Return(e);
                    else return Return(Expression.Call(Intrinsics.Ignore.MakeGenericMethod(e.Type), e));
                });
        }

        public override Func<IEnumerable<Expression>, Expression> VisitParenthesizedExpression(ParenthesizedExpressionSyntax node)
        {
            return Compile(node.Expression);
        }

        public override Func<IEnumerable<Expression>, Expression> VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
        {
            return Bind(Compile(node.Operand), o =>
                {
                    //TOOD: not always negation
                    return Return(Expression.MakeUnary(ExpressionType.Negate, o, o.Type));
                });
        }


        //Control Flow
        public override Func<IEnumerable<Expression>, Expression> VisitIfStatement(IfStatementSyntax node)
        {
            var c = Compile(node.Condition);
            var t = Compile(node.Statement);
            var e = node.Else != null ? Compile(node.Else.Statement) : null;

            if (node.Else != null)
            {
                return Bind(c, ci =>
                        Bind(t, ti =>
                         Bind(e, ei =>
                            Return(Expression.IfThenElse(ci, ti, ei))
                         )
                        )
                       );

            }
            else
            {
                return Bind(c, ci =>
                        Bind(t, ti =>
                            Return(Expression.IfThen(ci, ti))
                        )
                       );

            }
        }

        public override Func<IEnumerable<Expression>, Expression> VisitSwitchStatement(SwitchStatementSyntax node)
        {
            var sections = node.Sections.Where(s => s.Labels.All(l => l.Value != null)).Select(Compile).ToArray();
            var defSection = node.Sections.Where(s => s.Labels.Any(l => l.Value == null)).SelectMany(si => si.Statements).Select(Compile).FirstOrDefault();

            var @breakLabel = Expression.Label();

            if (defSection == null)
            {
                return WithLabels(breakLabel, breakLabel,
                       Bind(defSection, d =>
                       Bind(Compile(node.Expression), e =>
                       Bind(sections, s =>
                            Return(Expression.Block(new Expression[]{
                                Expression.Switch(e, d, s),
                                Expression.Label(@breakLabel)}))
                       ))));
            }
            else
            {
                return WithLabels(breakLabel, breakLabel,
                       Bind(Compile(node.Expression), e =>
                       Bind(sections, s =>
                            Return(Expression.Block(new Expression[]{
                                Expression.Switch(e, s),
                                Expression.Label(@breakLabel)}))
                       )));
            }
        }

        public override Func<IEnumerable<Expression>, Expression> VisitForStatement(ForStatementSyntax node)
        {
            var initializers = CompileVariable(node.Declaration);
            var incrementors = node.Incrementors.Select(i => Compile(i)).ToArray();
            var condition = Compile(node.Condition);
            var body = Compile(node.Statement);
            var breakLabel = Expression.Label(typeof(void));
            var continueLabel = Expression.Label(typeof(void));

            var r = WithLabels(breakLabel, continueLabel,
                    Bind(condition, c =>
                    Bind(incrementors, ic =>
                    Bind(body, b =>
                        Return(new ForExpression(initializers, c, ic, b, breakLabel, continueLabel))
                    ))));
            return r;
        }

        public override Func<IEnumerable<Expression>, Expression> VisitForEachStatement(ForEachStatementSyntax node)
        {
            var breakLabel = Expression.Label();
            var continueLabel = Expression.Label();
            var type = Resolve(node.Type);
            var name = node.Identifier.ToString();

            return WithLabels(breakLabel, continueLabel, 
                   Bind(Compile(node.Expression), e =>
                   {
                       var elementType = type;
                       if (type == null)
                       {
                           if (e.Type.IsArray)
                           {
                               elementType = e.Type.GetElementType();
                           }
                           else
                           {
                               var enumeratorTypes = new[] { e.Type.GetMethod("GetEnumerator", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy).ReturnType };

                               while (enumeratorTypes.Length > 0)
                               {
                                   var found = enumeratorTypes.FirstOrDefault(et => et.IsGenericType && et.GetGenericTypeDefinition() == typeof(IEnumerator<>));
                                   if (found != null)
                                   {
                                       elementType = found.GetGenericArguments()[0];
                                       break;
                                   }
                                   else enumeratorTypes = enumeratorTypes.SelectMany(et => et.GetInterfaces()).ToArray();
                               }
                           }
                       }

                       var p = CompileParameter(elementType, name);

                       return Bind(Compile(node.Statement), b =>
                                   Return(new ForEachExpression(p, e, b, breakLabel, continueLabel)));
                   }));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitWhileStatement(WhileStatementSyntax node)
        {
            var condition = Compile(node.Condition);
            var body = Compile(node.Statement);
            var breakLabel = Expression.Label(typeof(void));
            var continueLabel = Expression.Label(typeof(void));


            var oldLabel = m_currentBreakLabel;
            m_currentBreakLabel = breakLabel;
            var r = WithLabels(breakLabel, continueLabel,
                    Bind(condition, c =>
                    Bind(body, b =>
                        Return(new WhileExpression(c, b, breakLabel, continueLabel))
                    )));
            m_currentBreakLabel = oldLabel;
            return r;
        }

        public override Func<IEnumerable<Expression>, Expression> VisitDoStatement(DoStatementSyntax node)
        {
            var condition = Compile(node.Condition);
            var body = Compile(node.Statement);
            var breakLabel = Expression.Label(typeof(void));
            var continueLabel = Expression.Label(typeof(void));


            var oldLabel = m_currentBreakLabel;
            m_currentBreakLabel = breakLabel;
            var r = WithLabels(breakLabel, continueLabel,
                    Bind(condition, c =>
                    Bind(body, b =>
                        Return(new DoWhileExpression(c, b, breakLabel, continueLabel))
                    )));
            m_currentBreakLabel = oldLabel;
            return r;
        }

        public override Func<IEnumerable<Expression>, Expression> VisitBreakStatement(BreakStatementSyntax node)
        {
            return e => Expression.Break(m_currentBreakLabel);
        }

        public override Func<IEnumerable<Expression>, Expression> VisitContinueStatement(ContinueStatementSyntax node)
        {
            return e => Expression.Continue(m_currentContinueLabel);
        }

        //Object creation
        public override Func<IEnumerable<Expression>, Expression> VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
        {
            var initializers = node.Initializer == null ? null : node.Initializer.Expressions.Select(e => Compile(e)).ToArray();

            var type = Resolve(node.Type);
            var args = node.ArgumentList.Arguments.Select(a => Compile(a.Expression)).ToArray();


            if (initializers == null)
            {
                return Bind(args, a =>
                       {
                           var ctor = type.GetConstructor(a.Select(ai => ai.Type).ToArray());

                           return Return(Expression.New(ctor, a));
                       });
            }
            else
            {
                return Bind(args, a =>
                       Bind(initializers, i =>
                        {
                            var ctor = type.GetConstructor(a.Select(ai => ai.Type).ToArray());


                            var value = Expression.New(ctor, a);

                            var add = value.Type.GetMethod("Add");

                            return Return(Expression.ListInit(value, i.Select(ii => Expression.ElementInit(add, ii)).ToArray()));
                        }));
            }
        }

        public override Func<IEnumerable<Expression>, Expression> VisitArrayCreationExpression(ArrayCreationExpressionSyntax node)
        {
            throw new NotImplementedException();
        }

        public override Func<IEnumerable<Expression>, Expression> VisitInitializerExpression(InitializerExpressionSyntax node)
        {
            throw new NotImplementedException();
        }

        public override Func<IEnumerable<Expression>, Expression> VisitImplicitArrayCreationExpression(ImplicitArrayCreationExpressionSyntax node)
        {
            var init = node.Initializer.Expressions.Select(e => Compile(e)).ToArray();
            return Bind(init, i =>
            {
                //TODO: unify types here
                var type = i.First().Type;
                return Return(Expression.NewArrayInit(type, i));
            });
        }

        //Exceptions
        public override Func<IEnumerable<Expression>, Expression> VisitTryStatement(TryStatementSyntax node)
        {
            if (node.Finally != null && node.Catches != null && node.Catches.Count > 0)
            { 
                return Bind(Compile(node.Block, true), b =>
                       Bind(node.Catches.Select(Compile).ToArray(), catches =>
                       Bind(Compile(node.Finally), fin =>
                        Return(Expression.TryCatchFinally(b,fin, catches.OfType<CatchBlock>().ToArray()))
                       )));
            }
            else if (node.Catches != null && node.Catches.Count > 0)
            {
                return Bind(Compile(node.Block, true), b =>
                       Bind(node.Catches.Select(Compile).ToArray(), catches =>
                        Return(Expression.TryCatch(b, catches.OfType<CatchBlock>().ToArray()))
                       ));
            }
            else /* if (node.Finally != null) */
            {
                return Bind(Compile(node.Block, true), b =>
                       Bind(Compile(node.Finally, true), fin =>
                        Return(Expression.TryFinally(b, fin))
                       ));
            }
        }

        public override Func<IEnumerable<Expression>, Expression> VisitThrowStatement(ThrowStatementSyntax node)
        {
            return Bind(Compile(node.Expression), ex =>
                    Return(Expression.Throw(ex)));
        }

        //Calls
        public override Func<IEnumerable<Expression>, Expression> VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            var method = node.Expression as MemberAccessExpressionSyntax;
            var name = node.Expression as SimpleNameSyntax;

            if (method != null)
            {
                var id = method.Expression;

                Func<Type> type = () =>
                    {
                        try
                        {
                            return Resolve(method.Expression.ToString());
                        }
                        catch (TypeLoadException)
                        {
                            return null;
                        }
                    };

                if (!m_knownVariables.ContainsKey(id.ToString()) && type() != null)
                {
                    return Bind<Expression, Expression>(node.ArgumentList.Arguments.Select(a => Compile(a.Expression)).ToArray(), a =>
                           {
                               return Return(Expression.Call(null, GetMethod(type(), method.Name, a), a));
                           });
                }
                else
                {
                    return Bind(Compile(method.Expression), e =>
                           {
                               return Bind<Expression, Expression>(node.ArgumentList.Arguments.Select(a => Compile(a.Expression)).ToArray(), a =>
                               {
                                   var mi = GetMethod(e.Type, method.Name, a);

                                   var parameters = mi.GetParameters();
                                   if (mi.IsDefined(typeof(ExtensionAttribute)) && parameters.Length > 0 && parameters[0].ParameterType == e.Type)
                                       return Return(Expression.Call(null, mi, new[] { e }.Concat(a).ToArray()));
                                   else
                                       return Return(Expression.Call(e, mi, a));
                               });
                           });
                }
            }
            else if (name != null)
            {
                ParameterExpression lambda;
                if (m_knownVariables.TryGetValue(name.ToString(), out lambda))
                {
                    return Bind<Expression, Expression>(node.ArgumentList.Arguments.Select(a => Compile(a.Expression)).ToArray(), a =>
                    {
                        return Return(Expression.Invoke(lambda, a));
                    });
                }
                else
                {
                    var e = This;
                    return Bind<Expression, Expression>(node.ArgumentList.Arguments.Select(a => Compile(a.Expression)).ToArray(), a =>
                           {
                               var mi = GetMethod(e.Type, name, a);

                               var parameters = mi.GetParameters();
                               if (mi.IsDefined(typeof(ExtensionAttribute)) && parameters.Length > 0 && parameters[0].ParameterType == e.Type)
                                   return Return(Expression.Call(null, mi, new[] { e }.Concat(a).ToArray()));
                               else
                               {
                                   if (mi.IsStatic) return Return(Expression.Call(null, mi, a));
                                   else return Return(Expression.Call(e, mi, a));
                               }
                           });
                }
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        public override Func<IEnumerable<Expression>, Expression> VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            return Bind(Compile(node.Expression), e =>
                   {
                       var field = e.Type.GetField(node.Name.ToString(), BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
                   
                       if(field != null) 
                           return Return(Expression.Field(e, field));

                       var prop = e.Type.GetProperty(node.Name.ToString(), BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
                       if (prop != null)
                           return Return(Expression.Property(e, prop));

                       throw new NotImplementedException();
                   
                   });
        }

        public override Func<IEnumerable<Expression>, Expression> VisitElementAccessExpression(ElementAccessExpressionSyntax node)
        {
            return Bind(Compile(node.Expression), e =>
                   Bind<Expression, Expression>(node.ArgumentList.Arguments.Select(a => Compile(a.Expression)).ToArray(), a =>
                   {
                       if (e.Type.IsArray)
                           return Return(Expression.ArrayAccess(e, a));

                       var prop = e.Type.GetProperty("Item", BindingFlags.Instance | BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);

                       if (prop != null)
                           return Return(Expression.Property(e, prop, a));

                       throw new NotImplementedException();
                   }));
        }

        //Lambdas
        public override Func<IEnumerable<Expression>, Expression> VisitSimpleLambdaExpression(SimpleLambdaExpressionSyntax node)
        {

            var type = Resolve(node.Parameter.Type);
            type = type ?? Resolve(expectedType).GetGenericArguments()[0];

            var parameter = CompileParameter(node.Parameter, type);

            return Bind(Compile(node.Body), b =>
                   { 
                        
                        return Return(Expression.Lambda(b, parameter));
                   });
        }

        public override Func<IEnumerable<Expression>,Expression> VisitParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node)
        {
            var types = expectedType == null ? new Type[0] : Resolve(expectedType).GetGenericArguments();
            var parameters = node.ParameterList.Parameters.Select((p, i) => CompileParameter(p, types[i])).ToArray();
            return Bind(Compile(node.Body), b =>
            {
                return Return(Expression.Lambda(b, parameters));
            });
        }


        public override Func<IEnumerable<Expression>, Expression> VisitAttribute(AttributeSyntax node)
        {
            var args = node.ArgumentList != null ? node.ArgumentList.Arguments : Enumerable.Empty<AttributeArgumentSyntax>();

            return Bind<Expression, Expression>(args.Select(ai => Compile(ai.Expression)).ToArray(), a =>
                   { 
                       var name = node.Name.ToString();
                       Type t;
                       try 
                       {
                            t = Resolve(name);
                       }
                       catch(Exception) 
                       {
                            t = Resolve(name + "Attribute");
                       }

                       var ctor = t.GetConstructor(a.Select(ai => ai.Type).ToArray());
                       return Return(Expression.New(ctor, a));
                   });
        }

        /// <summary>
        /// DefaultVisit will throw an exception when called. this is useful for finding 
        /// node-types which are not yet supported via debugging
        /// </summary>
        public override Func<IEnumerable<Expression>, Expression> DefaultVisit(SyntaxNode node)
        {
            throw new NotImplementedException();
        }

        #endregion

        #region Static Compilation

        /// <summary>
        /// Creates a new CompileVisitor and visits the supplied SyntaxTree
        /// returns the obtained expression using an empty continuation.
        /// If the given member is a static method it's converted to a lambda-function taking the exact same arguments.
        /// if the method is non-static a lambda function expecting a "self" argument is returned which will then yield 
        /// the same representation described above.
        /// </summary>
        public static Expression Compile(MethodBase method, SyntaxTree tree)
        {
            var m = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().FirstOrDefault();
            var v = new CompileVisitor(method, new Type[0]);

            var expression = v.Visit(m)(Enumerable.Empty<Expression>());
            if (!method.IsStatic) return Expression.Lambda(expression, v.This);
            else return expression;
        }

        #endregion
    }


    /// <summary>
    /// static class holding special-attributes for all expressions in a ConditionalWeakTable.
    /// attributes can be freely added and queried for expressions using WithAttributes and GetAttributes.
    /// </summary>
    public static class ExpressionAttributor
    {
        private static ConditionalWeakTable<Expression, Expression[]> s_attributes = new ConditionalWeakTable<Expression, Expression[]>();

        public static T WithAttributes<T>(this T e, params Expression[] attributes)
            where T : Expression
        {
            Expression[] oldAttributes;

            if (s_attributes.TryGetValue(e, out oldAttributes))
            {
                attributes = attributes.Concat(oldAttributes).ToArray();
                s_attributes.Remove(e);
            }

            if (attributes.Length > 0)
                s_attributes.Add(e, attributes);

            return e;
        }

        public static Expression[] GetAttributes(this Expression e)
        {
            Expression[] result;
            if (s_attributes.TryGetValue(e, out result))
            {
                return result;
            }
            else return new Expression[0];
        }

    }


    /// <summary>
    /// Contains intrinsic functions needed for the compilation
    /// </summary>
    internal static class Intrinsics
    {
        private static void ignore<T>(T arg) { }
        public static MethodInfo Ignore = ((Action<int>)ignore<int>).Method.GetGenericMethodDefinition();
    }


}
