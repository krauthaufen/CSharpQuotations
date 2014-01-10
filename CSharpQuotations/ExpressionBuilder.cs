using Roslyn.Compilers.CSharp;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace CSharpQuotations
{

    public enum CustomExpressionType
    {
        DoWhileExpression,
        ForEachExpression,
        ForExpression,
        UsingExpression,
        WhileExpression,
    }

    public abstract class CustomExpression : Expression
    {

        public abstract CustomExpressionType CustomNodeType { get; }

        public override ExpressionType NodeType
        {
            get { return ExpressionType.Extension; }
        }

        public override bool CanReduce
        {
            get { return true; }
        }

    }

    public class ForExpression : CustomExpression
    {

        readonly Tuple<IEnumerable<ParameterExpression>, IEnumerable<Expression>> initializer;
        readonly Expression test;
        readonly Expression[] step;

        readonly Expression body;

        readonly LabelTarget break_target;
        readonly LabelTarget continue_target;

        public Tuple<IEnumerable<ParameterExpression>, IEnumerable<Expression>> Initializers
        {
            get { return initializer; }
        }

        public Expression Test
        {
            get { return test; }
        }

        public Expression[] Step
        {
            get { return step; }
        }

        public Expression Body
        {
            get { return body; }
        }

        public LabelTarget BreakTarget
        {
            get { return break_target; }
        }

        public LabelTarget ContinueTarget
        {
            get { return continue_target; }
        }

        public override Type Type
        {
            get
            {
                if (break_target != null)
                    return break_target.Type;

                return typeof(void);
            }
        }

        public override CustomExpressionType CustomNodeType
        {
            get { return CustomExpressionType.ForExpression; }
        }

        internal ForExpression(Tuple<IEnumerable<ParameterExpression>, IEnumerable<Expression>> initializers, Expression test, Expression[] step, Expression body, LabelTarget breakTarget, LabelTarget continueTarget)
        {
            this.initializer = initializers;
            this.test = test;
            this.step = step;
            this.body = body;
            this.break_target = breakTarget;
            this.continue_target = continueTarget;
        }

        public ForExpression Update(Tuple<IEnumerable<ParameterExpression>, IEnumerable<Expression>> initializers, Expression test, Expression[] step, Expression body, LabelTarget breakTarget, LabelTarget continueTarget)
        {
            if (this.initializer == initializers && this.test == test && this.step == step && this.body == body && this.break_target == breakTarget && this.continue_target == continueTarget)
                return this;

            return new ForExpression(initializers, test, step, body, breakTarget, continueTarget);
        }

        public override Expression Reduce()
        {
            var inner_loop_break = Expression.Label("inner_loop_break");
            var inner_loop_continue = Expression.Label("inner_loop_continue");

            var @continue = continue_target ?? Expression.Label("continue");
            var @break = break_target ?? Expression.Label("break");

            return Expression.Block(initializer.Item1,
                Expression.Block(initializer.Item2),
                Expression.Loop(
                    Expression.Block(
                        Expression.IfThen(
                            Expression.IsFalse(test),
                            Expression.Break(inner_loop_break)),
                        body,
                        Expression.Label(@continue),
                        Expression.Block(step)),
                    inner_loop_break,
                    inner_loop_continue),
                Expression.Label(@break));
        }

        protected override Expression VisitChildren(ExpressionVisitor visitor)
        {
            return Update(
                Tuple.Create(initializer.Item1, initializer.Item2.Select(ii => visitor.Visit(ii))),
                visitor.Visit(test),
                step.Select(s => visitor.Visit(s)).ToArray(),
                visitor.Visit(body),
                continue_target,
                break_target);
        }
    }

    public class ForEachExpression : CustomExpression
    {

        readonly ParameterExpression variable;
        readonly Expression enumerable;
        readonly Expression body;

        readonly LabelTarget break_target;
        readonly LabelTarget continue_target;

        public new ParameterExpression Variable
        {
            get { return variable; }
        }

        public Expression Enumerable
        {
            get { return enumerable; }
        }

        public Expression Body
        {
            get { return body; }
        }

        public LabelTarget BreakTarget
        {
            get { return break_target; }
        }

        public LabelTarget ContinueTarget
        {
            get { return continue_target; }
        }

        public override Type Type
        {
            get
            {
                if (break_target != null)
                    return break_target.Type;

                return typeof(void);
            }
        }

        public override CustomExpressionType CustomNodeType
        {
            get { return CustomExpressionType.ForEachExpression; }
        }

        internal ForEachExpression(ParameterExpression variable, Expression enumerable, Expression body, LabelTarget break_target, LabelTarget continue_target)
        {
            this.variable = variable;
            this.enumerable = enumerable;
            this.body = body;
            this.break_target = break_target;
            this.continue_target = continue_target;
        }

        public ForEachExpression Update(ParameterExpression variable, Expression enumerable, Expression body, LabelTarget breakTarget, LabelTarget continueTarget)
        {
            if (this.variable == variable && this.enumerable == enumerable && this.body == body && break_target == breakTarget && continue_target == continueTarget)
                return this;

            return new ForEachExpression(variable, enumerable, body, continueTarget, breakTarget);
        }

        public override Expression Reduce()
        {
            // Avoid allocating an unnecessary enumerator for arrays.
            if (enumerable.Type.IsArray)
                return ReduceForArray();

            return ReduceForEnumerable();
        }

        private Expression ReduceForArray()
        {
            var inner_loop_break = Expression.Label("inner_loop_break");
            var inner_loop_continue = Expression.Label("inner_loop_continue");

            var @continue = continue_target ?? Expression.Label("continue");
            var @break = break_target ?? Expression.Label("break");

            var index = Expression.Variable(typeof(int), "i");

            return Expression.Block(
                new[] { index, variable },
                Expression.Assign(index, Expression.Constant(0)),
                Expression.Loop(
                    Expression.Block(
                        Expression.IfThen(
                            Expression.IsFalse(
                                Expression.LessThan(
                                    index,
                                    Expression.ArrayLength(enumerable))),
                            Expression.Break(inner_loop_break)),
                        Expression.Assign(
                            variable,
                            Expression.Convert(
                                Expression.ArrayIndex(
                                enumerable,
                                index), variable.Type)),
                        body,
                        Expression.Label(@continue),
                        Expression.PreIncrementAssign(index)),
                    inner_loop_break,
                    inner_loop_continue),
                Expression.Label(@break));
        }

        private Expression ReduceForEnumerable()
        {
            MethodInfo get_enumerator;
            MethodInfo move_next;
            MethodInfo get_current;

            ResolveEnumerationMembers(out get_enumerator, out move_next, out get_current);

            var enumerator_type = get_enumerator.ReturnType;

            var enumerator = Expression.Variable(enumerator_type);

            var inner_loop_continue = Expression.Label("inner_loop_continue");
            var inner_loop_break = Expression.Label("inner_loop_break");
            var @continue = continue_target ?? Expression.Label("continue");
            var @break = break_target ?? Expression.Label("break");

            Expression variable_initializer;

            if (variable.Type.IsAssignableFrom(get_current.ReturnType))
                variable_initializer = Expression.Property(enumerator, get_current);
            else
                variable_initializer = Expression.Convert(Expression.Property(enumerator, get_current), variable.Type);

            Expression loop = Expression.Block(
                new[] { variable },
                Expression.Goto(@continue),
                Expression.Loop(
                    Expression.Block(
                        Expression.Assign(variable, variable_initializer),
                        body,
                        Expression.Label(@continue),
                        Expression.Condition(
                            Expression.Call(enumerator, move_next),
                            Expression.Goto(inner_loop_continue),
                            Expression.Goto(inner_loop_break))),
                    inner_loop_break,
                    inner_loop_continue),
                Expression.Label(@break));

            var dispose = CreateDisposeOperation(enumerator_type, enumerator);

            return Expression.Block(
                new[] { enumerator },
                Expression.Assign(enumerator, Expression.Call(enumerable, get_enumerator)),
                dispose != null
                    ? Expression.TryFinally(loop, dispose)
                    : loop);
        }

        private void ResolveEnumerationMembers(
            out MethodInfo get_enumerator,
            out MethodInfo move_next,
            out MethodInfo get_current)
        {
            Type item_type;
            Type enumerable_type;
            Type enumerator_type;

            if (TryGetGenericEnumerableArgument(out item_type))
            {
                enumerable_type = typeof(IEnumerable<>).MakeGenericType(item_type);
                enumerator_type = typeof(IEnumerator<>).MakeGenericType(item_type);
            }
            else
            {
                enumerable_type = typeof(IEnumerable);
                enumerator_type = typeof(IEnumerator);
            }

            move_next = typeof(IEnumerator).GetMethod("MoveNext");
            get_current = enumerator_type.GetProperty("Current").GetGetMethod();
            get_enumerator = enumerable.Type.GetMethod("GetEnumerator", BindingFlags.Public | BindingFlags.Instance);

            //
            // We want to avoid unnecessarily boxing an enumerator if it's a value type.  Look
            // for a GetEnumerator() method that conforms to the rules of the C# 'foreach'
            // pattern.  If we don't find one, fall back to IEnumerable[<T>].GetEnumerator().
            //

            if (get_enumerator == null || !enumerator_type.IsAssignableFrom(get_enumerator.ReturnType))
            {
                get_enumerator = enumerable_type.GetMethod("GetEnumerator");
            }
        }

        private static Expression CreateDisposeOperation(Type enumerator_type, ParameterExpression enumerator)
        {
            var dispose = typeof(IDisposable).GetMethod("Dispose");

            if (typeof(IDisposable).IsAssignableFrom(enumerator_type))
            {
                //
                // We know the enumerator implements IDisposable, so skip the type check.
                //
                return Expression.Call(enumerator, dispose);
            }

            if (enumerator_type.IsValueType)
            {
                //
                // The enumerator is a value type and doesn't implement IDisposable; we needn't
                // bother with a check at all.
                //
                return null;
            }

            //
            // We don't know whether the enumerator implements IDisposable or not.  Emit a
            // runtime check.
            //

            var disposable = Expression.Variable(typeof(IDisposable));

            return Expression.Block(
                new[] { disposable },
                Expression.Assign(disposable, Expression.TypeAs(enumerator, typeof(IDisposable))),
                Expression.IfThen(
                    Expression.ReferenceNotEqual(disposable, Expression.Constant(null)),
                    Expression.Call(
                        disposable,
                        "Dispose",
                        Type.EmptyTypes)));
        }

        private bool TryGetGenericEnumerableArgument(out Type argument)
        {
            argument = null;

            foreach (var iface in enumerable.Type.GetInterfaces())
            {
                if (!iface.IsGenericType)
                    continue;

                var definition = iface.GetGenericTypeDefinition();
                if (definition != typeof(IEnumerable<>))
                    continue;

                argument = iface.GetGenericArguments()[0];
                if (variable.Type.IsAssignableFrom(argument))
                    return true;
            }

            return false;
        }

        protected override Expression VisitChildren(ExpressionVisitor visitor)
        {
            return Update(
                (ParameterExpression)visitor.Visit(variable),
                visitor.Visit(enumerable),
                visitor.Visit(body),
                break_target,
                continue_target);
        }
    }

    public class WhileExpression : CustomExpression
    {

        readonly Expression test;
        readonly Expression body;

        readonly LabelTarget break_target;
        readonly LabelTarget continue_target;

        public Expression Test
        {
            get { return test; }
        }

        public Expression Body
        {
            get { return body; }
        }

        public LabelTarget BreakTarget
        {
            get { return break_target; }
        }

        public LabelTarget ContinueTarget
        {
            get { return continue_target; }
        }

        public override Type Type
        {
            get
            {
                if (break_target != null)
                    return break_target.Type;

                return typeof(void);
            }
        }

        public override CustomExpressionType CustomNodeType
        {
            get { return CustomExpressionType.WhileExpression; }
        }

        internal WhileExpression(Expression test, Expression body, LabelTarget breakTarget, LabelTarget continueTarget)
        {
            this.test = test;
            this.body = body;
            this.break_target = breakTarget;
            this.continue_target = continueTarget;
        }

        public WhileExpression Update(Expression test, Expression body, LabelTarget breakTarget, LabelTarget continueTarget)
        {
            if (this.test == test && this.body == body && this.break_target == breakTarget && this.continue_target == continueTarget)
                return this;

            return new WhileExpression(test, body, breakTarget, continueTarget);
        }

        public override Expression Reduce()
        {
            var inner_loop_break = Expression.Label("inner_loop_break");
            var inner_loop_continue = Expression.Label("inner_loop_continue");

            var @continue = continue_target ?? Expression.Label("continue");
            var @break = break_target ?? Expression.Label("break");

            return Expression.Block(
                Expression.Loop(
                    Expression.Block(
                        Expression.Label(@continue),
                        Expression.Condition(
                            test,
                            Expression.Block(
                                body,
                                Expression.Goto(inner_loop_continue)),
                            Expression.Goto(inner_loop_break))),
                    inner_loop_break,
                    inner_loop_continue),
                Expression.Label(@break));
        }

        protected override Expression VisitChildren(ExpressionVisitor visitor)
        {
            return Update(
                visitor.Visit(test),
                visitor.Visit(body),
                continue_target,
                break_target);
        }
    }

    public class DoWhileExpression : CustomExpression
    {

        readonly Expression test;
        readonly Expression body;

        readonly LabelTarget break_target;
        readonly LabelTarget continue_target;

        public Expression Test
        {
            get { return test; }
        }

        public Expression Body
        {
            get { return body; }
        }

        public LabelTarget BreakTarget
        {
            get { return break_target; }
        }

        public LabelTarget ContinueTarget
        {
            get { return continue_target; }
        }

        public override Type Type
        {
            get
            {
                if (break_target != null)
                    return break_target.Type;

                return typeof(void);
            }
        }

        public override CustomExpressionType CustomNodeType
        {
            get { return CustomExpressionType.DoWhileExpression; }
        }

        internal DoWhileExpression(Expression test, Expression body, LabelTarget breakTarget, LabelTarget continueTarget)
        {
            this.test = test;
            this.body = body;
            this.break_target = breakTarget;
            this.continue_target = continueTarget;
        }

        public DoWhileExpression Update(Expression test, Expression body, LabelTarget breakTarget, LabelTarget continueTarget)
        {
            if (this.test == test && this.body == body && this.break_target == breakTarget && this.continue_target == continueTarget)
                return this;

            return new DoWhileExpression(test, body, breakTarget, continueTarget);
        }

        public override Expression Reduce()
        {
            var inner_loop_break = Expression.Label("inner_loop_break");
            var inner_loop_continue = Expression.Label("inner_loop_continue");

            var @continue = continue_target ?? Expression.Label("continue");
            var @break = break_target ?? Expression.Label("break");

            return Expression.Block(
                Expression.Loop(
                    Expression.Block(
                        Expression.Label(@continue),
                        body,
                        Expression.Condition(
                            test,
                            Expression.Goto(inner_loop_continue),
                            Expression.Goto(inner_loop_break))),
                    inner_loop_break,
                    inner_loop_continue),
                Expression.Label(@break));
        }

        protected override Expression VisitChildren(ExpressionVisitor visitor)
        {
            return Update(
                visitor.Visit(test),
                visitor.Visit(body),
                continue_target,
                break_target);
        }

    }


    public class CompileVisitor : SyntaxVisitor<Func<IEnumerable<Expression>, Expression>>
    {
        private MethodBase m_method;
        private Type m_type;
        private ParameterExpression m_this;
        private CompiledNamespaceAttribute[] m_namespaces;

        private Dictionary<string, ParameterExpression> m_parameters = new Dictionary<string, ParameterExpression>();
        private LabelTarget m_currentBreakLabel;
        private LabelTarget m_currentContinueLabel;

        #region Compile

        private Func<IEnumerable<Expression>, Expression> Compile(SyntaxNode node)
        {
            return node.Accept(this);
        }

        private Func<IEnumerable<Expression>, Expression> Compile(BlockSyntax node, bool ignore = false)
        {
            return Bind(node.Accept(this), r =>
                {
                    return Return(ignore ? Expression.Block(r, Expression.Empty()) : r);
                });
        }

        private Func<IEnumerable<Expression>, CatchBlock> Compile(CatchClauseSyntax node)
        {
            var exceptionType = Resolve(node.Declaration.Type);
            return e =>
                    {
                        var body = Compile(node.Block, true)(Enumerable.Empty<Expression>());
                        return Expression.MakeCatchBlock(exceptionType, null, body, null);
                    };
        }

        private Func<IEnumerable<Expression>, Expression> Compile(FinallyClauseSyntax node, bool ignore)
        {
            return Compile(node.Block, ignore);
        }

        private Func<IEnumerable<Expression>, SwitchCase> Compile(SwitchSectionSyntax node)
        {
            return Bind(node.Statements.Select(Compile).ToArray(), b =>
                    Bind<Expression, SwitchCase>(node.Labels.Select(l => Compile(l.Value)).ToArray(), l =>
                    _ => Expression.SwitchCase(Expression.Block(b), l)));
        }

        #endregion

        #region Constructors

        public CompileVisitor(MethodBase method)
        {
            m_method = method;
            m_type = m_method.DeclaringType;
            m_namespaces = Expr.GetNamespaces(m_method).ToArray();
            m_this = null;
        }

        #endregion

        #region Types

#warning TODO: better resolve for types

        private Type Resolve(PredefinedTypeSyntax pd)
        {
            switch (pd.Keyword.ValueText)
            {
                case "int": return typeof(int);
                default:
                    throw new NotImplementedException();
            }
        }

        private Type Resolve(IdentifierNameSyntax type)
        {
            var name = type.Identifier.ToString();
            var mine = Type.GetType(m_type.Namespace + "." + name);
            if (mine != null) return mine;

            foreach (var n in m_namespaces)
            {
                if (n.Alias == null)
                {
                    var probe = Type.GetType(n.Namespace + "." + name);
                    if (probe != null) return probe;
                }
            }

            throw new NotImplementedException();
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

            if (pd != null) return Resolve(pd);
            else if (type.IsVar) return null;
            else if (nameType != null) return Resolve(nameType);
            else if (qualType != null) return Resolve(qualType);
            else throw new NotImplementedException();
        }

        #endregion

        #region Values

        private static object ParseNumeric(string numeric)
        {
            var n = numeric.ToLower();

            if (n.Contains("."))
            {
                if (n.EndsWith("f")) return float.Parse(n);
                else return double.Parse(n);
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

        private ParameterExpression CompileParameter(ParameterSyntax pi)
        {
            var name = pi.Identifier.ToString();
            ParameterExpression pe;

            if (!m_parameters.TryGetValue(name, out pe))
            {
                pe = Expression.Parameter(Resolve(pi.Type), name);
                m_parameters[name] = pe;
            }

            return pe;
        }

        private ParameterExpression CompileParameter(Type type, string name)
        {
            ParameterExpression pe;

            if (!m_parameters.TryGetValue(name, out pe))
            {
                pe = Expression.Parameter(type, name);
                m_parameters[name] = pe;
            }

            return pe;
        }

        private Tuple<IEnumerable<ParameterExpression>, IEnumerable<Expression>> CompileVariable(VariableDeclarationSyntax d)
        {
            
            var type = Resolve(d.Type);
            var tuples = d.Variables.Select(vd =>
                {
                    var value = Compile(vd.Initializer)(Enumerable.Empty<Expression>());
                    var p = CompileParameter(type ?? value.Type, vd.Identifier.ToString());
                    
                    return Tuple.Create(p, Expression.Assign(p, value));
                }).ToArray();


            return Tuple.Create(tuples.Select(t => t.Item1), tuples.Select(t => (Expression)t.Item2));
        }

        #endregion

        #region Monad

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
                default:
                    throw new NotImplementedException();
            }
        }

        #endregion

        #region Visitors

        private Expression This
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
            return Bind(body, b => Return(Expression.Lambda(b, parameters)));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitParameter(ParameterSyntax node)
        {
            return Return(CompileParameter(node));
        }

        public override Func<IEnumerable<Expression>, Expression> VisitIdentifierName(IdentifierNameSyntax node)
        {
            var flags = BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance;
 
            var name = node.Identifier.ToString();

            ParameterExpression e;
            if (m_parameters.TryGetValue(name, out e)) return Return(e);
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
            return Compile(node.Expression);
        }

        public override Func<IEnumerable<Expression>, Expression> VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            if (node.Kind == SyntaxKind.NumericLiteralExpression) return Return(Expression.Constant(ParseNumeric(node.Token.ValueText)));
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
            return Compile(node.Expression);
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
                           var enumeratorTypes = new[]{e.Type.GetMethod("GetEnumerator").ReturnType};

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


        #endregion

        #region Compile

        public static Expression Compile(MethodBase method, SyntaxTree tree)
        {
            var m = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().FirstOrDefault();
            var v = new CompileVisitor(method);

            var expression = v.Visit(m)(Enumerable.Empty<Expression>());

            if (!method.IsStatic) return Expression.Lambda(expression, (ParameterExpression)v.This);
            else return expression;
        }

        #endregion
    }

}
