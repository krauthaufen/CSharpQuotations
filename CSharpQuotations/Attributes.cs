using Roslyn.Compilers.CSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.Quotations
{
    public class ReflectedDefinitionAttribute : Attribute
    {
    }

    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Property, AllowMultiple = false)]
    public sealed class CompiledReflectedDefinitionAttribute : Attribute
    {
        public readonly string Code;

        public CompiledReflectedDefinitionAttribute(string code)
        {
            Code = code;
        }
    }

    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Property, AllowMultiple = true)]
    public sealed class CompiledNamespaceAttribute : Attribute
    {
        public readonly string Alias;
        public readonly string Namespace;

        public CompiledNamespaceAttribute(string alias, string name)
        {
            Alias = alias;
            Namespace = name;
        }

        public CompiledNamespaceAttribute(string name)
        {
            Alias = null;
            Namespace = name;
        }

    }

    public static class Expr
    {
        internal static bool TryGetReflectedCode(MethodBase b, out string code)
        {
            var att = b.GetCustomAttribute<CompiledReflectedDefinitionAttribute>(false);

            if (att != null)
            {
                var c = att.Code;

                c = c.Replace("\\r", "\r").Replace("\\n", "\n");
                code = c;

                return true;
            }
            else
            {
                code = null;
                return false;
            }
        
        }

        internal static bool TryGetReflectedTree(MethodBase m, out SyntaxTree tree)
        { 
            string code;
            if (TryGetReflectedCode(m, out code))
            {
                tree = SyntaxTree.ParseText(code);
                return true;
            }
            else
            {
                tree = null;
                return false;
            }
        }

        public static bool TryGetReflectedDefinition(MethodBase m, out Expression e)
        { 
            SyntaxTree tree;
            if (TryGetReflectedTree(m, out tree))
            {
                e = CompileVisitor.Compile(m, tree);
                return true;
            }
            else
            {
                e = null;
                return false;
            }
        }

        public static IEnumerable<CompiledNamespaceAttribute> GetNamespaces(MethodBase m)
        {
            return m.GetCustomAttributes<CompiledNamespaceAttribute>(false);
        }

    }


}
