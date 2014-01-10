using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.Quotations
{
    /// <summary>
    /// base class for all custom expressions giving 'Extension' as its node type
    /// all custom nodes must provide a Reduce method in order to be compilable using the Compile-method
    /// </summary>
    public abstract class CustomExpression : Expression
    {

        /// <summary>
        /// the node type is extensions in order to tell all core calls how to treat them
        /// </summary>
        public override ExpressionType NodeType
        {
            get { return ExpressionType.Extension; }
        }

        /// <summary>
        /// all custom expressions must be reducible
        /// </summary>
        public override bool CanReduce
        {
            get { return true; }
        }

        /// <summary>
        /// must provide a valid reduced form of the expression
        /// </summary>
        public abstract Expression Reduce();
    }



}
