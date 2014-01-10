using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.Quotations
{

    /// <summary>
    /// base class for visitors visiting custom expressions
    /// </summary>
    public class CustomExpressionVisitor : ExpressionVisitor
    {
        protected internal virtual Expression VisitFor(ForExpression node)
        {
            return base.Visit(node);
        }

        protected internal virtual Expression VisitWhile(WhileExpression node)
        {
            return base.Visit(node);
        }

        protected internal virtual Expression VisitDoWhile(DoWhileExpression node)
        {
            return base.Visit(node);
        }

        protected internal virtual Expression VisitForEach(ForEachExpression node)
        {
            return base.Visit(node);
        }

        public override Expression Visit(Expression node)
        {
            if (node is ForExpression) return VisitFor((ForExpression)node);
            else if (node is WhileExpression) return VisitWhile((WhileExpression)node);
            else if (node is DoWhileExpression) return VisitDoWhile((DoWhileExpression)node);
            else if (node is ForEachExpression) return VisitForEach((ForEachExpression)node);
            else return base.Visit(node);
        }
    }

    /// <summary>
    /// typed visitor for custom expressions
    /// </summary>
    public abstract class CustomExpressionVisitor<T>
    {
        public abstract T VisitFor(ForExpression node);
        public abstract T VisitWhile(WhileExpression node);
        public abstract T VisitDoWhile(DoWhileExpression node);
        public abstract T VisitForEach(ForEachExpression node);

        public abstract T VisitBinary(BinaryExpression node);
        public abstract T VisitBlock(BlockExpression node);
        public abstract T VisitConditional(ConditionalExpression node);
        public abstract T VisitConstant(ConstantExpression node);
        //public abstract T VisitDebugInfo(DebugInfoExpression node);
        public abstract T VisitDefault(DefaultExpression node);
        //public abstract T VisitDynamic(DynamicExpression node);
        //public abstract T VisitGoto(GotoExpression node);
        //public abstract T VisitIndex(IndexExpression node);
        public abstract T VisitInvocation(InvocationExpression node);
        public abstract T VisitLabel(LabelExpression node);
        public abstract T VisitLambda(LambdaExpression node);
        //public abstract T VisitListInit(ListInitExpression node);
        //public abstract T VisitLoop(LoopExpression node);
        public abstract T VisitMember(MemberExpression node);
        public abstract T VisitMemberInit(MemberInitExpression node);
        public abstract T VisitMethodCall(MethodCallExpression node);
        public abstract T VisitNew(NewExpression node);
        public abstract T VisitNewArray(NewArrayExpression node);
        public abstract T VisitParameter(ParameterExpression node);
        public abstract T VisitSwitch(SwitchExpression node);
        public abstract T VisitTry(TryExpression node);
        //public abstract T VisitTypeBinary(TypeBinaryExpression node);
        public abstract T VisitUnary(UnaryExpression node);

        public abstract T VisitBreak(GotoExpression node);
        public abstract T VisitContinue(GotoExpression node);
        public abstract T VisitReturn(GotoExpression node);
        public abstract T VisitGoto(GotoExpression node);

        internal T Visit(Expression e)
        {
            var b = new BuilderVisitor(this);
            b.Visit(e);
            return b.Result;
        }

        private class BuilderVisitor : CustomExpressionVisitor
        {
            private CustomExpressionVisitor<T> m_parent;
            public T Result = default(T);

            public BuilderVisitor(CustomExpressionVisitor<T> parent)
            {
                m_parent = parent;
            }

            public T Run(Expression e)
            {
                base.Visit(e);
                return Result;
            }

            protected override Expression VisitGoto(GotoExpression node)
            {
                if (node.Kind == GotoExpressionKind.Break)
                    Result = m_parent.VisitBreak(node);
                else if (node.Kind == GotoExpressionKind.Continue)
                    Result = m_parent.VisitContinue(node);
                else if (node.Kind == GotoExpressionKind.Return)
                    Result = m_parent.VisitReturn(node);
                else
                    Result = m_parent.VisitGoto(node);

                return base.VisitGoto(node);
            }

            protected internal override Expression VisitFor(ForExpression node)
            {

                Result = m_parent.VisitFor(node);
                return base.VisitFor(node);
            }

            protected internal override Expression VisitWhile(WhileExpression node)
            {
                Result = m_parent.VisitWhile(node);
                return base.VisitWhile(node);
            }

            protected internal override Expression VisitDoWhile(DoWhileExpression node)
            {
                Result = m_parent.VisitDoWhile(node);
                return base.VisitDoWhile(node);
            }

            protected internal override Expression VisitForEach(ForEachExpression node)
            {
                Result = m_parent.VisitForEach(node);
                return base.VisitForEach(node);
            }

            protected override Expression VisitBinary(BinaryExpression node)
            {
                Result = m_parent.VisitBinary(node);
                return base.VisitBinary(node);
            }

            protected override Expression VisitBlock(BlockExpression node)
            {
                Result = m_parent.VisitBlock(node);
                return base.VisitBlock(node);
            }

            protected override Expression VisitConditional(ConditionalExpression node)
            {
                Result = m_parent.VisitConditional(node);
                return base.VisitConditional(node);
            }

            protected override Expression VisitConstant(ConstantExpression node)
            {
                Result = m_parent.VisitConstant(node);
                return base.VisitConstant(node);
            }

            protected override Expression VisitDefault(DefaultExpression node)
            {
                Result = m_parent.VisitDefault(node);
                return base.VisitDefault(node);
            }



            protected override Expression VisitInvocation(InvocationExpression node)
            {
                Result = m_parent.VisitInvocation(node);
                return base.VisitInvocation(node);
            }

            protected override Expression VisitLabel(LabelExpression node)
            {
                Result = m_parent.VisitLabel(node);
                return base.VisitLabel(node);
            }

            protected override Expression VisitLambda<TL>(Expression<TL> node)
            {
                Result = m_parent.VisitLambda(node);
                return base.VisitLambda(node);
            }

            protected override Expression VisitMember(MemberExpression node)
            {
                Result = m_parent.VisitMember(node);
                return base.VisitMember(node);
            }

            protected override Expression VisitMethodCall(MethodCallExpression node)
            {
                Result = m_parent.VisitMethodCall(node);
                return base.VisitMethodCall(node);
            }

            protected override Expression VisitNew(NewExpression node)
            {
                Result = m_parent.VisitNew(node);
                return base.VisitNew(node);
            }

            protected override Expression VisitNewArray(NewArrayExpression node)
            {
                Result = m_parent.VisitNewArray(node);
                return base.VisitNewArray(node);
            }

            protected override Expression VisitParameter(ParameterExpression node)
            {
                Result = m_parent.VisitParameter(node);
                return base.VisitParameter(node);
            }

            protected override Expression VisitSwitch(SwitchExpression node)
            {
                Result = m_parent.VisitSwitch(node);
                return base.VisitSwitch(node);
            }

            protected override Expression VisitTry(TryExpression node)
            {
                Result = m_parent.VisitTry(node);
                return base.VisitTry(node);
            }

            protected override Expression VisitUnary(UnaryExpression node)
            {
                Result = m_parent.VisitUnary(node);
                return base.VisitUnary(node);
            }

        }
    }

    public static class ExpressionExtensions
    {
        public static T Accept<T>(this Expression e, CustomExpressionVisitor<T> visitor)
        {
            return visitor.Visit(e);
        }
    }
}
