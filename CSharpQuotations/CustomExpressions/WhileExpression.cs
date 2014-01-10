using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.Quotations
{
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

}
