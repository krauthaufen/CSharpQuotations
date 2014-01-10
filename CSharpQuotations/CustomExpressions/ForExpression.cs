using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.Quotations
{
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

}
