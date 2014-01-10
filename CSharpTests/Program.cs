using CSharp.Quotations;
using Roslyn.Compilers.CSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace Test
{
    class Program
    {

        public class MyVisitor : CustomExpressionVisitor<int>
        {

            public override int VisitFor(ForExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitWhile(WhileExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitDoWhile(DoWhileExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitForEach(ForEachExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitBinary(BinaryExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitBlock(BlockExpression node)
            {
                return node.Expressions.Sum(e => e.Accept(this));
            }

            public override int VisitConditional(ConditionalExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitConstant(ConstantExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitDefault(DefaultExpression node)
            {
                throw new NotImplementedException();
            }


            public override int VisitInvocation(InvocationExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitLabel(LabelExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitLambda(LambdaExpression node)
            {
                return node.Body.Accept(this);
            }

            public override int VisitMember(MemberExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitMemberInit(MemberInitExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitMethodCall(MethodCallExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitNew(NewExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitNewArray(NewArrayExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitParameter(ParameterExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitSwitch(SwitchExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitTry(TryExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitUnary(UnaryExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitBreak(GotoExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitContinue(GotoExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitReturn(GotoExpression node)
            {
                throw new NotImplementedException();
            }

            public override int VisitGoto(GotoExpression node)
            {
                throw new NotImplementedException();
            }
        }


        static void Main(string[] args)
        {
            var t = new Test(1,2,3,4,5);
            Func<int, int> compiled = null;
            Func<int, int> reference = i => t.Some(i);

            Expression ex;
            var mi = typeof(Test).GetMethod("Some");
            if (Expr.TryGetReflectedDefinition(mi, out ex))
            {

                compiled = ((Expression<Func<Test, Func<int, int>>>)ex).Compile()(t);
            }


            var test = true;
            var errs = new List<int>();
            for (int i = -500; i <= 500; i++)
            {
                if (compiled(i) != reference(i))
                {
                    test = false;
                    errs.Add(i);
                }
            }

            Console.WriteLine(test ? "Success" : "Error");


        }
    }
}
