using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CSharp.Quotations;
using System.Linq.Expressions;
using System.Diagnostics;
using System.Collections.Generic;

namespace ExpressionTests
{


    [TestClass]
    [ReflectedDefinition]
    public class ExpressionCompilerTest
    {
        private static void TestCompiledExpression(Func<int, int> f, int min, int max)
        {
            Func<int, int> compiled = null;
            Expression e;
            if (Expr.TryGetReflectedDefinition(f.Method, out e))
            {
                compiled = ((Expression<Func<int, int>>)e).Compile();
            }
            else
            {
                Assert.Fail("no reflected definition found for {0}", f.Method);
            }

            Debug.WriteLine("starting test for {0} with range [{1}, {2}]", f.Method, min, max);
            for (int a = min; a <= max; a++)
            {
                var should = f(a);
                var real = compiled(a);

                Assert.AreEqual(should, real, "invalid value for {0}", a);

            }

            Debug.WriteLine("success");
        }

        #region If

        private static int IfTestMethod(int arg)
        {
            var sum = 2 * arg;
            if (arg < 10)
            {
                sum = 2 * sum + 5;
            }

            return sum;
        }

        [TestMethod]
        public void IfTest()
        {
            TestCompiledExpression(IfTestMethod, -100, 100);
        }

        #endregion

        #region IfElse

        private static int IfElseTestMethod(int arg)
        {
            var sum = 2 * arg;
            if (arg < 10)
            {
                sum = 2 * sum + 5;
            }
            else
                sum = 0;

            return sum;
        }

        [TestMethod]
        public void IfElseTest()
        {
            TestCompiledExpression(IfElseTestMethod, -100, 100);
        }

        #endregion

        #region Conditional

        private static int ConditionalTestMethod(int arg)
        {
            return arg % 2 == 0 ? 10 : arg;
        }

        [TestMethod]
        public void ConditionalTest()
        {
            TestCompiledExpression(ConditionalTestMethod, -100, 100);
        }

        #endregion

        #region Coerce

        private static int CoerceTest(int arg)
        {
            var x = 3.0f * (float)arg;
            return (int)x;
        }

        [TestMethod]
        public void CoerceTest()
        {
            TestCompiledExpression(CoerceTest, -100, 100);
        }

        #endregion

        #region MethodCall

        private static int MethodCallTestMethod(int arg)
        {
            var a = new Tuple<int, int>(arg, -arg);
            return a.GetType().GetHashCode();
        }

        [TestMethod]
        public void MethodCallTest()
        {
            TestCompiledExpression(MethodCallTestMethod, -100, 100);
        }

        #endregion

        #region For

        private static int TestForMethod(int arg)
        {
            var r = 0;
            for (int i = 0; i < arg; i++)
            {
                r = r + i;
            }

            return r;
        }

        [TestMethod]
        public void TestFor()
        {
            TestCompiledExpression(TestForMethod, -10, 100);
        }

        #endregion

        #region While

        private static int TestWhileMethod(int arg)
        {
            var s = 0;
            var i = 1;
            while (s < 1000)
            {
                s = s + (System.Math.Abs(arg) + 1) * i;
                i++;
            }

            return s;
        }

        [TestMethod]
        public void TestWhile()
        {
            TestCompiledExpression(TestWhileMethod, -100, 100);
        }

        #endregion

        #region DoWhile

        private static int TestDoWhileMethod(int arg)
        {
            var s = 0;
            var i = 1;
            do
            {
                s = s + (System.Math.Abs(arg) + 1) * i;
                i++;
            }
            while (s < 1000);

            return s;
        }

        [TestMethod]
        public void TestDoWhile()
        {
            TestCompiledExpression(TestDoWhileMethod, -100, 100);
        }

        #endregion

        #region ForEach

        private static int TestForEachMethod(int arg)
        {
            var e = new[]{ arg, arg + 2, arg + 3, arg + 4 };
            var sum = 0;
            foreach (var ei in e)
            {
                sum = sum + ei;
            }

            return sum;
        }

        [TestMethod]
        public void TestForEach()
        {
            TestCompiledExpression(TestForEachMethod, -100, 100);
        }

        #endregion



    }
}
