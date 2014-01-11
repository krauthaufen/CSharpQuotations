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
        private static bool RunTest(Func<int, int> f, int min, int max)
        {
            Func<int, int> compiled = null;
            Expression e;
            if (Expr.TryGetReflectedDefinition(f.Method, out e))
            {
                compiled = ((Expression<Func<int, int>>)e).Compile();
            }
            else 
            {
                Console.WriteLine("no reflected definition found for {0}", f.Method);
                return false;
            }


            for (int a = min; a <= max; a++)
            {
                var should = f(a);
                var real = compiled(a);

                if (real != should)
                {
                    Console.WriteLine("error");
                    return false;
                }

            }

            return true;

        }



        static void Main(string[] args)
        {
            RunTest(Test.Some, -400, 400);

        }
    }
}
