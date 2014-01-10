using CSharp.Quotations;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace Test
{

    public class Sepp : IEnumerable<int>
    {
        private List<int> m_list = new List<int>() { 1, 2, 3, 4 };

        public IEnumerator<int> GetEnumerator()
        {
            return m_list.GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return m_list.GetEnumerator();
        }
    }

    public static class Ext
    {
        public static int Abs(this int value)
        {
            return value;
        }
    }

    [ReflectedDefinition]
    public class Test
    {
        private static List<int> list;
        public Test(params int[] a)
        {
            list = a.ToList();
        }

        private static int Abs2(int xi)
        {
            return 5*xi;
        }

        private int this[int c]
        {
            get { return 7 + c; }
        }

        public int Some(int a)
        {
            foreach (var xi in new List<int>() {1,2,3})
            {
                a += Abs2(xi) + this[xi];
            }

            try
            {
                a += 10;
            }
            catch (Exception)
            {
                switch (a)
                {
                    case 1: 
                        a = 10 * a;
                        break;
                    default: 
                        a = 2 * a;
                        break;
                }
            }

            if (a >= 1)
            {
                do
                {
                    a -= 10;
                }
                while (a > 0);
            }
            else if (a == 0)
            {
                for (int i = 10; i < 100; i++)
                {
                    while (a < i)
                    {
                        a += i;
                        if (a > 10) break;
                    }
                }
            }
            else a = 0;

            var x = 2;
            var y = 10;
            return a * x * y;
        }
    }

    [ReflectedDefinition]
    public class TestClass
    {
        public static int Test(int a, int b)
        {
            return a + b;
        }

        private static int Sepp
        {
            get { return 10; }
            set { }
        }

        public static int Some(int a, int b)
        {
            Func<int, int, int> f = (xi, yi) => yi * xi;

            IEnumerable<int> arr = new List<int>() { 1, 2, 3, 4 };
            var test = arr.ToArray();

            Sepp = 3;

            var x = a * b;
            if ((float)x < 10.0f)
                x = x + Test(Sepp, 123);
            else
                x = f(x, x - 10);

            while (x > 10)
                x = x / 4;

            for (int i = 1; i <= 10; i++)
            {
                x = x * i;
            }

            return x;
        }

    }

}

