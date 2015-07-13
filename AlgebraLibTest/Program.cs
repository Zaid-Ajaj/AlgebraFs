using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MathParser;

namespace AlgebraLibTest
{
    class Program
    {
        static void Main(string[] args)
        {
            while(true)
            {
               Console.WriteLine(Parser.parseDiffSimpPrettyPrint(Console.ReadLine()));
            }
        }
    }
}
