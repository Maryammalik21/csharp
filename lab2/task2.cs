using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        Console.WriteLine("Enter a logical operator:");
        string input = Console.ReadLine();

        Regex logicalOperatorsRegex = new Regex(@"^(\&\&|\|\||!)$");

        if (logicalOperatorsRegex.IsMatch(input))
            Console.WriteLine("Valid logical operator.");
        else
            Console.WriteLine("Invalid logical operator.");
    }
}
// input: &&
// output: Valid logical operator.
