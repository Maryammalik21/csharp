using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        Console.WriteLine("Enter a relational operator:");
        string input = Console.ReadLine();

        Regex relationalOperatorsRegex = new Regex(@"^(==|!=|<=|>=|<|>)$");

        if (relationalOperatorsRegex.IsMatch(input))
            Console.WriteLine("Valid relational operator.");
        else
            Console.WriteLine("Invalid relational operator.");
    }
}
// input: ==
// output: Valid relational operator.
