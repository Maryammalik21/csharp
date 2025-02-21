using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        Console.WriteLine("Enter a variable name:");
        string input = Console.ReadLine();

        Regex variableRegex = new Regex(@"^[A-Za-z][A-Za-z0-9]{0,24}$");

        if (variableRegex.IsMatch(input))
            Console.WriteLine("Valid variable name.");
        else
            Console.WriteLine("Invalid variable name.");
    }
}
// input: varName
// ouput: Valid variable name
