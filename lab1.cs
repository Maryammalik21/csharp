using System;
using System.Text.RegularExpressions;

public class PasswordValidator
{
    public static void Main()
    {
        string pattern = 
            @"^(?=.*?sp)" +
            @"(?=.*?[A-Z])" +
            @"(?=(?:.*?[!@#$%^&*()_+{}\[\]:;<>,\.?~`]){2,})" +
            @"(?=(?:.*?[maryamtanveermalik]){4,}).{0,12}$";

        // Print the password instructions:
        Console.WriteLine("Password Requirements:");
        Console.WriteLine("1. Must contain the substring \"sp\" (from registration number sp22-bcs-021).");
        Console.WriteLine("2. Must contain at least one uppercase letter.");
        Console.WriteLine("3. Must contain at least 2 special characters (allowed: !@#$%^&*()_+{}[]:;<>,.?~`). They can appear anywhere.");
        Console.WriteLine("4. Must contain at least 4 lowercase letters that appear in the name \"maryam tanveer malik\".");
        Console.WriteLine("5. Maximum length is 12 characters.");
        Console.WriteLine();
        
        // List of test passwords (with expected outcome as comments):
        string[] testPasswords = new string[]
        {
            "spA!ma@ry",      // Expected: Valid
            "spB#ta$lk",      // Expected: Valid
            "spC%na&vi",      // Expected: Valid
            "spD@ma!ik",      // Expected: Valid
            "spE$tan#er",     // Expected: Valid
            "sA!ma@ry",       // Expected: Invalid (missing "sp")
            "spa!ma@ry",      // Expected: Invalid (missing uppercase letter)
            "spAma@ry",       // Expected: Invalid (only one special character)
            "spA!ma@ryextra", // Expected: Invalid (length > 12)
            "spA!m0@ry"       // Expected: Invalid (digit '0' is not in allowed lowercase letters)
        };

        Console.WriteLine("=== Test Results ===");
        foreach (string pass in testPasswords)
        {
            bool isValid = Regex.IsMatch(pass, pattern);
            Console.WriteLine($"{pass} : {(isValid ? "Valid" : "Invalid")}");
        }
        
        // Optionally, allow the user to enter a password:
        Console.WriteLine("\nEnter a password for validation:");
        string userPassword = Console.ReadLine();
        bool isUserValid = Regex.IsMatch(userPassword, pattern);
        Console.WriteLine(isUserValid ? "Valid password." : "Invalid password.");
    }
}
