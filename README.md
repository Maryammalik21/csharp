# C# Programming Repository

This repository contains a collection of C# programming assignments and projects focused on compiler design, lexical analysis, and regular expressions. The code demonstrates various concepts in programming language theory and implementation.

## 📁 Repository Structure

```
csharp/
├── lab1/           # Password validation and generation using regex
├── lab2/           # Variable name validation
├── lab3/           # Floating-point number validation
├── lab4/           # Lexical analyzer implementation
├── lab5/           # Enhanced lexical analyzer with hash table
├── lab mid/        # Midterm lab exercises (currently empty)
└── minicompiler/   # Mini compiler implementation
```

## 📚 Lab Descriptions

### Lab 1: Password Validation & Generation
**Files:** `task1.cs`, `task2.cs`

- **Task 1 (Password Validator):** Implements a regex-based password validator with specific requirements:
  - Must contain substring "sp" (from registration number)
  - At least one uppercase letter
  - At least 2 special characters
  - At least 4 lowercase letters from "maryam tanveer malik"
  - Maximum length of 12 characters

- **Task 2 (Password Generator):** Generates random passwords based on user input (first name, last name, registration number, favorite movie, and food) that meet specific security criteria.

### Lab 2: Variable Name Validation
**Files:** `task1.cs`, `task2.cs`, `task3.cs`

Implements regular expression validators for:
- Variable name validation (must start with a letter, max 25 characters)
- Additional validation tasks

**Example:**
```
Input: varName
Output: Valid variable name
```

### Lab 3: Floating-Point Number Validation
**Files:** `task1.cs`

Validates floating-point numbers using regex patterns:
- Supports up to 6 digits before and after decimal point
- Regex pattern: `^\d{1,6}(\.\d{1,6})?$`

### Lab 4: Basic Lexical Analyzer
**Files:** `task.cs`

A lexical analyzer that:
- Identifies keywords: `int`, `float`, `while`, `main`, `if`, `else`, `new`
- Recognizes variables, constants, operators, and special characters
- Generates token output
- Maintains a symbol table with index, name, type, and value

**Supported Patterns:**
- Variables: `^[A-Za-z_][A-Za-z0-9_]*$`
- Constants: `^[0-9]+([.][0-9]+)?([e]([+|-])?[0-9]+)?$`
- Operators: `^[-*+/><&&||=]$`
- Special chars: `^[.,'\[\]{}();:?]$`

### Lab 5: Enhanced Lexical Analyzer with Hash Table
**Files:** `task.cs`, `output.png`

An improved version of Lab 4's lexical analyzer that:
- Uses a Dictionary (hash table) for efficient symbol table management
- Provides better performance for symbol lookup
- Same token recognition capabilities as Lab 4
- Includes output screenshot for reference

### Minicompiler: Complete Compiler Implementation
**Files:** `code.cs`, `output.txt`

A comprehensive mini compiler featuring:

**Lexical Analyzer (Scanner):**
- Token types: Keywords, identifiers, literals, operators, delimiters
- Supported keywords: `int`, `float`, `string`, `bool`, `if`, `else`, `while`, `for`, `return`, `print`
- Operators: Arithmetic (`+`, `-`, `*`, `/`, `%`), Relational (`==`, `!=`, `<`, `>`, `<=`, `>=`), Logical (`&&`, `||`, `!`)
- Line and column tracking for error reporting

**Token Types:**
- Keywords and data types
- Identifiers and literals (numbers, strings, booleans)
- Operators (arithmetic, relational, logical)
- Delimiters (parentheses, braces, semicolons, commas)

## 🚀 Getting Started

### Prerequisites
- .NET SDK (recommended: .NET 6.0 or later)
- C# compiler
- Any C# IDE (Visual Studio, VS Code, Rider) or command-line tools

### Running the Programs

1. **Clone the repository:**
```bash
git clone https://github.com/Maryammalik21/csharp.git
cd csharp
```

2. **Compile and run any lab:**
```bash
# Navigate to the desired lab folder
cd lab1

# Compile the C# file
csc task1.cs

# Run the executable
./task1.exe  # Windows
mono task1.exe  # Linux/Mac
```

Or use `dotnet run` if you have a .NET project structure.

## 📖 Usage Examples

### Lab 1 - Password Validator
```bash
cd lab1
csc task1.cs
./task1.exe
```
The program will test several predefined passwords and allow you to enter your own.

### Lab 4/5 - Lexical Analyzer
```bash
cd lab5
csc task.cs
./task.exe
```
Enter your code (press Enter twice to end input):
```
int x = 5;
float y = 3.14;
```

**Output:**
```
Tokens:
<keyword, int>
<var, x>
<punc, =>
<const, 5>
<punc, ;>
...

Symbol Table:
Index: 0, Name: x, Type: identifier, Value: -
Index: 1, Name: y, Type: identifier, Value: -
```

## 🔍 Key Concepts Covered

1. **Regular Expressions:** Pattern matching for lexical analysis
2. **Lexical Analysis:** Breaking source code into tokens
3. **Symbol Table Management:** Storing and retrieving identifier information
4. **Token Recognition:** Identifying keywords, operators, literals, and identifiers
5. **Compiler Design Fundamentals:** Understanding the first phase of compilation

## 📝 Notes

- The lexical analyzers support scientific notation for numbers (e.g., `1.5e-10`)
- Symbol tables track variables and their attributes
- The minicompiler provides a foundation for further development (parser, semantic analyzer, code generator)
- Some labs include example inputs/outputs as comments in the code

## 👤 Author

**Maryam Malik**
- GitHub: [@Maryammalik21](https://github.com/Maryammalik21)

## 📄 License

This project is created for educational purposes as part of compiler design coursework.

## 🤝 Contributing

This is a personal academic repository. However, suggestions and feedback are welcome!

## 📧 Contact

For questions or discussions about the code, please open an issue in this repository.

---

**Last Updated:** January 2026
