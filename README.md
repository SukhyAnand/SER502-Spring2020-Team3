# SER502-Spring2020-Team3
#### Team project for SER-502: Compiler and Virtual Machine for a Programming Language
&nbsp;

# YEPL - Yet Another Programming Language

## System Specifications

Compiler and Runtime Environment:

* [Operating System] - Windows 10
* [Processor] - Intel(R) Core(TM) i5-8265U CPU
* [Memory] - 8 GB RAM

## Tools Used

This project utilizes the below tools:

* [Prolog] - A Declarative Programming Language for implementing the compiler and runtime environment.

## Installation

1) This project requires the standard [SWI-Prolog] 8.0.3-1 to run. Download it from the below link:

```sh
https://www.swi-prolog.org/download/stable
```
2) Download the code from the repository:

```sh
$ git clone https://github.com/SukhyAnand/SER502-Spring2020-Team3.git
$ cd SER502-Spring2020-Team3
```

## Build/Run Instructions

```
1) Open the SWI-Prolog runtime environment.
2) Consult and execute both the files compiler.pl and runtime.pl.
3) Run the predicate yepl("<FILE_PATH/FILE_NAME>.yepl"). The .ic file is generated in the same directory as the input file.
4) Run the predicate runYepl("<FILE_PATH/FILE_NAME>.ic") to execute the intermediate code file. 
```

## Example Run

For Windows: 

```
1) yepl("C:/Users/lenovo/Desktop/ASU Assignments/SER-502/Team Project/SER502-Spring2020-Team3/data/dummy.yepl").
2) runYepl("C:/Users/lenovo/Desktop/ASU Assignments/SER-502/Team Project/SER502-Spring2020-Team3/data/dummy.ic").
```

## Requirements Addressed

```
1) Implemented primitive data types types bool, int, string.
2) Implemented operations on bool and int datatype.
3) Support for addition subtraction, multiplication and division operations on int datatype.
4) Support for and, not, or operations for bool datatypes.
5) Support for assigment operations and evaluation of expressions.
6) Support for 'if-else' selection statements.
7) Support for traditional 'while' iteration statements.
8) Support for traditional 'for' iteration statement.
9) Support for 'for in range' iteration statement.
10) Support for 'ternary operator (?:)'.
11) Support for 'print' statement.
12) Generates intermediate code (parsetree) and saves it to a .ic file.
13) Interpreter takes .ic file as input and prints the ouput on the Prolog runtime environment.
```

## Extra Features Added

```
1) Implemented type safety check to ensure a variable holds a value permitted by the domain defined by its datatype.
2) Implemented type casting of data types bool and int.
3) Support for nested if-elseif-else statements.
4) Support for different variants of for loops, with or without initialization statement, with or without increment statement.
5) Support for mutable and non-mutable expressions to handle r-value and l-value safety checks.
6) Support for mutable operators: "=", "+=", "-=", "*=", "/=", "++" and "--".
7) Support for relational operators: "<", "<=", ">", ">=", "==" and "!=".
8) Support for unary operators: "+" and "-".
9) Support for '%' arithmetic operator.
10) Support for paranthesis in expressions "(" and ")".
11) Handling of prededence and associativity for all expression evaluations based on C language.
```

## Team Members

```
1) Sukhpreet Singh Anand (ssanand3@asu.edu)
2) Sakshi Jain (smjain@asu.edu)
3) Aditya Bajaj (anbajaj@asu.edu)
4) Aihaab Shaikh (aashaik2@asu.edu)
```

## YouTube Link:

```
https://youtu.be/BiSkYMSSa5g
```
