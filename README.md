# programming-languages-exercises
🔣 Implementations of key programming language features using Scheme, developed in the Programming Languages (20905) course.

# Introduction
This repository contains implementations of various programming language features that were part of the exercises in the **Programming Languages (20905)** course. All the exercises are implemented in **Scheme.** Throughout this course, I gained a deep practical understanding of the distinction between **Concrete** and **Abstract Syntax** in programming languages, as well as how interpreters evaluate different types of expressions. Beginning with simple expressions like if-expressions and map-expressions, I progressed to more complex constructs such as **multi-parameter procedures, procedure calls, assignment expressions,** and beyond. Through these hands-on implementations, my comprehension of programming languages has significantly deepened, especially in understanding concepts like Environments, References, and other critical programming language features.

# Feature Implementations
+ **Arithmetic Operations:** Implements the interpretation and concrete syntax of mathematical operations in the created language, including addition, division, greater-than boolean expression, and more.
+ **Arrays Initialization:** Implements the interpretation and concrete syntax of array-related operations in the created language, including array creation, array indexing, and setting the value of an array cell.
+ **Dynamic Binding:** Changes the calling technique of procedures in the language to dynamic binding instead of static. This means procedures are now called with the environment defined during the call, rather than the environment at the time of their definition.
+ **Exception Handling:** Implements the interpretation and concrete syntax for a try-expression and a throw-expression in the created language, providing the capability to manage error handling within the program.
+ **Lists Transformations:** Implements the interpretation and concrete syntax for list manipulation expressions, similar to those provided by Scheme, allowing operations such as retrieving the first element, the rest of the list, and more.
+ **Objects Creation:** Implements the interpretation and concrete syntax for creating new records (objects) with fields and provides the capability to modify their values.
+ **Overloading:** Implements the interpretation and concrete syntax for overloading, allowing multiple ways to handle function call arguments based on their type and throwing an error if no suitable overload is found.
+ **Proc Language:** This exercise involved creating a multi-parameter procedure expression in the Proc language. The implementation utilized a single-parameter procedure expression (currying) and incorporated a recursive function based on the Y-combinator concept.
  
