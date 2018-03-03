# samsara
A Samsara compiler in OCaml. Also an assignment repo for CSC-312
Language Implementation with Peter Michael Osera. By Theo Kalfas.

# samsara
samsara is currently a strongly typed functional language with infix operators
and recursion. It supports basic arithmetic operations and comparisons on
integers and floating-point numbers. It also supports if-expressions, booleans,
function declaraction, and let-bindings. For a deeper explanation of the
syntax, refer to the __Syntax__ section below.

## Execution
Run samsara in the terminal, using `./samsara.native file1 file2 ...`, where the
file arguments are paths to files that contain a samsara expression. samsara
will then print out the evaluation of each of these expressions. If no file is
specified, or if the `-stdin` flag is set, samsara will read from stdin instead.
This is useful when piping input for samsara, or as a quick execution of a
samsara expression in the terminal. Hint: to send EOF in the terminal, press
`Crtl-D`.

samsara supports several execution flags:

* _-lex_ prints out the lexed tokens.
* _-parse_ prints out the parsed AST in S-Expression format.
* _-step_ prints out all the small-step evaluations of the samsara expression
    given until the final evaluation to a singular value.
* _-type_ simply typechecks the expression and prints the type of the given expression.
* _-stdin_ switches input from files to stdin.

## Syntax
The syntax is described by the following context free grammar:

    e    ::= (e) | n | bool | if e then e else e
           | fun (x:t) : t => e | fix f (x:t) : t => e | fun () => e
           | x | e <- e | let x:t = e in e
           | (e, e) | fst <- e | snd <- e
           | []:t | e :: e | hd <- e | tl <- e | empty <- e
           | ()
    n  ::= (n) | n | f | NaN | n + n | n - n
           | n * n | n / n | n % n
    bool ::= (bool) | true | false | n < n | n <= n | n > n
           | n >= n

    t    ::= num | bool | unit | [t] | (t, t) | t->t

Where:

* Parentheses are optional.
* `()` is the unit value.
* `n` is a positive integer literal, such as `1` or `912`
* `f` is a positive floating point literal, such as `1.1234` or `0.12312`
* `NaN` is the respective representation of the IEEE 754 NaN.
* `+`, `-`, and the rest of the `num` expressions are their respective
  operations on numbers. An operation on two integers evaluates to an integer,
  while an operation that contains at least one float returns a float. All of
  these are left associative.
  * `%` has the least priority, while `+` and `-` have medium priority, with
      `*` and `/` having the greatest priority.
* `NaN` is taintful in operations, meaning that any operation that contains
  `NaN` returns `NaN`.
* `<` `>`, and the rest of the `bool` expressions are their respective
  comparisons on numbers. An operation returns a boolean.
* `true` and `false` represent their respective boolean values.
* The rule `t` describes the types of the language, where `num` represents
  numbers, `bool` booleans, `unit` the unit type, and `[t]` is a list of type `t`, and
  `(t1, t2)` is a pair of types `t1` and `t2`.
  * `t1 -> t2` is a chain of types, given to functions that take an element of
    type `t1` and return an element of type `t2`. This can be chained
    right-associatively, to construct the types for functions that take multiple
    arguments, `t1 -> t2 -> t3 -> ...`.
* The `if` expression evaluates the boolean argument and if it evaluates to
  `true`, then the `if` expression evaluates to the second `e` expression that
  is given to it, and if the boolean evaluates to `false`, then the `if`
  expression evaluates to the third `e` expression that is given to it.
  * The third `e` expression only captures the minimal expression to the right
      of the `else` keyword. This is due to the left associativity of the
      operations. Use parentheses if you want to catch a bigger expression.
  * The first `e` expression must evaluate to a boolean.
* `fun (x:t1) : t2 => e` defines a function of type `t2` that takes a single
  argument `x` of type `t1` that evalutates to the expression `e` when applied
  to an argument.
  * `fun () => e` is suntastic sugar for defining a function that takes the `()`
    unit as an argument, to prevent code such as `fun (x:unit) => e` where `e` doesn't
    even contain the `x` label.
* `fix f (x:t) : t => e` defines a function that takes a single argument `x` of
  type `t` that evalutates to the expression `e` when applied to an argument.
  `f` refers to the function itself, allowing for recursive calls in the
  function body `e`.
* `e <- e` refers to function application, where the first `e` is  a
  function that takes a single argument, and `e` is applied to that
  function and substituted in its body.
  * To define functions that take more than 1 argument we can use the
    sequence `fun x => fun y => ... => e`, so that we can then apply to it many
    arguments using the sequence `arg1 <- arg2 <- arg3 <- ...`, such that the
    first argument is applied and leaves a function that takes another
    argument. Thus, we can have functions that seemingly take many
    arguments.
* `let x:t = e in e` defines a let-binding of the variable `x` to the first
  expression, inside the second expression. An example is `let x = 2 in x + 2`
  will evaluate to `2 + 2`.
* `x` refers to a variable name, defined in lets and functions, which can be
  found in function bodies and let-binding bodies. It will be replaced by its
  definition in let-bindings and by the argument applied in functions.
* `(e1, e2)` constructs a pair of the expressions `e1` and `e2`.
* `fst <- e` provides the first element of a pair `e`, while `snd <- e` provides the
  second.
* `[]:t` constructs an empty list of type `t`.
* `e1::e2` appends `e1` to the list `e2`, given that it typechecks correctly.
* `hd <- e` returns the head element of the list `e`, while `tl <- e` provides
  the rest of the elements as a list.
* `empty <- e` returns `true` if the list `e` is empty, and `false` if it is not.

Some operations have margin for errors and type mismatches. These are usually
raised with an error message describing the error, but not its location, because
ocamllex is being a bit hard on me, and I don't have enough time to do it right
now.

Some more notes:

* A samsara file should contain a single samsara expression. Number literals
    cannot be followed by letters without a space separating them.
* Function application is left-associative. Due to the specific order of
    operations, arguments such as `2 + 2` in a chain of function
    applications have to be surrounded by parentheses.

### Typechecking

After parsing, samsara typechecks the given expression to make sure there are no
type mismatches. For the rules of typechecking, refer to the
[Assignment website](http://www.cs.grinnell.edu/~osera/courses/csc312/18sp/homework/05-types.html).
They are very sensible and predictable.

TODO: Add typechecking rules directly in README.md

## Dependencies
samsara is written in OCaml, and uses the core library exclusively. Follow
the official instructions from the [OCaml website](https://ocaml.org/docs/install.html)
to install the latest OCaml tools.

The `ocamlbuild` compiler is used to build the project. The Menhir library as
well as `ocamllex` are used for parsing and lexing. Use `opam` to download
Menhir with

    opam install menhir

This should also install `ocamlbuild`, since it's a dependency, but in the rare
case that it doesn't `ocamlbuil` can be installed using

    opam install ocamlbuild

A number of common GNU tools are also required for tests and simple builds.
These tools are as follows:

* bash, located in `/bin/bash`
* `make`
* `diff`
* `rm`
* `cd`
* `echo`
* `pwd`

# simplecli
A simple exercise in CLI construction in OCaml.

simplecli takes an amount of arguments and prints them out in order, one in
each line. It also supports two command flags:

* **-length** -- prints out the lengths of the arguments instead of the arguments
    themselves.
* **-help**  -- prints out a usage message for simplecli.

These, or equivalent versions are available with almost all Linux and MacOS
installations, and are mostly available in Windows using the [Windows Subsystem
for Linux](https://docs.microsoft.com/en-us/windows/wsl/about).

## Dependencies
See __samsara__ dependencies.

## Execution
Run simplecli in the terminal, using `./simplecli [flag] arg1 arg2 arg3`,
where `arg1` `arg2` `arg3`, etc are the arguments that are to be echoed back
into the terminal line by line, and `[flag]` an optional flag, as seen above in
the **simplecli** section.

Note about flags: The `-help` flag overrides any other execution. If it's
present, then the usage message will be displayed and no arguments will be
echoed. Invalid flags also interrupt the program and print the usage statement.

# Build
To compile simplecli, run `make simplecli` in the root directory.

To compile samsara, run `make` or `make samsara` in the root directory.
If you're a dev and you make changes to simplecli, there might be an error
about inconsistent interfaces. If so, run `make clean` before building the
project.

To remove all compiled and intermediary files, run `make clean`.

# Testing
Tests can be ran using `make test`. They produce a diff of the expected outputs
of the arguments in the test dirs vs the actual results when ran. An easy way
of checking if the test was successful in a script is to check the result of the
variable `$?`, which reports the exit code of the last command that ran. If it's
`0`, then the results are identical, else the test has failed.

# Hooks
It's highly recommended that git hooks are installed for this repo to automate
testing and keep commits clean and correct. Refer to the HOOKS.md file in the
**hooks/** directory.

# Changelog

## Assignment 04 - 2018-02-23

### Added
* Typechecking!
  * A very flexible `num` type, and a `bool` type.
  * Other functional language types such as `unit`, pairs, and lists.
  * In-built functions for manipulating such types such as `hd` and `fst`.
* Tests for types, pairs, lists

### Known bugs
* Insufficient and inaccurate error reporting.
* No documentation in code (coming soon, gotta bugfix the developer first)

## Assignment 04 - 2018-02-23

### Added
* Let bindings, functions and function application.
* Compilation flags for step-by-step evaluation and stdin input.
* Tests for recursion, lets, and function application.

### Changed
* `pre-commit` hook now cleans up after itself.

### Known bugs
* Insufficient and inaccurate error reporting.

## Assignment 03 - 2018-02-16

### Added
* Menhir and ocamllex parser generation.
* Compilation flags for lexing and parsing.
* Tests for lexing and parsing, and scripts to add them nicely.

### Changed
* samsara grammar now uses infix operators with optional parentheses.

### Known bugs
* Insufficient and inaccurate error reporting.

## Assignment 02 - 2018-02-08

### Added
* samsara compiler source code.
* Documentation on samsara syntax.
* Tests for samsara in `test/samsara/`.
* Script to easily add more tests for samsara in `test/samsara/new-test`.
* Makefile target for `samsara`.

### Changed
* Tests for simplecli moved to  `test/simplecli/`

### Known bugs
* None

## Assignment 01 - 2018-01-30

### Added
* simplecli source code.
* Tests for simplecli in `test/`.
* Makefile with targets for `make`, `clean` and `test`.

### Changed
* None

### Known bugs
* None
