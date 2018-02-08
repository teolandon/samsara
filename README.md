# samsara
A Samsara compiler in OCaml. Also an assignment repo for CSC-312
Language Implementation with Peter Michael Osera. By Theo Kalfas.

# samsara
samsara is currently an interpreted language using S-Expressions. It supports
basic arithmetic operations and comparisons on integers and floating-point
numbers. It also supports if-expressions and booleans. For a deeper explanation
of the syntax, refer to the __Syntax__ section below.

## Execution
Run samsara in the terminal, using `./samsara file1 file2 ...`, where the file
arguments are paths to files that contain a samsara expression. samsara will
then print out the evaluation of each of these expressions. No compilation flags
are yet implemented.

## Syntax
The syntax is described by the following context free grammar:

    e    ::= num | bool | (if bool e e )
    num  ::= n | f | NaN | (+ num num) | (- num num)
           | (* num num) | (/ num num) | (mod num num)
    bool ::= true | false | (< num num) | (<= num num) | (> num num)
           | (>= num num)

Where:

* `n` is a positive integer literal, such as `1` or `912`
* `f` is a positive floating point literal, such as `1.1234` or `0.12312`
* `NaN` is the respective representation of the IEEE 754 NaN.
* `+`, `-`, and the rest of the `num` expressions are their respective
    operations on numbers. An operation on two integers evaluates to an integer,
    while an operation that contains at least one float returns a float.
* `NaN` is taintful in operations, meaning that any operation that contains
    `NaN` returns `NaN`.
* `<` `>`, and the rest of the `bool` expressions are their respective
    comparisons on numbers. An operation returns a boolean.
* `true` and `false` represent their respective boolean values.
* The `if` expression evaluates the boolean argument and if it evaluates to
    `true`, then the `if` expression evaluates to the first `e` expression that
    is given to it, and if the boolean evaluates to `false`, then the `if`
    expression evaluates to the second `e` expression that is given to it.

Some operations have margin for errors and type mismatches. These are described
below:

[[[PLACEHOLDER]]]

Some more notes:

A samsara file should contain a single samsara expression. Number literals
cannot be followed by letters without a space separating them.

## Dependencies
samsara is written in OCaml, and uses the core library exclusively. Follow
the official instructions from the [OCaml website](https://ocaml.org/docs/install.html)
to install the latest OCaml tools.

The `ocamlopt` compiler is used to build the project.
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

## Assignment 01 - 2018-01-30

### Added
* simplecli source code
* Tests for simplecli in `test/`
* Makefile with targets for `make`, `clean` and `test`

### Changed
* None

### Known bugs
* None
