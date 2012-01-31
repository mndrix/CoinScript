Introduction
============

**CoinScript** is a toy programming language for experimenting with
stack-oriented programming, type inference and termination analysis.
[Bitcoin's scripting language](https://en.bitcoin.it/wiki/Script)
pointed out that I know little about stack-oriented programming.
This is me learning.
As a hat tip, and since I'm bad at naming, it's called CoinScript.

A CoinScript program is a string of Unicode characters.  Like
[FALSE](http://en.wikipedia.org/wiki/FALSE)
and [F](http://www.nsl.com/k/f/f.htm) ("Functional False"),
each character in the script is a stack operation.  The
script `40 2+` adds
the numbers 40 and 2, leaving 42 on the stack as the program's
output.

So far, CoinScript's only unique feature (as far as I can tell), is
the type inference mechanism.  All language operations are defined
in terms of a generic virtual machine interface.  Running a program
on the "data machine" produces its output.  Running a
program on the "type machine" produces its type signature.

The first implementation was 116 lines of Haskell.  That seems
pretty decent for a language interpreter and type inference engine.

Stack Operations
================

  * `+` - numeric addition
  * `d` - dup
  * `D` - drop
  * `t` - boolean true
  * `f` - boolean false
  * `1234` - integer literal
  * `"foo"` - string literal
  * space - noop (useful for separating consecutive integers)
