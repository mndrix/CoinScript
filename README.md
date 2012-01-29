Introduction
============

CoinScript is a toy programming language for experimenting with
stack-based programming, type inference and termination analysis.  
[Bitcoin's stack-based scripting language](https://en.bitcoin.it/wiki/Script)
pointed out that I know little about stack oriented programming.
This is my attempt to learn.
As a hat tip, and since I'm bad at naming, it's called CoinScript.

A CoinScript program is a string of Unicode characters.  Like
[FALSE](http://en.wikipedia.org/wiki/FALSE)
and [F ("Functional False")](http://www.nsl.com/k/f/f.htm),
each character in the script is a stack operation.  The following
script adds
the numbers 40 and 2, leaving 42 on the stack as the result

    40 2+

So far, CoinScript's only unique feature (as far as I can tell), is
the type inference mechanism.  All language operations are defined
in terms of a generic virtual machine interface.  Running a program
on the "data machine" produces its output.  Running a
program on the "type machine" produces its type signature.

The first implementation is 116 lines of Haskell code.  That seems
pretty decent for a statically typed language with type inference.

Stack Operations
================

  * ' ' - noop
  * '+' - numeric addition
  * 'd' - dup
  * 'D' - drop
  * integer literals - pushed onto the stack