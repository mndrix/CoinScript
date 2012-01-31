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

Data Types
----------

**Boolean**: the operations `t` and `f` push a true or false value onto the
stack, respectively.

**Integers**: an integer literal like `1234` is pushed onto the stack as is.
To push consecutive numbers, separate them with a space: `1 2`.

**Strings**: a string literal is enclosed in double quotes, `"like this"`.

**Lists**: a list starts with `(` and ends with `)`.  Values inside
the list are separated with `,`.  For non-empty lists, a trailing `,`
is required.  For example, `(1,t,4 1+,)` pushes a list onto the stack
containing 1, true and 5.  This works because `(` pushes an empty list
onto the stack and `,` appends items onto it.  The same result could
be accomplished with `()1,t,4 1+,`


Stack Operations
----------------

  * `+` - numeric addition
  * `d` - dup
  * `D` - drop
  * `(` - push an empty list
  * `,` - append an item to a list
  * `)` - noop (required for balancing `(`)
  * space - noop (useful for separating consecutive integers)
