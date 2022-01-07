# loxomotive - A Lox Interpreter implemented in Haskell

## Book

I read the book [Crafting Interpreters](http://www.craftinginterpreters.com/) by
Bob Nystrom and this is an interpreter implementation for the language `Lox`
that he created for the book. The first part of the book where you write the
interpreter is now finished and is really worth a look. It helped me learn a lot
about programming languages in general and also helped me to teach me some
haskell.

You can find the code to the book on github. The repository is linked as a
submodule.

Thank you Bob for writing this.


## Description

This implementation of a Lox interpreter is closely modelled after the
originial java implementation from the
[craftinginterpreters](http://www.craftinginterpreters.com/) book.
Both interpreters should be mostly compatible but don't take my word for that.

I got sidetracked with my [first](https://github.com/ccntrq/plox) two attemtpts
at implementing this interpreter while waiting for the book to finish.
Since this is my third time I didn't want to re-read the book again. This time
I decided to just look at the Java sources and translate them to haskell as
close as possible. I might refactor some of this later.


## Usage

### Dependencies

To build and run the interpreter you will need a version of [stack](haskellstack.org).

### Build

```
stack build
```

### Run

```
stack run -- [filename]
```

You can invoke `loxomotive` either with zero args to start the repl or with a
file to interpret that file

```
loxomotive v1.0.0
Usage: loxomotive [filename]
```
