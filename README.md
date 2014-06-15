#fronsduk#

A SECD machine for a school project.

It provides a basic SECD machine, and a language that can be compiled
to the machine's bytecode : *Qzitche*.

##Installation##

You can install it as a standard cabal project.

##Usage##

The project has 4 programs:
- `fronsduk`: the bytecode interpreter
- `fronsduk-assemble`: the assembler to bytecode compiler
- `fronsduk-disassemble`: the bytecode disassembler
- `qzitchec`: the qzitche to bytecode compiler

###Compile assembler###

The `fronsduk-assemble` program takes an assembler program as input,
and outputs the bytecode. The easiest usage, in console, is:

    fronsduk-assemble <input.asm >output.fdk

You can test with examples in the `asm` directory

###Run###

Bytecodes can be run using `fronsduk` program. It can either receive the
bytecode in stdin, or read the file if the name is given as command-line argument:

    fronsduk <bytecode.fdk

or

    fronsduk bytecode.fdk

###Disassemble###

The program `fronsduk-disassemble` does the reciprocal of `fronsduk-assemble`,
with a similar interface.

###Compile Qzitche code###

The program `qzitchec` takes a qzitche program as input, and outputs the
resulting bytecode.

To see what qzitche looks like, see the examples in the `qzitche` directory.

###Emacs support###

Major modes for editing assembler and qzitche programs can be found in
the directory `extra/emacs`. They implement syntax coloration, and
buffer evaluation with the shortcut `C-c C-b`. You can also see the
assembler output in qzitche-mode using `C-c C-d`.

###License###

This project is released under the GPL v3 license. See `LICENSE` for more details.
