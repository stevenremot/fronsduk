#fronsduk#

A SECD machine for a school project.

##Installation##

You can install it as a standard cabal project.

##Usage##

The project has 3 programs:
- `fronsduk`: the bytecode interpreter
- `fronsduk-assemble`: the assembler to bytecode compiler
- `fronsduk-disassemble`: the bytecode disassembler

###Compile###

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

###Emacs support###

A major mode for editing files in fronsduk assembler is defined in
`extra/emacs/fronsduk-mode.el`.

###License###

This project is released under the GPL v3 license. See `LICENSE` for more details.
