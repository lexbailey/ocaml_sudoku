# Sudoku solver in OCaml

Solves arbitrary size sudoku puzzles

See top of sudoku.ml for explanation of algorithm

## Build

The solver is built using dune, requires ocaml version 4.07 or later and ANSITerminal module.

There is a script to run dune for you and copy the executable to `sudoku`

    ./build

## Usage

Command line interface:

    sudoku [--verbose] filename

Where filename is the path to a file containing an intial board state.

the `--verbose` mode dumps out the domain computed after each step of the algorithm. This step may include many decisions about reductions of domains of individual cells, so is not particularly useful for gaining an insight into _how_ the puzzle was solved. But is fun to look at.

## Input files

Example:

    -----------
    |2,3,| ,1,|
    | , ,| , ,|
    -----------
    | , ,| , ,|
    |1, ,| ,4 |
    -----------

Spaces, newlines, dashes, and pipes (` -|⏎`) in input files are all ignored.

All non-ignored characters betwen commas represent cell values in order from left to right, top to bottom.

The following is also a valid way to specify the example puzzle above.

    2,3,,1, , ,, , , , , , 1, , ,4 

It's just less readable.

That example puzzle is a 4x4 puzzle. A traditional sudoku is 9x9, but this solver can solve arbitrary size puzzles.

The range of values applicable for a puzzle is from 1 to the side length of the puzzle square. Numbers always expressed in base 10 (so if you find a hexadoku example on the internet, you might need to convert it from `0-9A-F` into `1-16`).

The size of the puzzle is determined by the number of values in the input file, which must be some integer to the fourth power.

For example, puzzle files of lengths 1, 16, 81, 256 are the first four valid lengths (1^4, 2^4, 3^4, 4^4 respectively).

Puzzles of size 1 are trivial solved, always. An input file to represent the 1x1 case contains no commas, and optionally the number 1. (And of course as many of the other ignored characters as you like.)

There's various example input files included. Most of them have solutions, some are deliberately broken or have multiple solutions for testing that the tool handles them correctly.
