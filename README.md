Twentyseven
===========

Rubik's cube solver in Haskell

Inspired by Herbert Kociemba's *Cube Explorer*.

---

This project builds an executable `twentyseven`.

`twentyseven` reads from standard input and writes to standard output.

Command line arguments:

- `-p` (Must be on at the first call) Precomputes tables
and saves large pruning tables in `path/to/tables/`. Then exit.
- `-v` Verbose mode. Print the time taken to solve a cube.
- `--table-dir=/path/to/tables/` Set the path of the directory where tables are
stored. Default: `.27` (in the current directory!).
- `--optimal` Use the optimal solver. This is still experimental and does
not terminate within a reasonable amount of time beyond ten moves or so.

The two-phase algorithm finds solutions with a suboptimal number of moves,
but runs rather quickly. It uses a different set of heuristics from those
described on H. Kociemba's page.

Phase 1 uses the maximum estimation from

- Edge orientations and UD slice edge positions;
- Corner orientations and UD slice edge positions.

Phase 2 uses the maximum estimation from

- Edge permutation;
- Corner permutation and UD slice edge permutation.

`twentyseven` currently solves 200 random cubes (uniformly distributed)
in about one minute.

Input format
------------

The input must be one of:

- a string of length 54 (ignoring spaces) made of a set of (almost any) 6 characters.
  Each character then corresponds to the color of one facelet,
  in the order illustrated below.

        Facelets are numbered in base 9. Faces 0,1,2,3,4,5 correspond to U,L,F,R,B,D.
    
                  00 01 02
                  03 04 05
                  06 07 08
              
        10 11 12  20 21 22  30 31 32  40 41 42
        13 14 15  23 24 25  33 34 35  43 44 45
        16 17 18  26 27 28  36 37 38  46 47 48
    
                  50 51 52
                  53 54 55
                  56 57 58

- a dot `.` followed by a sequence of moves to scramble the cube.

  The basic moves are given by a letter in `[ULFRBD]`,
  or their lowercase counterparts.
  Each letter corresponds to a clockwise quarter turn of the given face
  (up, left, front, right, back, down).
  The orientation is determined when looking directly at the turning face.

  For every basic move, an optional prefix `[23']` allows to specify
  a half turn (e.g., `U2`),
  equivalent to a sequence of two quarter turns (`UU`),
  or a counterclockwise quarter turn (e.g., `U3` or `U'`)
  equivalent to a sequence of three clockwise (`UUU`).

  `twentyseven` then replies with a description of the resulting cube,
  if the moves are applied starting from the solved cube.
  (in the format above, with letters `ULFRBD` as colors).

- The keyword `random`, `twentyseven` generates a uniformly random solvable
cube.

- `quit` or `(EOF)` terminate the interactive session.

Spaces are ignored.

Example
-------

###Initialization

    $ mkdir .27      # or use another directory
    $ twentyseven -p # and pass it with --table-dir=...

###Solving

`examples.txt`:


    qwqwqwqwq erererere tytytytyt rerererer ytytytyty wqwqwqwqw
    qwqwqwqwq erqrerere tytytytyt rerererer ytytytyty wqwqwqwqw
    BBBBUBBBB UUUULUUUU RRRRFRRRR DDDDRDDDD LLLLBLLLL FFFFDFFFF
    DDDFUDLRB FUFDLLLRR UBLBFDFUD ULBFRULLB RRRLBBRUB UBFFDFDRU
    111121111 333313333 222232222 444454444 666646666 555565555
    111111214 223222222 131333333 344444444 555555555 666666666
    .udddlrrrbfffuddd
    random

The output then looks like this (answers line by line)

    $ twentyseven < examples.txt
    U2 D2 L2 R2 F2 B2
    Facelets [6,18,11] ("qtq") do not match any regular cubie.
    U D F B L R U2 R2 F2 R2 U2 L2 B2 U' D' B2
    U L B' L R2 D R U2 F U2 L2 B2 U B2 D' B2 U' R2 U L2 R2 U
    U D L R F B U2 B2 L2 F2 D2 B2 R2 U' D' L2
    L U' F2 U F2 U L U' L2 D F2 D' F2
    BBBBUBBBB UUUULUUUU RRRRFRRRR DDDDRDDDD LLLLBLLLL FFFFDFFFF
    BDLLUFBUD LBUBLURFL RLBFFBFRU RLFURULRR UBDRBRDDU DFBDDDFLF

---

To do:
- Better UI
- Visual input
- Optimal solver
- Benchmarks

[Project page](https://lysxia.github.io/twentyseven) (no content yet)

