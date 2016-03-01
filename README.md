Twentyseven
===========

Rubik's cube solver in Haskell.

Inspired by [Herbert Kociemba's *Cube Explorer*](http://kociemba.org/cube.htm).

The main idea is to precompute, for every configuration, the number of moves
required to put certain subsets of the 27 cubies composing the 3x3 Rubik's cube
in their right place and/or in the right orientation. This gives lower bounds
used for an A⋆-like search in the graph of scrambled cubes.

---

By default, a suboptimal "two-phase" solver is used, as it runs rather quickly.
It currently solves 1000 random cubes (uniformly distributed) in about one
minute. The optimal solver is quite slow however, taking between five minutes
and two hours to solve a random cube (18 moves in average).

The solver must precompute a certain number of lookup tables, which can be
stored in files. These tables take fifteen seconds to compute and weigh 13MB
for the two-phase solver, compare that to about 8 hours and 2GB for the optimal
one!

You may check the produced files with the checksums in `ts-tables.sha256`.

Usage summary
-------------

    twentyseven [-p] [--strict] [-d DIR] [--optimal]

- For the first invocation, use `-p` to precompute nonexistent lookup tables,
  otherwise an error is thrown when `twentyseven` tries to load them;
- `--strict` loads tables immediately, otherwise they are loaded "by need" (so
  you can also send it a cube to solve);
- `-d DIR` specifies the directory where the tables should be read and written
  (default: `$HOME/.27/`).

The input is read line by line.

Input format
------------

A line can be one of:

- A string of 54 characters (ignoring spaces) from a set of (almost any) 6
  characters. Each character then corresponds to the color of one facelet, in
  the order illustrated below.

  Output: a sequence of moves to unscramble it.

  Facelets are numbered in base 9. Faces `0,1,2,3,4,5` correspond to `U,L,F,R,B,D`.

                  00 01 02
                  03 04 05
                  06 07 08

        10 11 12  20 21 22  30 31 32  40 41 42
        13 14 15  23 24 25  33 34 35  43 44 45
        16 17 18  26 27 28  36 37 38  46 47 48

                  50 51 52
                  53 54 55
                  56 57 58

- A dot `.` followed by a sequence of moves to scramble the cube.

  The basic moves are given by a letter in `[ULFRBD]`, or their lowercase
  counterparts.  Each letter corresponds to a clockwise quarter turn of the
  given face (up, left, front, right, back, down).  The orientation is
  determined when looking directly at the turning face.

  For every basic move, an optional suffix `[23']` allows to specify a half
  turn (e.g., `U2`), equivalent to a sequence of two quarter turns (`UU`), or a
  counterclockwise quarter turn (e.g., `U3` or `U'`) equivalent to a sequence
  of three clockwise (`UUU`).

  Output: a description of the resulting cube if the moves are applied starting
  from the solved cube (in the format above, with letters `ULFRBD` as
  colors).

- The keyword `random`.

  Output: a random *solvable* cube with uniform distribution.

- The keyword `quit` (or an end-of-file) terminates the interactive session.

Example
-------

### Initialization

    $ twentyseven -p --strict
    quit

### Example

`examples.txt`:

    qwqwqwqwq erererere tytytytyt rerererer ytytytyty wqwqwqwqw
    qwqwqwqwq erqrerere tytytytyt rerererer ytytytyty wqwqwqwqw
    BBBBUBBBB UUUULUUUU RRRRFRRRR DDDDRDDDD LLLLBLLLL FFFFDFFFF
    DDDFUDLRB FUFDLLLRR UBLBFDFUD ULBFRULLB RRRLBBRUB UBFFDFDRU
    111121111 333313333 222232222 444454444 666646666 555565555
    111111214 223222222 131333333 344444444 555555555 666666666
    .udddlrrrbfffuddd
    random

The output then looks like this:

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

Detail of current heuristics
----------------------------

The distance estimations are based on cosets corresponding to the following
elements.

### Two-phase

#### Phase 1

- Corner Orientation × UD Slice
- Edge Orientation × UD Slice

It is possible to store the actual distances to the goal set in phase 1 but
the current speed seems good enough for now.

#### Phase 2

- Corner Permutation × UD Slice Permutation (Phase 2)
- UD Edge Permutation (Phase 2) × UD SlicePermutation (Phase 2)

### Optimal

- Corner Orientation × XY Slice Permutation, for XY in {UD, LR, FB}
- Corner Orientation × Corner Permutation
