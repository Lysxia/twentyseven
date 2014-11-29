Twentyseven
===========

Rubik's cube solver in Haskell

Inspired by Herbert Kociemba's *Cube Explorer*.

---

This project builds an executable `twophase`.

`twophase` can be run either with an input as a command line argument,
or interactively.

In an interactive session,
the first call to the solver will first take about 15s to initialize move
tables for the whole session. It is also possible to start a session
with an argument being a single dash `twophase -`, then the initialization
is done before waiting for an input.
An empty line terminates the interactive session.

The two-phase algorithm finds solutions with a suboptimal number of moves,
but runs rather quickly.

---

Input format
------------

The input must be one of:

- a string of length 54 made of a set of (almost any) 6 characters.
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
  
  `twophase` then replies with a description of the resulting cube,
  if the moves are applied starting from the solved cube.
  (in the format above, with letters `ULFRBD` as colors).

- A single exclamation mark `!`. `twophase` generates a random solvable cube
  and solves it.
  
Spaces are ignored.

---

Example
-------

    $ twophase qwqwqwqwq erererere tytytytyt rerererer ytytytyty wqwqwqwqw
    U2 D2 L2 R2 F2 B2
    $ twophase qwqwqwqwq erqrerere tytytytyt rerererer ytytytyty wqwqwqwqw
    Facelets [6,18,11] ("qtq") do not match any regular cubie.
    $ twophase
    > .udddlrrrbfffuddd
    BBBBUBBBB UUUULUUUU RRRRFRRRR DDDDRDDDD LLLLBLLLL FFFFDFFFF
    > 111121111 333313333 222232222 444454444 666646666 555565555
    U  D  L  R  F  B  U2 B2 L2 F2 D2 B2 R2 U' D' L2
    > 111111214 223222222 131333333 344444444 555555555 666666666
    L  U' F2 U  F2 U  L  U' L2 D  F2 D' F2
    > !
    UFBLUBULF LFFULUFDL RULFFUDLU DDRRRBBLF DRBRBBRDU BDRRDBLFD
    B L F U F D2 R' B' L U' R2 F2 U2 R2 D L2 D R2 D L2 D F2
    >
    $

---

To do:
- Better UI
- Visual input
- Optimal solver
- Benchmarks

[Project page](https://lysxia.github.io/twentyseven) (no content yet)
