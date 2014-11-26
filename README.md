Twentyseven
===========

Rubik's cube solver in Haskell

Inspired by Herbert Kociemba's
*Cube Explorer*.

This project builds an executable `twophase`.

`twophase` can be run either with a command line argument,
or interactively, where it initializes in about 15s,
and waits for an input.

The input must be one of:
- a dot followed by a string of `u`, `l`, `f`, `r`, `b`, `d`,
  or their upper case counterparts,
  terminated by a newline;
  that string specifies a sequence of moves to scramble the cube.
  It then replies with a sequence of moves which solve the cube
  (without cheating...).

  Each letter corresponds to a clockwise quarter turn of the given face
  (up, left, front, right, back, down).
  The orientation is determined when looking directly at the turning face.
  A half turn (e.g., `U2`) is equivalent to a sequence of two quarter turns,
  and a counterclockwise quarter turn (e.g., `U'`) to a sequence of three
  clockwise.
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

Spaces are ignored.
An empty line terminates interactive mode.

---

The two-phase algorithm finds solutions with a suboptimal number of moves,
but runs rather quickly.

---

Example:

    $ twophase qwqwqwqwq erererere tytytytyt rerererer ytytytyty wqwqwqwqw
    U2 D2 L2 R2 F2 B2
    $ twophase qwqwqwqwq erqrerere tytytytyt rerererer ytytytyty wqwqwqwqw
    Facelets [6,18,11] ("qtq") do not match any regular cubie.
    $ twophase
    Ready.
    > .udddlrrrbfffuddd
    U  D  F  B  L  R  U2 R2 F2 R2 U2 L2 B2 U' D' B2
    > 111111214 223222222 131333333 344444444 555555555 666666666
    L  U  L  D  L' U' L  D' F2 U' B2 U  F2 U' B2 U  L2
    >
    $

---

To do:
- Better UI
- Visual input
- Optimal solver
- Benchmarks
- Fix solver

        > 111121111 333313333 222232222 444454444 666646666 555565555
        
        ??? twophase ate all the memory

[Project page](https://lysxia.github.io/twentyseven) (no content yet)
