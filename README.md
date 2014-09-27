Twentyseven
===========

Rubik's cube solver in Haskell

Inspired by Herbert Kociemba's
*Cube Explorer*.

This project builds two executables.

`tstables [directory]` precomputes some move tables and saves two binary files
`phase1` and `phase2` in the given directory,
or in `$HOME/.tseven/` if there is no argument.

`twophase [directory]` uses those files to initialize, in about 10s,
and waits for an input. The input must be a string of
`u`, `l`, `f`, `r`, `b`, `d`, or their upper case counterparts,
terminated by a newline (no spaces);
that string specifies a sequence of moves to scramble the cube.
It then replies with a sequence of moves which solve the cube
(without cheating...).

Each letter corresponds to a clockwise quarter turn of the given face
(up, left, front, right, back, down).
The orientation is determined when looking directly at the turning face.
A half turn (e.g., `U2`) is equivalent to a sequence of two quarter turns,
and a counterclockwise quarter turn (e.g., `U'`) to a sequence of three
clockwise.

The two-phase algorithm finds solutions with a suboptimal number of moves,
but runs rather quickly.

Example:

    $ tstables
    Phase 1
    Phase 2
    $ twophase
    Ready.
    > udddlrrrbfffuddd
    U  D  F  B  L  R  U2 R2 F2 R2 U2 L2 B2 U' D' B2
    > q
    $

To do:
- Better UI
- Visual input
- Optimal solver
- Benchmarks
- ...

[Project page](https://lysxia.github.io/twentyseven) (no content yet)

