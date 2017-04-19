# SNEK is Not an Acronym

![head](img/img.png?raw=true "head")


## About

`snek` is a simple data structure for working with vertices and edges. More
importantly it is a programming pattern for applying changes to the structure.
It is specifically written to be useful for a broad range of ways in which I
usually write generative algorithms for creating art.

The pattern depends on the concept of `alterations`. In short: an `alteration`
is a change that will be applied to the structure at the end of a given
context. `alterations` are further described in
http://inconvergent.net/snek-is-not-an-acronym/.

Here is and example of manipulating a `snek` instance called `snk` using
`alterations`:

```lisp
; context start
(with-snek (snk)
  ; iterate vertices
  (itr-verts (snk v)
    ; move alteration
    (move-vert v (rnd-in-circ))
    ; w will be an arbitrary
    ; vertex in snk
    (with-rnd-vert (snk w)
      ; join v and w if they are closer than d
      (if (< (edge-length snk (list v w)) d)
        ; join vertices alteration
        (join-verts v w))))
; context end
; alterations have been applied
```

There are some examples included. They can be executed like this:

    ./ex-slope.lisp res/slope
    ./ex-lines.lisp res/lines
    ./ex-grp.lisp res/grp

There is also and example of how to use the plotting functionality. This will
export a simple vector file and an image to indicate the result

    ./ex-plot-perspective.lisp res/perspective

Notice that the resulting image will end up in `res`.

`snek` is used in

  - http://moment.inconvergent.net/
  - https://twitter.com/sandpaintbot
  - https://twitter.com/scratchpaintbot

![lines](img/ex-lines.png?raw=true "lines")


## Dependencies

This code requires `Quicklisp` and `zpng`. The path to quicklisp must be set in
`src/load`. `zpng` is automatically installed via `quicklisp`.

 - http://www.xach.com/lisp/zpng/
 - https://www.quicklisp.org/beta/


## Run tests

There are some tests included. Run them like this:

    ./test.lisp


## Stability, Changes and Versioning

This code is highly experimental on my part. It is likely to change with no
warning or explanation. I will keep a note of the version number in `VERSION`.


## On Use and Contributions

This code is a tool that I have written for my own use. I release it publicly
in case people find it useful. It is not however intended as a
collaboration/Open Source project. As such I am unlikely to accept PRs, reply
to issues, or take requests.


## Wanted Changes

 - math utils as package?
 - snek as package?
 - Randomized order of alteration apply
 - Maintain list of singly-connected vertices?

