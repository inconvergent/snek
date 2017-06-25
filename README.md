# SNEK is Not an Acronym


## About

`snek` is a simple data structure for working with vertices and edges. More
importantly it is a programming pattern for applying changes to the structure.
It is specifically written to be useful for a broad range of ways in which I
usually write generative algorithms for creating art.

![head](img/img.png?raw=true "head")


## About the Name

A while back someone on twitter suggested that if Python 3 was named "snek" it
would avoid naming confusion. I found that amusing at the time, and picked snek
as the placeholder name for this project. I've been looking for a better name,
but I haven't found one yet.

## Alterations

The pattern depends on the concept of `alterations`. In short: an `alteration`
is a change that will be applied to the structure at the end of a given
context. `alterations` are further described in
http://inconvergent.net/snek-is-not-an-acronym/.

I have also written about things related to `snek` at

  - http://inconvergent.net/a-propensity-for-mistakes/ (indirectly about `snek`)
  - http://inconvergent.net/a-method-for-mistakes/
  - http://inconvergent.net/arbitrary-alterations/
  - http://inconvergent.net/grains-of-sand/ (about "sandpainting", see `src/sandpaint.lisp`)

Here is and example of manipulating a `snek` instance called `snk` using
`alterations`. Alteration constructors are postfixed with ?.

```lisp
; context start
(with-snek (snk)
  ; iterate vertices
  (itr-verts (snk v)
    ; move alteration
    (move-vert? v (rnd:in-circ))
    ; w will be an arbitrary
    ; vertex in snk
    (with-rnd-vert (snk w)
      ; join v and w if they are closer than d
      (if (< (edge-length snk (list v w)) d)
        ; join vertices alteration
        (join-verts? v w))))
; context end
; alterations have been applied
```


### Custom alterations

You can define your own arbitrary alterations. There is an example of this in
`ex-custom-alt.lisp`. I have also written about it here:
http://inconvergent.net/arbitrary-alterations/


## Examples

There are some examples included. All examples are named like this: `ex-*.lisp`.

if you don't provide a result name as the first argument, then the resulting
file will be named `tmp.png`.

This includes a few samples of how to use the plotting functionality. This will
export a simple vector file and an image to indicate the result.


## Usage

`snek` is used in

  - http://moment.inconvergent.net/
  - https://twitter.com/sandpaintbot
  - https://twitter.com/scratchpaintbot
  - https://twitter.com/cronianrings


![lines](img/ex-lines.png?raw=true "lines")


## Dependencies

This code requires `Quicklisp` and `zpng`. The path to quicklisp must be set in
`src/load`. `zpng` is automatically installed via `quicklisp`.

 - http://www.xach.com/lisp/zpng/
 - https://www.quicklisp.org/beta/


## Tests

There are some tests included. Run them like this:

    ./test.lisp
    ./test-snek.lisp


## Stability, Changes and Versioning

This code is highly experimental on my part. It is likely to change with no
warning or explanation. I will keep a note of the version number in
`src/load.lisp`.


## On Use and Contributions

This code is a tool that I have written for my own use. I release it publicly
in case people find it useful. It is not however intended as a
collaboration/Open Source project. As such I am unlikely to accept PRs, reply
to issues, or take requests.


## Planned Changes

 - math utils as package?
 - snek as package?
 - Randomized order of alteration apply
 - Maintain list of singly-connected vertices?


## THANKS

I would like to thank

  - https://twitter.com/RainerJoswig
  - https://twitter.com/paulg
  - https://twitter.com/jackrusher

Who have provided me with useful hints and code feedback.

