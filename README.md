# SNEK is Not an Acronym


## About

This library is specifically written to be useful for a broad range of ways in
which I create art using various generative algorithms.

![head](img/cells.lisp.png?raw=true "ex")

In short `snek` is four things:

1. A simple (graph) data structure for working with vertices and edges The
   structure is named `snek`; the name is explained below. This structure is
   combined with a programming pattern for applying changes to the structure.
   The pattern relies on `alterations`, see below.

2. A series of useful data structures and tools. E.g. a 2D vector `vec` and a
   package for generating different kinds of random numbers: `rnd`.


3. A tool for drawing things called `sandpaint`. `sandpaint` uses random
   sampling to draw it's primitives. This creates a fairly distinct and gritty
   look in many cases.

4. A tool for drawing svg files.

![head](img/spline-script.lisp.png?raw=true "ex")


### About the Name

A while back someone on twitter suggested that if Python 3 was named "snek" it
would avoid naming confusion. I found that amusing at the time, and picked snek
as the placeholder name for this project. I've been looking for a better name,
but I haven't found one yet.


## Alterations

The pattern depends on the concept of `alterations`. In short: an `alteration`
is a change that will be applied to the structure at the end of a given
context. `alterations` are further described in
https://inconvergent.net/2017/snek-is-not-an-acronym/.

I have also written about things related to `snek` at

  - https://inconvergent.net/2017/a-propensity-for-mistakes/ (indirectly about `snek`)
  - https://inconvergent.net/2017/a-method-for-mistakes/
  - https://inconvergent.net/2017/arbitrary-alterations/
  - https://inconvergent.net/2017/grains-of-sand/ (about "sandpainting", see
    `src/sandpaint.lisp`)

Here is and example of manipulating a `snek` instance called `snk` using
`alterations`. Alteration constructors are postfixed with `?`.

```lisp
; context start
(snek:with (snk)
  ; iterate vertices
  (snek:itr-verts (snk v)
    ; move alteration
    (snek:move-vert? v (rnd:in-circ 1d0))
    ; w will be an arbitrary
    ; vertex in snk
    (snek:with-rnd-vert (snk w)
      ; join v and w if they are closer than d
      (if (< (snek:edge-length snk (list v w)) d)
        ; join vertices alteration
        (snek:join-verts? v w))))
; context end
; alterations have been applied
```


### Custom alterations

You can define your own arbitrary alterations. There is an example of this in
`ex/custom-alt.lisp`. I have also written about it here:
https://inconvergent.net/2017/arbitrary-alterations/


## Examples

There are some examples included. All examples are in the `examples` folder.

If you don't provide a filename (with full or relative path) as the first
argument, the resulting file will be named `./tmp.png`.


## Usage

I use snek for most of the work that I post on twitter
(https://twitter.com/inconvergent). Both for generating raster images as well
as vector images for plotting.

![lines](img/lines.lisp.png?raw=true "ex")

![lines](img/grid-bz-walk.lisp.png?raw=true "ex")

Here are some plotted examples:

 - https://inconvergent.net/2017/spline-script-plots/
 - https://inconvergent.net/mechanical-plotter-drawings/
 - https://inconvergent.net/mechanical-plotter-drawings/3/
 - https://inconvergent.net/mechanical-plotter-drawings/5/


## Dependencies

This code requires `libpng-dev`, `Quicklisp`, `zpng`, `cl-svg` and `cl-png`.
The path to quicklisp must be set in `src/load`. `zpng`, `cl-svg` and `cl-png`
are automatically installed via `quicklisp`.

 - https://www.quicklisp.org/beta/
 - http://www.xach.com/lisp/zpng/


## Tests

There are some tests included, see the `test` folder.


## Stability, Changes and Versioning

This code is highly experimental on my part. It is likely to change with no
warning or explanation. I will keep a note of the version number in
`src/load.lisp`.


## Bugs

 - bzspl has lower sample density near the edges in some cases.


## Thanks

I would like to thank

  - https://twitter.com/RainerJoswig
  - https://twitter.com/paulg
  - https://twitter.com/jackrusher

Who have provided me with useful hints and code feedback.

