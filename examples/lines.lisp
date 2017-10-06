#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)

  (let ((mid (* 0.5d0 size))
        (repeat 15)
        (grains 3)
        (itt 1000)
        (sand (sandpaint:make size
                :active (list 0 0 0 0.01)
                :bg (list 1 1 1 1))))

    (loop for i in (math:linspace repeat 100 900)
          for j from 1 to repeat do
      (print-every j 2)
      (let ((snk (snek:make))
            (va (vec:vec 0d0 0d0))
            (vb (vec:vec 0d0 0d0))
            (p1 (vec:vec 100d0 i))
            (p2 (vec:vec 900d0 i)))

        (loop for s in (math:linspace itt 0.0 1.0) do
          (let ((v1 (snek:add-vert! snk (vec:on-line s p1 p2)))
                (v2 (snek:add-vert! snk (vec:add va (vec:on-line s p1 p2)))))

            (setf va (vec:add va (rnd:in-circ (* 0.7d0 j))))
            (setf vb (vec:add vb (rnd:in-circ (* 0.001d0 j))))

            (snek:with (snk)
              (snek:itr-verts (snk v)
                (snek:move-vert? v (vec:add (rnd:in-circ 0.1d0) vb)))
              (snek:join-verts? v1 v2))

            (snek:draw-edges snk sand grains)
            (snek:draw-verts snk sand)))))


    (sandpaint:pixel-hack sand)
    ;(sandpaint:chromatic-aberration sand (list mid mid) :s 100.0)
    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

