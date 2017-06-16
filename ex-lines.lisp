#!/usr/bin/env sbcl --script

(load "src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)

  (let ((mid (half size))
        (repeat 15)
        (grains 3)
        (itt 1000)
        (sand (sandpaint:make size
                :active (list 0 0 0 0.01)
                :bg (list 1 1 1 1))))

    (loop for i in (linspace 100 900 repeat)
          for j from 1 to repeat do
      (print-every j 2)
      (let ((snk (make-snek))
            (va (list 0 0))
            (vb (list 0 0))
            (p1 (list 100 i))
            (p2 (list 900 i)))

        (loop for s in (linspace 0.0 1.0 itt) do
          (let ((v1 (add-vert! snk (on-line s p1 p2)))
                (v2 (add-vert! snk (add va (on-line s p1 p2)))))

            (setf va (add va (rnd:in-circ (* 0.7 j))))
            (setf vb (add vb (rnd:in-circ (* 0.001 j))))

            (with-snek (snk)
              (itr-verts (snk v)
                (move-vert? v (add (rnd:in-circ 0.1) vb)))
              (join-verts? v1 v2))

            (snek-draw-edges snk sand grains)
            (snek-draw-verts snk sand)))))


    (sandpaint:pixel-hack sand)
    ;(sandpaint:chromatic-aberration sand (list mid mid) :scale 100.0)
    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

