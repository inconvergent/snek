#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)

  (let ((p1 (rnd:in-circ 100d0 :xy (vec:vec 800d0 200d0)))
        (p2 (rnd:in-circ 100d0 :xy (vec:vec 200d0 800d0))))
    (let ((va (vec:add
                (vec:scale
                   (vec:norm
                     (vec:mult (vec:flip (vec:sub p1 p2)) (vec:vec -1d0 1d0)))
                   40d0)
                 (rnd:on-circ 20d0 :xy (vec:zero))))
         (repeat 10)
         (noise (rnd:rnd 4d0))
         (grains 70)
         (itt 6000)
         (sand (sandpaint:make size
                 :active (list 0 0 0 0.01)
                 :bg (list 1 1 1 1))))

      (loop for j from 1 to repeat
        do
          (print-every j 2)
          (let ((snk (snek:make)))
            (setf va (vec:add va (rnd:in-circ noise :xy (vec:zero))))

            (loop for k from 1 to itt
              do
                (snek:with (snk)
                  (snek:add-vert? (rnd:on-line p1 p2))
                  (snek:with-rnd-vert (snk v)
                    (snek:append-edge? v va))
                  (snek:with-rnd-vert (snk v)
                    (snek:with-rnd-vert (snk w)
                      (snek:join-verts? w v)))))
              (snek:draw-edges snk sand grains)))


   (sandpaint:pixel-hack sand)
   (sandpaint:save sand fn))))


(time (main 1000 (second (cmd-args))))

