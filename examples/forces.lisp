#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)

  (let ((mid (half size))
        (itt 500)
        (snk (snek:make))
        (grains 100)
        (rad 1d0)
        (sand (sandpaint:make size
                :active (list 1.0 1.0 1.0 0.024)
                :bg (list 0.1 0.1 0.1 1.0))))

        (math:nrep 3000 (snek:add-vert! snk (rnd:on-line (vec:vec 200d0 200d0)
                                                         (vec:vec 800d0 800d0))))
        (math:nrep 3000 (snek:add-vert! snk (rnd:on-line (vec:vec 800d0 200d0)
                                                         (vec:vec 200d0 800d0))))

      (loop for k from 1 to itt do
        (print-every k 100)

        (snek:with (snk :zwidth 10.0d0)
          (snek:itr-all-verts (snk v)
            (snek:move-vert? v (rnd:in-circ 1d0))
            (map 'list
              (lambda (u) (snek:force? snk u v -0.1d0))
              (snek:verts-in-rad snk (snek:get-vert snk v) 10.0d0)))))
      (snek:draw-circ snk sand rad grains)

   (sandpaint:pixel-hack sand)
   (sandpaint:save sand fn :gamma 1.3)))


(time (main 1000 (second (cmd-args))))

