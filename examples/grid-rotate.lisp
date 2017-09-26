#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/state")
(load "../utils/grid")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)
  (let* ((ngrid 100)
         (snk (snek:make :max-verts 100000))
         (grid )
         (state-gen (get-state-gen (lambda () (rnd:get-circ-stp*))))
         (sand (sandpaint:make size
                 :active (color:black 0.05)
                 :bg (color:white))))

    (loop for path in (get-stroke-grid
                        100 100 2000d0 (* PI 0.5d0)
                        (vec:vec 0d0 0d0) (vec:vec 1000d0 0d0)) do
          (snek:add-path! snk path :g (snek:add-grp! snk)))

    (loop for path in (get-stroke-grid
                        100 200 2000d0 (* PI 1.2d0)
                        (vec:vec 1000d0 0d0) (vec:vec 1000d0 2000d0)) do
          (snek:add-path! snk path :g (snek:add-grp! snk)))

    (loop for i from 0 below 450 do
      (print-every i 100)
      (snek:with (snk)
        (snek:itr-all-verts (snk v)
          ;(snek:move-vert? v (funcall state-gen v 0.0009d0))
          (snek:move-vert? v (rnd:in-circ 0.5d0))))
      (snek:itr-grps (snk g)
        (sandpaint:pix sand
          (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 500))))

      ;(snek:itr-grps (snk g)
      ;  (sandpaint:lin-path sand
      ;    (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 10000 :order t)
      ;    1d0 30))

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

