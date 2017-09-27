#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/state")
(load "../utils/grid")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun place-grid (snk small large mid)
  (let* ((df (math:dfloat (half (- large small))))
         (a (rnd:in-box df df :xy mid))
         (b (rnd:in-box df df :xy mid))
         (angle (rnd:rnd (* 2d0 PI)))
         (num (floor (* 0.1d0 (vec:dst a b)))))

    (loop for path in (get-stroke-grid num 100 2000d0 angle a b) do
      (snek:add-path! snk path :g (snek:add-grp! snk)))))


(defun main (size fn)
  (let* ((ngrid 100)
         (snk (snek:make :max-verts 100000))
         (grid )
         (state-gen (get-state-gen (lambda () (rnd:get-circ-stp*))))
         (sand (sandpaint:make size
                 :active (color:black 0.05)
                 :bg (color:white))))

    (loop for i from 0 below 10 do
      (place-grid snk 0 size (vec:vec 500d0 500d0)))

    (loop for i from 0 below 50 do
      (print-every i 100)
      (snek:with (snk)
        (snek:itr-all-verts (snk v)
          ;(snek:move-vert? v (funcall state-gen v 0.0009d0))
          (snek:move-vert? v (rnd:in-circ 0.5d0))))
      ;(snek:itr-grps (snk g)
      ;  (sandpaint:pix sand
      ;    (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 500)))

      )

      (snek:itr-grps (snk g)
        (sandpaint:lin-path sand
          (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 10000 :order t)
          1d0 10))

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

