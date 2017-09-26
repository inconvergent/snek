#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/state")
(load "../utils/grid")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)
  (let* ((ngrid 100)
         (snk (snek:make :max-verts 10000))
         (grid (get-grid size 50d0 ngrid))
         (state-gen (get-state-gen (lambda () (rnd:get-acc-circ-stp*))))
         (sand (sandpaint:make size
                 :active (color:black 0.05)
                 :bg (color:white))))

    (snek:add-verts! snk (flatten grid))

    (loop for i from 0 below (- ngrid 2) do
          (snek:add-path*! snk (mapcar (lambda (k) (+ i (if (< (rnd:rnd) 0.1) 2 0 )
                                                      (* k ngrid)) )
                                       (math:range 0 ngrid))
                           :g (snek:add-grp! snk)))


    (loop for i from 0 below 320 do
      (print-every i 100)

      (snek:with (snk)
        (snek:itr-all-verts (snk v)
          (snek:move-vert? v (funcall state-gen v 0.000009d0)))))

      (snek:itr-grps (snk g)
        (sandpaint:lin-path sand
          (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 1000 :order t)
          1d0 30))

    ;(let ((grps (snek:get-all-grps snk)))
    ;  (loop for a in grps and b in (cdr grps) do

    ;        (let ((ga (snek:get-grp-as-bzspl snk a))
    ;              (gb (snek:get-grp-as-bzspl snk b)))
    ;          (loop for k in (math:linspace 10000 0 1) do
    ;                (sandpaint:stroke sand (list (bzspl:pos ga k)
    ;                                             (bzspl:pos gb k))
    ;                                  10)))))

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

