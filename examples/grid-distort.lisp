#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/state")
(load "../utils/grid")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defun up-down (p n)
  (if (< (rnd:rnd) p)
    (* n (expt -1 (rnd:rndi 2)))
    0))


(defun main (size fn)
  (let* ((ngrid 80)
         (snk (snek:make :max-verts 10000))
         (grid (get-grid size 50d0 ngrid))
         (state-gen (get-state-gen (lambda () (rnd:get-acc-circ-stp*))))
         (sand (sandpaint:make size
                 :active (color:white 0.05)
                 :bg (color:dark))))

    (snek:add-verts! snk (flatten grid))

    (loop for i from 3 below (- ngrid 3) do
          (snek:add-path*! snk (mapcar (lambda (k)
                                         (+ i (up-down 0.1 1) (* k ngrid)))
                                       (math:range 0 ngrid))
                           :g (snek:add-grp! snk)))

    (loop for i from (/ ngrid 2) below (- ngrid 3) do
          (snek:add-path*! snk (mapcar (lambda (k)
                                         (+ k (up-down 0.1 ngrid) (* ngrid i)))
                                       (math:range 0 ngrid))
                           :g (snek:add-grp! snk)))

    (loop for i from 0 below 500 do
      (print-every i 100)

      (snek:with (snk)
        (snek:itr-all-verts (snk v)
          (snek:move-vert? v (funcall state-gen v 0.000009d0))
          ;(snek:move-vert? v (rnd:in-circ 0.4d0))

          ))

      (snek:itr-grps (snk g)
        (sandpaint:pix sand
          (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 100)))

      )

      (snek:itr-grps (snk g)
        (sandpaint:pix sand
          (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 50000)))

      ;(snek:itr-grps (snk g)
      ;  (sandpaint:lin-path sand
      ;    (bzspl:rndpos (snek:get-grp-as-bzspl snk g) 1000 :order t)
      ;    1.2d0 40))

      ;(sandpaint:set-rgba sand (color:hsv 0.5 0.8 0.8 0.05))
      ;(sandpaint:circ sand (snek:get-all-verts snk) 2d0 300)


    ;(let ((grps (snek:get-all-grps snk)))
    ;  (loop for a in grps and b in (cdr grps) do

    ;        (let ((ga (snek:get-grp-as-bzspl snk a))
    ;              (gb (snek:get-grp-as-bzspl snk b)))
    ;          (loop for k in (math:linspace 10000 0 1) do
    ;                (sandpaint:stroke sand (list (bzspl:pos ga k)
    ;                                             (bzspl:pos gb k))
    ;                                  10)))))

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn :gamma 1.5d0)))


(time (main 1000 (second (cmd-args))))

