#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/state")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun roll-once (aa)
  (butlast (append (last aa) aa) 1))


(defun init-vertical (n size xy border)
  (let ((snk (snek:make :max-verts (* n n))))
    (loop for x in (math:linspace n border (- size border)) collect
      (snek:add-path! snk (math:rep (y (math:linspace n border (- size border)))
                            (vec:vec x y)) :g (snek:add-grp! snk)))
  snk))


(defun init-fan (n size xy)
  (let ((snk (snek:make :max-verts (* n n))))
    (loop for a in (math:linspace n pi (* 2d0 pi)) collect
      (snek:add-path! snk (math:rep (rad (math:linspace n 0 size))
                            (vec:add (vec:scale (vec:cos-sin a) rad) xy))
                      :g (snek:add-grp! snk)))
  snk))


;(defun init-spiral (snum n rad xy)
;  (let ((snk (snek:make :max-verts (* snum n))))
;    (loop for rot in (math:linspace snum 0d0 (* 2d0 pi) :end nil) collect
;      (snek:add-path! snk (math:rep (p (math:linspace n 0d0 10d0))
;                            (vec:on-spiral p rad :rot rot :xy xy))
;                      :g (snek:add-grp! snk)))
;  snk))



(defun main (size fn)
  (let ((itt 4000)
        ;(hgrains 15)
        (vgrains 10)
        (vnoise 0.0000001d0)
        (hnoise 0.000001d0)
        (dens 0.25d0)
        ;(snk (init-vertical 20 size (vec:vec (* 0.5d0 size)
        ;                            (math:dfloat (- size 100d0)))
        ;                         100d0))
        (snk (init-fan 40 1400 (vec:vec 500d0 1300d0)))
        ;(snk (init-spiral 20 20 800d0 (vec:vec 500d0 500d0)))
        (v-state-gen (get-state-gen
                     (lambda () (rnd:get-acc-circ-stp*))))
        (h-state-gen (get-state-gen
                     (lambda () (rnd:get-acc-lin-stp (rnd:rnd)))))
        (sand (sandpaint:make size
                :active (color:white 0.01)
                :bg (color:black))))

    (snek:with (snk)
      (snek:itr-all-verts (snk v)
        (snek:move-vert? v (rnd:in-circ 9d0))))

    (let ((grps (snek:get-all-grps snk)))
      (loop for i from 0 below itt do
        (print-every i 100)

        (snek:with (snk)
          (snek:itr-all-verts (snk v)
            (snek:move-vert? v (funcall v-state-gen v vnoise))
            (snek:move-vert? v (rnd:in-circ 0.07d0))))

        (loop for ga in grps and gb in (cdr grps) do
          (loop for p in (math:range 0 45) do
            (let ((m (funcall h-state-gen (list ga gb p) hnoise))
                  (bza (snek:get-grp-as-bzspl snk ga))
                  (bzb (snek:get-grp-as-bzspl snk gb)))
              (sandpaint:dens-stroke sand
                (list (bzspl:pos bza m) (bzspl:pos bzb m)) dens )
              (sandpaint:bzspl-stroke sand bza vgrains)
              (sandpaint:bzspl-stroke sand bzb vgrains))))))

    (sandpaint:save sand fn :gamma 1.1)))

(time (main 1000 (second (cmd-args))))

