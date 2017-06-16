#!/usr/bin/env sbcl --script

(load "src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-snek (n m s xy)
  ; a group (grp) is a collection of vertices.

  ; make m grps with n verts in each grp.
  (let ((snk (make-snek)))
    (mapcar
      (lambda (g) (rep (p (linspace 0 1 n))
                       ; vertices are evenly spaced on a circle centered at xy
                       (add-vert! snk (on-circ p 600 :xy xy) :g g)))
      (nrep m (add-grp! snk)))
  snk))


(defun get-walkers-state-gen (snk)
  (let ((walkers (make-hash-table :test #'equal)))
    ; iterate all verts in all grps
    (itr-grps (snk g)
      (itr-verts (snk v :g g)
        (setf
          (gethash v walkers)
          ; associate a random 2d walker with this vert.
          ; these walkers have a randomly changing
          ; acceleration, which gives intersting behaviour
          (rnd:get-acc-circ-stp*))))

    ; function that generates a move vert alteration based on the
    ; state of the corresponding walker
    (lambda (v noise)
      (move-vert? v (funcall (gethash v walkers) noise)))))


(defun main (size fn)
  (let ((itt 1000000)
        (noise 0.000000000007d0)
        (grains 10)
        (snk (init-snek 40 1
               (half size)
               (nrep 2 (half size))))
        (sand (sandpaint:make size
                :active (color:white 0.05)
                :bg (color:dark)))
        (mut (make-mutate :noise 100 :prob 0.000005)))

      (let ((state-gen (get-walkers-state-gen snk)))

        (loop for i from 0 below itt
          do
            (print-every i 100000)

            (with-snek (snk)
              ; try to enable this to see what happens
              ;(mutate (mut)
                (itr-grps (snk g)
                  (itr-verts (snk v :g g)
                    ; get an alteration for vert v
                    (funcall state-gen v noise))));)

            ;(sandpaint:set-rgba sand (color:hsv 0.51 1 1 0.05))
            (itr-grps (snk g)
              ; draw random dots along the bezier spline.
              (sandpaint:pix
                sand
                (bzspl:rndpos
                  ; make a bezier spline through the verts of g
                  (bzspl:make (get-grp-vert-vals snk :g g) :closed t)
                  grains)))))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 2000 (second (cmd-args))))

