#!/usr/bin/sbcl --script

(load "../src/load.lisp")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init (snk rep rad)
  (loop for x in (math:linspace rep 200 800) for i from 0 do
    (loop for y in (math:linspace rep 200 800) for j from 0 do
      (let ((g (snek:add-grp! snk :type 'path :closed t)))
        (snek:add-polygon! snk (rnd:rndi 3 6) rad
                           :xy (vec:vec x y)
                           :g g)))))

(defun main (size fn)
  (let ((grains 4)
        (itt 10000)
        (noise 0.000000018d0)
        (rep 10)
        (rad 25d0)
        (snk (snek:make :max-verts 10000))
        (sand (sandpaint:make size
                :active (color:white 0.005)
                :bg (color:gray 0.1d0))))

    (init snk rep rad)

    (let ((grp-states (make-hash-table :test #'equal)))
      (snek:itr-grps (snk g)
        (setf (gethash g grp-states) (rnd:get-acc-circ-stp*)))

      (loop for i from 0 to itt do
        (print-every i 1000)

        (snek:with (snk)
          (snek:itr-grps (snk g)
            (let ((ns (funcall (gethash g grp-states) noise)))
              (snek:itr-verts (snk v :g g)
                (snek:move-vert? v
                  (vec:add ns
                    (rnd:in-circ 0.05d0)))))))

        (snek:itr-grps (snk g)
          (snek:draw-edges snk sand grains :g g))))

    (sandpaint:chromatic-aberration sand (vec:vec 500d0 500d0) :s 200d0)
    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

