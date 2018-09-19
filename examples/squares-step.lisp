#!/usr/bin/sbcl --script

(load "../src/load")

(rnd:set-rnd-state 1)


(defun main (size fn)
  (let ((grains 220)
        (itt 20000)
        (noise 0.0000018d0)
        (rep 5)
        (rad 185d0)
        (snk (snek:make))
        (sand (sandpaint:make size
                :fg (pigment:white 0.005)
                :bg (pigment:gray 0.1d0))))

    (loop for x in (math:linspace rep 200d0 1800d0) for i from 0 do
      (loop for y in (math:linspace rep 200d0 1800d0) for j from 0 do
        (let ((g (snek:add-grp! snk :type 'path)))
          (snek:add-polygon! snk 4 rad
                             :xy (vec:vec x y)
                             :g g))))

    (let ((grp-states (make-hash-table :test #'equal))
          (c 1))
      (snek:itr-grps (snk g)
        (incf c)
        (setf (gethash g grp-states)
              (list (lin-path:make (snek:get-grp-verts snk :g g))
                    (rnd:get-acc-lin-stp* (rnd:rnd))
                    (rnd:get-acc-lin-stp* (rnd:rnd))
                    noise)))

      ;(sandpaint:stroke sand (list (vec:vec 100d0) (vec:vec 900d0 100d0) ) 100000)

      (loop for i from 0 to itt do
        (print-every i 1000)

        ;(snek:with (snk)
        ;  (snek:itr-grps (snk g)
        ;    (snek:itr-grp-verts (snk v :g g)
        ;      (snek:move-vert? v (rnd:in-circ 0.3d0) :rel t))))

        (snek:itr-grps (snk g :collect nil)
          (destructuring-bind (path lina linb ns) (gethash g grp-states)
            ;(lin-path:move path (get-grp-vert-vals snk :g g) :rel nil) ;closed must be set in constr.
            (sandpaint:stroke sand
              (lin-path:pos* path (list (funcall linb ns)
                                        (funcall lina ns)))
              grains)))))

    (sandpaint:chromatic-aberration sand :s 50d0)
    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn)))


(time (main 2000 (second (cmd-args))))

