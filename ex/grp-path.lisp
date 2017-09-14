#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init (snk rep rad)
  (loop for x in (math:linspace rep 200 800) for i from 0 do
    (loop for y in (math:linspace rep 200 800) for j from 0 do
      (let ((g (snek:add-grp! snk :type 'path :closed t)))
        (snek:add-path! snk
                        (rnd:nin-box 8 rad rad :xy (vec:vec x y))
                        :closed t :g g)
        ;(add-verts
        ;  snk
        ;  (rnd:nin-box 7 rad rad :xy (list x y))
        ;  :g g)
        ))))

(defun main (size fn)
  (let ((grains 15)
        (itt 30000)
        (noise 0.0000002d0)
        (rep 5)
        (rad 85d0)
        (snk (snek:make :max-verts 10000))
        (sand (sandpaint:make size
                :active (color:black 0.005)
                :bg (color:white))))

    (init snk rep rad)

    (let ((grp-states (make-hash-table :test #'equal)))
      (snek:itr-grps (snk g)
        (setf (gethash g grp-states)
              (list
                (lin-path:make (snek:get-grp-verts snk :g g))
                (rnd:get-acc-lin-stp* (rnd:rnd))
                (rnd:get-acc-lin-stp* (rnd:rnd)))))

      (loop for i from 0 to itt do
        (print-every i 1000)
        (snek:itr-grps (snk g)
          (destructuring-bind (path lina linb)
            (gethash g grp-states)
            (sandpaint:stroke sand
              (lin-path:pos* path (list (funcall linb noise)
                                        (funcall lina noise)))
              grains))))

    ;(sandpaint:chromatic-aberration sand (list 500 500) :s 200.0)
    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn))))


(time (main 1000 (second (cmd-args))))

