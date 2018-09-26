#!/usr/bin/sbcl --script

(load "../src/load")


(defun sum-alter-velocity (velocities a num)
  (loop for i from 1 to (1- num) do
    (vec:sarr-set velocities i
                  (vec:add (rnd:in-circ a)
                           (vec:sarr-get velocities (1- i))))))


(defun make-lattice (n a b)
  (apply #'append
         (loop for x in (math:linspace n a b)
               collect (loop for y in (math:linspace n a b)
                             collect (vec:vec x y)))))


(defun main (size fn)

  (let ((snk (snek:make))
        (verts nil)
        (grains 4)
        (velocities (make-array 200 :initial-element 0d0
                                    :element-type 'double-float))
        (lattice (make-lattice 50 100d0 900d0))
        (noise 1.5d0)
        (itt 8000)
        (sand (sandpaint:make size
                :fg (pigment:black 0.08)
                :bg (pigment:white))))

    (setf verts (loop for i from 1 to 100
                      collect (snek:add-vert! snk (rnd:rndget lattice))))

    (loop for i from 0 to itt do
      (if (eql (mod i 100) 0) (format t "~a~%" i))
      (destructuring-bind (p1 p2)
        (list (rnd:rndget verts) (rnd:rndget verts))
        (if (< (vec:dst (snek:get-vert snk p1) (snek:get-vert snk p2)) 100.0)
          (progn (sum-alter-velocity velocities noise 100)
                 (snek:with (snk)
                   ; TODO mutate was used here. implement later.
                   (snek:add-edge? p1 p2)
                   (snek:itr-grp-verts (snk v)
                     (snek:move-vert? v (vec:sarr-get velocities v)))))))

        (snek:draw-edges snk sand grains))

     (sandpaint:pixel-hack sand)
     (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

