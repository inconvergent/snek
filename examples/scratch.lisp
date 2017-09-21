#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun sum-alter-velocity (velocities a num)
  (loop for i from 1 to (1- num) do
    (vec:arr-set
      velocities i
      (vec:add
        (rnd:in-circ a)
        (vec:arr-get velocities (1- i))))))


(defun make-lattice (a b n)
  (apply
    #'append
      (loop for x in (math:linspace n a b)
        collect (loop for y in (math:linspace n a b)
          collect (vec:vec x y)))))


(defun main (size fn)

  (let ((snk (snek:make))
        (verts nil)
        (grains 4)
        (velocities (make-dfloat-array 100))
        (lattice (make-lattice 100 900 50))
        (noise 1.5d0)
        (itt 8000)
        (mut (snek:make-mutate))
        (sand (sandpaint:make size
                 :active (list 0.0 0.0 0.0 0.08)
                 :bg (list 1.0 1.0 1.0 1.0))))

    (setf verts (loop for i from 1 to 100
      collect (snek:add-vert! snk (rnd:lget lattice))))

    (loop for i from 0 to itt do
      (if (eql (mod i 100) 0) (format t "~a~%" i))
      (destructuring-bind (p1 p2)
        (list (rnd:lget verts) (rnd:lget verts))
        (if (< (vec:dst (snek:get-vert snk p1) (snek:get-vert snk p2)) 100.0)
          (progn
            (sum-alter-velocity velocities noise 100)
            (snek:with (snk)
              (snek:mutate (mut)
                (snek:join-verts? p1 p2)
                (snek:itr-verts (snk v)
                  (snek:move-vert? v (vec:arr-get velocities v))))))))

        (snek:draw-edges snk sand grains))

     (sandpaint:pixel-hack sand)
     (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

