#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/grid")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))



(defun mixed (x f)
  (declare (double-float x f))
  (+ (random (* f x)) (- x (* 2.0d0 (random x)))))


(defun rnd-dir ()
  (nth (rnd:rndi 4)
       (list
         (list 0 -1)
         (list 0 1)
         (list -1 0)
         (list 1 0))))

(defmacro -swap (n m n* m*)
  (with-gensyms (h w)
    `(progn
      (setf ,n ,n*)
      (setf ,m ,m*)
      (destructuring-bind (,h ,w)
        (rnd-dir)
        (setf ,n* (+ ,n ,h))
        (setf ,m* (+ ,m ,w))))))


(defun get-walker (grid)
  (let ((ngrid (length grid)))
    (let ((x 0.d0)
          (n (rnd:rndi ngrid))
          (m (rnd:rndi ngrid)))
      (destructuring-bind (n* m*)
        (math:add (list n m) (rnd-dir))

        (lambda (noise)
          (incf x (mixed noise 0.2d0))
          (if (> x 1.0d0)
            (-swap n m n* m*))
          (if (< x 0.0d0)
            (-swap n* m* n m))
          (setf x (mod x 1.0d0))
          (vec:on-line x
            (nth (mod m ngrid) (nth (mod n ngrid) grid))
            (nth (mod m* ngrid) (nth (mod n* ngrid) grid))))))))


(defun main (size fn)
  (let ((itt 10000000)
        (ngrid 7)
        (nwalkers 2)
        (noise 0.00002d0)
        (grains 2)
        (edge 0d0)
        (sand (sandpaint:make size
                :active (color:white 0.05)
                :bg (color:dark))))

    (let* ((grid (get-grid size edge (rnd:rndi 5 11)))
           (walkers (math:nrep nwalkers (get-walker grid))))

      (loop for i from 0 below itt do
        (print-every i 100000)
        ;(sandpaint:circ sand
        ;  (loop for w in walkers collect
        ;    (funcall w (* ngrid 0.00000001d0)))
        ;  2.0
        ;  1)
        (sandpaint:stroke sand
          (loop for w in walkers collect
            (funcall w noise))
          grains)))

    (sandpaint:save sand fn :gamma 2.2)))

(time (main 2000 (second (cmd-args))))

