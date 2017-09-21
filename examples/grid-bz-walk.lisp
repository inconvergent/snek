#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun mixed (x f)
  (declare (double-float x f))
  (+ (random (* f x)) (- x (* 2.0d0 (random x)))))


(defun get-grid (size edge ngrid)
  (loop for x in (math:linspace ngrid edge (- size edge)) collect
    (loop for y in (math:linspace ngrid edge (- size edge)) collect
      (vec:vec x y))))

;(defun get-grid (size edge ngrid)
;  (let ((a (- edge))
;        (b (+ size edge)))
;    (loop for x in (rnd:rndspace ngrid a b :order t) collect
;      (loop for y in (rnd:rndspace ngrid a b :order t) collect
;        (list x y)))))


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
          (math:on-line x
            (nth (mod m ngrid) (nth (mod n ngrid) grid))
            (nth (mod m* ngrid) (nth (mod n* ngrid) grid))))))))


(defun main (size fn)
  (let ((itt 1000000)
        (ngrid 7)
        (nwalkers 4)
        (noise 0.00001d0)
        (grains 10)
        (edge 60)
        (sand (sandpaint:make size
                :active (color:white 0.05)
                :bg (color:dark))))


    (let* ((grid (get-grid size edge 5))
           (walkers-a (math:nrep nwalkers (get-walker grid)))
           (walkers-b (math:nrep nwalkers (get-walker grid))))

      (loop for i from 0 below itt do
        (print-every i 100000)
        (sandpaint:set-rgba sand (color:hsv 0.51 1 1 0.05))
        (sandpaint:pix sand
          (bzspl:rndpos
            (bzspl:make (loop for w in walkers-a collect
                           (funcall w noise)) :closed t)
            grains))
        (sandpaint:set-rgba sand (color:hsv 0.91 1 1 0.05))
        (sandpaint:pix sand
          (bzspl:rndpos
            (bzspl:make (loop for w in walkers-b collect
                           (funcall w noise)) :closed t)
            grains))))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 2000 (second (cmd-args))))

