#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun get-dst (centroids cand)
  (first (sort (loop for c in centroids
                     and i from 0
                     collect (list i (vec:dst cand c)))
            #'< :key #'second)))

(defun get-pt (w size)
  (rnd:in-box w w :xy (vec:rep (* 0.5d0 size))))


(defun get-distributed-pts (centroids size w n)
  (let ((res (make-vec))
        (scale (sqrt (* 2d0 (expt size 2d0)))))
    (loop for k from 0 below n do
      (let ((pt (get-pt w size)))
        (destructuring-bind (i dst)
          (get-dst centroids pt)
          (if (> (expt (rnd:rnd) 10d0) (* 3d0 (/ dst scale)))
            (vector-push-extend pt res)))))
    (coerce res 'list)))


(defun draw-circ (sand colors circs)
  (loop for c in circs and color in colors do
    (sandpaint:set-rgba sand color)
    (sandpaint:circ sand (list c) 15d0 10000)))


(defun main (size fn)
  (let ((border 50d0)
        (grains 10)
        (centroids (math:nrep 20 (get-pt 450d0 size)))
        (colors (math:nrep 20 (color:hsv (rnd:rnd) 0.7 0.8 0.09))))

    (let ((sand (sandpaint:make size
                :active (color:black 0.08)
                :bg (color:white))))
      (sandpaint:set-rgba sand (color:black 0.009))
      (loop for pt in (get-distributed-pts centroids size 450d0 300000) do
        (destructuring-bind (i dst)
         (get-dst centroids pt)
         (sandpaint:circ sand (list pt) 3d0 1300)))

      (draw-circ sand colors centroids)

      (sandpaint:pixel-hack sand)
      (sandpaint:save sand (append-postfix fn "-1") :gamma 1.5))

    (let ((sand (sandpaint:make size
                :active (color:black 0.08)
                :bg (color:white))))
      (sandpaint:set-rgba sand (color:black 0.009))
      (loop for pt in (get-distributed-pts centroids size 450d0 300000) do
        (destructuring-bind (i dst)
         (get-dst centroids pt)
         (sandpaint:stroke sand (list (nth i centroids) pt) 700)))

      (draw-circ sand colors centroids)

      (sandpaint:pixel-hack sand)
      (sandpaint:save sand fn :gamma 1.5))

    ))


(time (main 1000 (second (cmd-args))))

