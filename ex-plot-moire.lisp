#!/usr/bin/sbcl --script

(load "src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defun make-stepper (ll)
  (destructuring-bind (a1 a2 b1 b2)
    ll
    (lambda (p) (list (on-line p a1 a2)
                      (on-line p b1 b2)))))

(defun perp (v)
  (destructuring-bind (a b)
    v
    (list b (- a))))

(defun box (xy vec)
  (list
    (add (sub xy vec) (perp vec))
    (add (add xy vec) (perp vec))
    (sub (sub xy vec) (perp vec))
    (sub (add xy vec) (perp vec))))


(defun main (size fn)
  (let ((stipples 1)
        (stipple-len 5)
        (rad 50)
        (rep 5)
        (box-rep 20)
        (itt 12000)
        (noise-inc 0.001d0)
        (left 180)
        (right 820)
        (noise 0.0d0)
        (angle 0.0d0)
        (plt (plot:make size)))

    (let ((stepper (make-stepper (box (list 500 499) (list 0 480)))))
      (loop for s in (linspace 0.0 1.0 192) do
        (plot:path plt (funcall stepper s))))


    (loop
      for xs in (linspace 0.0 1.0 rep)
      for x in (linspace left right rep)
      do
      (loop
        for ys in (linspace 0.0 1.0 rep)
        for y in (linspace left right rep)
        do
          (let ((stepper (make-stepper
                           (box (list x y)
                                (scale
                                  (cos-sin (+ (* pi 0.5) angle))
                                  rad)))))
            (loop for s in (linspace 0.0 1.0 box-rep :end nil) do
              (plot:path plt (funcall stepper s))))
          (incf angle (rnd 0.01d0))
        ))

      (plot:save plt fn)))


(time (main 1000 (second (cmd-args))))

