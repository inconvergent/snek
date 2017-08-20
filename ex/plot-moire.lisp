#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defun make-stepper (ll)
  (destructuring-bind (a1 a2 b1 b2)
    ll
    (lambda (p) (list (math:on-line p a1 a2)
                      (math:on-line p b1 b2)))))

(defun box (xy v)
  (list
    (vec:add (vec:sub xy v) (vec:perp v))
    (vec:add (vec:add xy v) (vec:perp v))
    (vec:sub (vec:sub xy v) (vec:perp v))
    (vec:sub (vec:add xy v) (vec:perp v))))


(defun main (size fn)
  (let ((stipples 1)
        (stipple-len 5)
        (rad 50d0)
        (rep 5)
        (box-rep 20)
        (itt 12000)
        (noise-inc 0.001d0)
        (left 180d0)
        (right 820d0)
        (noise 0.0d0)
        (angle 0.0d0)
        (plt (plot:make size)))

    (let ((stepper (make-stepper (box (vec:vec 500d0 499d0) (vec:vec 0d0 480d0)))))
      (loop for s in (math:linspace 0.0 1.0 192) do
        (plot:path plt (funcall stepper s))))


    (loop for xs in (math:linspace 0.0 1.0 rep)
          for x in (math:linspace left right rep) do
      (loop for ys in (math:linspace 0.0 1.0 rep)
            for y in (math:linspace left right rep) do
        (let ((stepper (make-stepper
                         (box (vec:vec x y)
                              (vec:scale
                                (vec:cos-sin (+ (* pi 0.5) angle))
                                rad)))))
          (loop for s in (math:linspace 0.0 1.0 box-rep :end nil) do
            (plot:path plt (funcall stepper s))))
        (incf angle (rnd:rnd 0.01d0))))

      (plot:save plt fn)))


(time (main 1000 (second (cmd-args))))

