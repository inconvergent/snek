#!/usr/bin/env sbcl --script

(load "src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

; note that this does not use the snek structure.


(defun main (size fn)
  (let ((rad 0.07)
        (rep 5)
        (box-rep 10)
        (left 150)
        (right 850)
        (plt (plot:make size)))

    (let ((ddd (make-perspective-transform
                 (rnd:in-box 500 500 :xy (list -500 500))
                 (rnd:in-box 500 500 :xy (list 1500 500))
                 (rnd:in-box 500 500 :xy (list 500 1500)))))
      (loop for x in (linspace left right rep) do
        (loop for y in (linspace left right rep) do
          (let ((points (funcall ddd
                          (list x y) rad rad (half rad) (half rad))))
            (let ((top (subseq points 0 5))
                  (bottom (subseq points 5 10)))
              (loop for s in (linspace 0.0 1.0 box-rep) do
                (plot:path plt
                   (loop for a in top
                     for b in bottom
                     collect
                     (on-line s a b)))))))))

      (plot:save plt fn)))


(time (main 1000 (second (cmd-args))))

