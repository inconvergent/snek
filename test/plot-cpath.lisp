#!/usr/bin/sbcl --script

(load "../src/load")


(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun make-line (xy s &optional a)
  (let ((res (list
               (vec:add xy (vec:vec 0d0 (- s)))
               (vec:add xy (vec:vec 6d0 0d0))
               (vec:add xy (vec:vec 0d0 s)))))
    (if a (rot res a xy) res)))

(defun make-line* (xy s &optional a)
  (let ((res (list
               (vec:add xy (vec:vec 0d0 (+ s)))
               (vec:add xy (vec:vec 6d0 0d0))
               (vec:add xy (vec:vec 0d0 (- s))))))
    (if a (rot res a xy) res)))


(defun make-box (xy s &optional a)
  (let ((res (list
               (vec:add xy (vec:vec (- s) (- s)))
               (vec:add xy (vec:vec s (- s)))
               (vec:add xy (vec:vec s s))
               (vec:add xy (vec:vec (- s) s)))))
    (if a (rot res a xy) res)))

(defun make-box* (xy s &optional a)
  (let ((res (list
               (vec:add xy (vec:vec (- s) (- s)))
               (vec:add xy (vec:vec (- s) s))
               (vec:add xy (vec:vec s s))
               (vec:add xy (vec:vec s  (- s))))))
    (if a (rot res a xy) res)))


(defun make-s (xy s &optional a)
  (let ((res (list
               (vec:add xy (vec:vec (- s) (- s)))
               (vec:add xy (vec:vec s (- s)))
               (vec:add xy (vec:vec s 0d0))
               (vec:add xy (vec:vec (- s) 0d0))
               (vec:add xy (vec:vec (- s) s))
               (vec:add xy (vec:vec s s)))))
    (if a (rot res a xy) res)))


(defun rot (v a xy)
  (loop for b in v collect (vec:rot b a :xy xy)))


(defun main (size fn)
  (let ((left 200d0)
        (right 800d0)
        (bs 150d0)
        (psvg (plot-svg:make* :width 1000d0
                              :height 1000d0
                              :stroke-width 1d0
                              :rep-scale 0.5d0)))
    (let (( res (make-generic-array) ))

      (loop for a in (math:linspace 15 0 (* 2d0 PI))
            and x in (math:linspace 15 60d0 940d0) do
        (array-push (list nil (make-box (vec:vec x 100d0) 10d0 a)) res)
        (array-push (list nil (make-box* (vec:vec x 200d0) 10d0 a)) res)

        (array-push (list nil (make-line (vec:vec x 490d0) 10d0 a)) res)
        (array-push (list nil (make-line* (vec:vec x 510d0) 10d0 a)) res)

        (array-push (list t (make-box (vec:vec x 300d0) 10d0 a)) res)
        (array-push (list t (make-box* (vec:vec x 400d0) 10d0 a)) res)

        (array-push (list t (make-s (vec:vec x 700d0) 20d0 (rnd:rnd* PI))) res)
        (array-push (list nil (make-s (vec:vec x 600d0) 20d0 (rnd:rnd* PI))) res))

      (array-push (list t (list
                             (vec:vec 400d0 900d0)
                             (vec:vec 300d0 900d0)
                             (vec:vec 300d0 970d0)))  res)
      (array-push (list nil (list
                              (vec:vec 700d0 900d0)
                              (vec:vec 500d0 900d0)
                              (vec:vec 500d0 970d0)))  res)
      (array-push (list t (list
                             (vec:vec 100d0 800d0)
                             (vec:vec 300d0 800d0)
                             (vec:vec 300d0 870d0)))  res)

      (array-push (list nil (list
                              (vec:vec 400d0 800d0)
                              (vec:vec 600d0 800d0)
                              (vec:vec 600d0 870d0)))  res)

      (array-push (list nil (list
                              (vec:vec 800d0 800d0)
                              (vec:vec 800d0 900d0))) res)

      (array-push (list nil (list
                              (vec:vec 850d0 800d0)
                              (vec:vec 850d0 850d0)
                              (vec:vec 850d0 900d0))) res)

      (array-push (list nil (list
                              (vec:vec 750d0 800d0)
                              (vec:vec 750d0 850d0)
                              (vec:vec 770d0 840d0)
                              (vec:vec 750d0 900d0))) res)

      (array-push (list nil (list
                              (vec:vec 700d0 800d0)
                              (vec:vec 700d0 850d0)
                              (vec:vec 700.1d0 840d0)
                              (vec:vec 700d0 900d0))) res)

      (array-push (list nil (list
                              (vec:vec 650d0 800d0)
                              (vec:vec 650d0 850d0)
                              (vec:vec 650d0 840d0)
                              (vec:vec 650d0 900d0))) res)


      (array-push (list nil (list
                              (vec:vec 900d0 900d0)
                              (vec:vec 900d0 800d0))) res)
      (array-push (list nil (rot (list
                                   (vec:vec 900d0 900d0)
                                   (vec:vec 900d0 800d0))
                                 (rnd:rnd*)
                                 (vec:vec 900d0 900d0))) res)

      ;180 flip
      (array-push (list nil (list
                              (vec:vec 20d0 900d0)
                              (vec:vec 70d0 900d0)
                              (vec:vec 70d0 970d0)
                              (vec:vec 90d0 850d0)
                              (vec:vec 120d0 850d0)

                              )) res)


      (loop for (c box) across res do
        (plot-svg:cpath psvg box :width 15d0 :closed c)))

    (plot-svg:save psvg fn)))

(time (main 1000 (second (cmd-args))))

