#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/text")
(load "../utils/text-sample")
(load "../utils/spline-script")
(load "../utils/state")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


;(vec:vec 1d0 (+ 1d0 (* 2d0 (rnd:norm))))
(defun scale ()
  (if (< (rnd:rnd) 0.15)
    (vec:vec 1d0 3.5d0)
    (vec:vec 1d0 1d0)))


;(defun shape (n bx by)
;  (rnd:nin-box n bx by))


(defun shape (n bx by)
  (math:vmult (rnd:nin-circ n 1d0) (vec:vec bx by)))


(defun draw (snk sand plt)
  (let ((state-gen (get-state-gen (lambda () (rnd:get-acc-circ-stp*))))
        (grains 20))
    (loop for p in (math:linspace 500 0 1)
          and i from 0 do
      (snek:with (snk)
        (snek:itr-all-verts (snk v)
          (snek:move-vert? v (rnd:in-circ 0.1d0))
          (snek:move-vert? v (funcall state-gen v 0.000008d0))))
      ;(sandpaint:set-rgba sand (color:hsv 0.55 (- 1.0 i) (- 1.0 i) 0.009))
      (snek:itr-grps (snk g)
        (aif (snek:get-grp-as-bzspl snk g)
          (progn
            (when (= i 200)
              (plot:path plt (bzspl:rndpos it (* 50 (bzspl::bzspl-n it))
                                           :order t)))
            (sandpaint:pix sand (bzspl:rndpos it
                                 (* (rnd:rndi (- grains 5) (+ grains 5))
                                    (bzspl::bzspl-n it))))))))))


(defun main (size fn)
  (let ((trbl (list 70d0 950d0 950d0 55d0))
        (bbox (vec:vec 20d0 35d0))
        (spacebox (vec:vec 14d0 30d0)) (sand (sandpaint:make size
                :active (color:black 0.009)
                :bg (color:white))))

    (labels
      ((get-bbox-fxn ()
        (let ((bbox (vec:mult bbox (scale))))
          (vec:with-xy ((vec:scale bbox 0.5d0) bx by)
            (lambda (n)
              (mapcar (lambda (v) (vec:rot v (* PI 0.2d0)))
                      (shape n bx by))))))
       (estimate-nc-ncn-fxn (bbox-fxn)
         (let* ((samples (funcall bbox-fxn 1000))
                (mid (math:mid samples))
                (ma (apply #'max (mapcar (lambda (a) (vec:dst a mid)) samples))))
           (if (< ma 30d0)
             (list 4 1)
             (list 7 1)))))

      (let ((alphabet (get-alphabet "abcdefghijklmnopqrstuvwxyz.,?-—:'"
                                    :bbox-fxn #'get-bbox-fxn
                                    :nc-ncn-fxn #'estimate-nc-ncn-fxn
                                    :sort-fxn (lambda () (if (< (rnd:rnd) 0.5) #'< #'>))
                                    ;:sort-fxn (lambda () #'>)
                                    :min-dst 16d0))
            (words (apply #'append (get-words* *snowman*)))
            (wind 0))

        (loop for k from 0 do
          (let ((snk (snek:make))
                (plt (plot:make size))
                (txt (subseq words wind)))
            (incf wind (do-write snk alphabet spacebox trbl txt))
            (draw snk sand plt)
            (plot:save plt (append-postfix fn (format nil "-plt-~3,'0d" k))))

          (sandpaint:pixel-hack sand)
          (sandpaint:save sand (append-postfix fn (format nil "-~3,'0d" k))
                          :gamma 1.5)
          (sandpaint:clear sand (color:white)))))))


(time (main 1000 (second (cmd-args))))

