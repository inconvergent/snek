#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/text")
(load "../utils/text-sample")
(load "../utils/spline-script")
(load "../utils/state")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun scale ()
  (if (< (rnd:rnd) 0.15)
    (vec:vec 1.3d0 4.5d0)
    (vec:vec 1d0 1d0)))


(defun shape (n bx by)
  ;(rnd:nin-box n bx by)
  (vec:lmult* (rnd:nin-circ n 1d0) (vec:vec bx by)))


(defun draw-pix (sand bzs grains)
  (sandpaint:bzspl-stroke sand bzs (* grains (bzspl::bzspl-n bzs))))


(defun draw-varying-pix (sand bzs grains)
  (sandpaint:pix sand (bzspl:pos* bzs
    (mapcar (lambda (x) (expt x 0.6d0))
      (rnd:rndspace (* grains (bzspl::bzspl-n bzs)) 0d0 1d0)))))


(defun draw (snk sand plt)
  (let ((state-gen (get-state-gen (lambda () (rnd:get-acc-circ-stp*))))
        (grains 15))
    (loop for p in (math:linspace 500 0 1)
          and i from 0 do
      (snek:with (snk)
        (snek:itr-all-verts (snk v)
          (snek:move-vert? v (vec:scale (vec:sin-cos 0.1d0) 0.005d0))
          (snek:move-vert? v (rnd:in-circ 0.1d0))
          (snek:move-vert? v (funcall state-gen v 0.000008d0))))
      ;(sandpaint:set-rgba sand (color:hsv 0.55 (- 1.0 i) (- 1.0 i) 0.009))
      (snek:itr-grps (snk g)
        (aif (snek:get-grp-as-bzspl snk g)
          (progn
            (when (= i 200)
              (plot:path plt (bzspl:rndpos it (* 50 (bzspl::bzspl-n it))
                                           :order t)))
            (draw-pix sand it grains)
            ;(draw-varying-pix sand it grains)
            ))))))


(defun main (size fn)
  (let ((trbl (list 70d0 950d0 950d0 55d0))
        (bbox (vec:vec 15d0 20d0))
        (spacebox (vec:vec 10d0 25d0))
        (sand (sandpaint:make size
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
                (mid (vec:lmid samples))
                (area (apply #'max (mapcar (lambda (a) (vec:dst a mid)) samples))))
           (if (< area 30d0)
             (list area 4 1)
             (list area 7 1)))))

      (let ((alphabet (show-alphabet (get-alphabet
                        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.,?-—:'"
                        :get-bbox-fxn #'get-bbox-fxn
                        :nc-ncn-fxn #'estimate-nc-ncn-fxn
                        ;:sort-fxn (lambda () (if (< (rnd:rnd) 0.5) #'< #'>))
                        :sort-fxn (lambda () #'>)
                        :min-dst 5d0)) )
            (words (remove-if (lambda (x) (= 0 (second x)))
                              (apply #'append (get-words* *alice*))) )
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

