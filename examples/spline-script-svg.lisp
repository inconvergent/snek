#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/text")
(load "../utils/text-sample")
(load "../utils/spline-script")
(load "../utils/state")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defvar *alphabet* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.,?-—:'")


(defun scale ()
  (if (< (rnd:rnd) 0.15)
    (vec:vec 1.1d0 3.9d0)
    (vec:vec 1d0 1d0)))


(defun swap (l)
  (let* ((ll (loop for k in l collect k))
         (n (length l))
         (a (rnd:rndi n))
         (b (rnd:rndi n)))
    (setf (nth a ll) (nth b l)
          (nth b ll) (nth a l))
    ll))


(defun shape (n bx by)
  ;(rnd:nin-box n bx by)
  (vec:lmult* (rnd:nin-circ n 1d0) (vec:vec bx by)))


(defun draw (snk psvg)
  (let ((state-gen (get-walker-state-gen (lambda () (rnd:get-acc-circ-stp*))))
        (drift (vec:scale (vec:sin-cos -0.1d0) 0.009d0)))
    (loop for p in (math:linspace 200 0d0 1d0)
          and i from 0 do
      (snek:with (snk)
        (snek:itr-all-verts (snk v)
          ;(snek:move-vert? v (funcall state-gen v 0.000008d0))
          (snek:move-vert? v drift)
          (snek:move-vert? v (rnd:in-circ 0.1d0)))))
    (snek:itr-grps (snk g :collect nil)
      (let ((pts (snek:get-grp-verts snk :g g)))
        (when (> (length pts) 2)
          (plot-svg:bzspl psvg pts))))))


(defun main (size fn)
  (let ((trbl (list 70d0 950d0 950d0 55d0))
        (bbox (vec:vec 20d0 25d0))
        (spacebox (vec:vec 10d0 25d0)))

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
             (list area 7 1))))

       (sort-centroids-fxn ()
         (let ((f (if (< (rnd:rnd)) #'< #'>))
               (rot (rnd:rnd PII)))
           (lambda (centroids)
             (let ((res (sort (loop for c in centroids collect
                          (list (+ rot (apply #'atan (reverse (vec:tolist c)))) c))
                          f :key #'first)))
               (if (< (rnd:rnd) 0.6)
                 (swap res)
                 res))))))

      (let ((alphabet (show-alphabet (get-alphabet *alphabet*
                        :get-bbox-fxn #'get-bbox-fxn
                        :nc-ncn-fxn #'estimate-nc-ncn-fxn
                        :sort-fxn #'sort-centroids-fxn
                        :min-dst 5d0)))
            (words (remove-if (lambda (x) (= 0 (second x)))
                              (apply #'append (get-words* *alice*))) )
            (wind 0))

        (block draw-loop
          (loop for k from 0 do
            (let ((snk (snek:make))
                  (psvg (plot-svg:make :layout 'plot-svg:a4-landscape))
                  (txt (subseq words wind)))
              (incf wind (aif (do-write snk alphabet spacebox trbl txt)
                              it
                              (return-from draw-loop)))
              (draw snk psvg)
              (plot-svg:save psvg (append-postfix fn (format nil "-~3,'0d" k))))))))))


(time (main 1000 (second (cmd-args))))

