#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-snek (n)
  (let ((snk (snek:make
               :max-verts 10000
               :grp-size 100)))
    (math:nrep n (snek:add-vert!
                   snk
                   (vec:vec (rnd:rnd 100d0)
                            (rnd:rnd 100d0))))
    snk))


(defun main ()
  (let* ((itt 5000)
         (num 10000)
         (snk (init-snek num)))

    (loop for i from 0 below itt do
      (if (= (mod i 100) 0)
        (format t "itt ~a edges ~a ~%" i (length (snek:get-edges snk))))

      (snek:with (snk)
        (snek:itr-all-verts (snk v)
                            ;nil
          ;(snek:move-vert? v (rnd:in-circ 1d0))
          ;(snek:move-vert? v 1d0 2d0)
          ;(math:2d 1.0d0 1.0d0)
          (rnd:in-circ 1d0)
          )))))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         ;:mode :cpu
                         ;:mode :alloc
                         :mode :time
                         :report :graph)
 (time (main)))

