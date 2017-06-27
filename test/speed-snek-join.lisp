#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-snek (n)
  (let ((snk (make-snek
               :max-verts 5000000
               :max-main-grp-edges 500000)))
    (nrep n (add-vert! snk (nrep 2 (rnd:rnd 100))))
    snk))


(defun main ()
  (let* ((itt 50000)
         (num 10000)
         (snk (init-snek num)))

    (loop for i from 0 below itt do
      (if (= (mod i 100) 0)
        (format t "itt ~a edges ~a ~%" i (length (get-edges snk))))

      (with-snek (snk)
        (join-verts? (rnd:rndi num) (rnd:rndi num)))

      ;(del-edge! snk (nrep 2 (rnd:rndi num)))
      )))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         ;:mode :cpu
                         ;:mode :alloc
                         :mode :time
                         :report :flat)
 (time (main)))

