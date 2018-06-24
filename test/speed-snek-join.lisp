#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun init-snek (n)
  (let ((snk (snek:make :max-verts 5000000 :grp-size 100)))
    (math:nrep n (snek:add-vert! snk (vec:rep (rnd:rnd 100d0))))
    snk))


(defun main ()
  (let* ((itt 50000)
         (num 10000)
         (snk (init-snek num)))

    (loop for i from 0 below itt do
      (when (= (mod i 100) 0)
        (format t "itt ~a edges ~a ~%" i (length (snek:get-edges snk))))

      (snek:with (snk)
        (snek:join-verts? (rnd:rndi num) (rnd:rndi num)))
      ;(snek:del-edge! snk (nrep 2 (rnd:rndi num)))
      )))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         :mode :cpu
                         ;:mode :alloc
                         ;:mode :time
                         :report :graph)
 (time (main)))

