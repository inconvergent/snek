#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)


(defun init-snek (n)
  (rnd:set-rnd-state 1)
  (let ((snk (snek:make :max-verts 500000 :grp-size 100)))
    (math:nrep n (snek:add-vert! snk (vec:rep (rnd:rnd 100d0))))
    snk))


(defun main ()
  (let* ((itt 1000000)
         (num 10000)
         (snk (init-snek num)))

    (time (loop for i from 0 below itt do
      (when (= (mod i 100000) 0)
        (format t "itt ~a edges ~a ~%" i (length (snek:get-edges snk))))

      (snek:with (snk)
        (snek:add-edge? (rnd:rndi num) (rnd:rndi num))
        (snek:del-edge? (rnd:rndi num) (rnd:rndi num)))))

    (setf snk (init-snek num))

    (time (loop for i from 0 below itt do
      (when (= (mod i 100000) 0)
        (format t "itt ~a edges ~a ~%" i (length (snek:get-edges snk))))

      (snek:cwith (snk %)
        (% (snek:add-edge? (rnd:rndi num) (rnd:rndi num)))
        (% (snek:del-edge? (rnd:rndi num) (rnd:rndi num))))))))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         :mode :cpu
                         ;:mode :alloc
                         ;:mode :time
                         :report :graph)
 (main))

