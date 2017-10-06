#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main ()
  (let ((num 1000000)
        (rep 1000))

  (format t "rndspace:~%")
  (time
    (loop repeat rep do
      (rnd:rndspace num 0d0 1d0)))

  (format t "with-rndspace:~%")
  (time
    (loop repeat rep do
      (rnd:with-rndspace (num 0d0 1d0 v)
        v)))))

;(require :sb-sprof)
;(sb-sprof:with-profiling (:max-samples 200000
;                         :mode :cpu
;                         ;:mode :alloc
;                         ;:mode :time
;                         :report :graph)
; (main))

(main)
