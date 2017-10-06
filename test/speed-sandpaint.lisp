#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main ()
  (let* ((num 1000000)
         (pix-num 1000000)
         (grains 100)
         (sand (sandpaint:make 1000
                :active (list 0 0 0 0.01)
                :bg (list 1 1 1 1))))


  (format t "strokes:~%")
  (time
    (sandpaint:strokes sand
     (math:nrep num (list
                     (rnd:in-box 500d0 500d0 :xy (vec:v 500d0))
                     (rnd:in-box 500d0 500d0 :xy (vec:v 500d0))))
     grains))

  (format t "pix:~%")
  (time
    (loop repeat 100 do
      (sandpaint:pix sand
        (math:nrep pix-num (rnd:in-box 500d0 500d0 :xy (vec:v 500d0))))))))

;(require :sb-sprof)
;(sb-sprof:with-profiling (:max-samples 200000
;                         :mode :cpu
;                         ;:mode :alloc
;                         ;:mode :time
;                         :report :graph)
; (main))

(main)
