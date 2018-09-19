#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun main ()
  (let* ((num 1000000)
         (pix-num 1000000)
         (grains 100)
         (sand (sandpaint:make 1000 :fg (pigment:black 0.01)
                                    :bg (pigment:white))))

  (format t "strokes:~%")
  (time
    (sandpaint:strokes sand
      (math:nrep num (list (rnd:in-box 500d0 500d0 :xy (vec:vec 500d0))
                           (rnd:in-box 500d0 500d0 :xy (vec:vec 500d0))))
     grains))

  (format t "pix:~%")
  (time
    (loop repeat 100 do
      (sandpaint:pix sand
        (math:nrep pix-num (rnd:in-box 500d0 500d0 :xy (vec:vec 500d0))))))

  (format t "lin-path:~%")
  (time
    (loop repeat 500 do
      (sandpaint:lin-path sand
        (rnd:nin-box 5 500d0 500d0 :xy (vec:vec 500d0))
        3d0 grains)))

  ))

;(require :sb-sprof)
;(sb-sprof:with-profiling (:max-samples 200000
;                         :mode :cpu
;                         ;:mode :alloc
;                         ;:mode :time
;                         :report :graph)
; (main))

(main)

