#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun main ()
  (let* ((itt 500000))

    (format t "make 100")
    (time
      (loop with pts = (rnd:nin-box 100 500d0 500d0 :xy (vec:vec 500d0))
            for i from 0 below itt
            do (bzspl:make pts)))

    (format t "make 1000")
    (time
      (loop with pts = (rnd:nin-box 1000 500d0 500d0 :xy (vec:vec 500d0))
            for i from 0 below itt
            do (bzspl:make pts)))

    (format t "pos 500")
    (time
      (loop with pos = (rnd:nrnd 500)
            with bz = (bzspl:make (rnd:nin-box 100 500d0 500d0 :xy (vec:vec 500d0)))
            for i from 0 below itt
            do (bzspl:pos* bz pos)))

    (format t "adaptive")
    (time
      (loop with bz = (bzspl:make (rnd:nin-box 5 500d0 500d0 :xy (vec:vec 500d0)))
            for i from 0 below 40000
            do (bzspl:adaptive-pos bz)))))


(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         ;:mode :cpu
                         ;:mode :alloc
                         :mode :time
                         :report :graph)
 (main))
