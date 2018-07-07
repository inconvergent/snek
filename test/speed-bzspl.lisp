#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun main ()
  (let* ((itt 500000))

    ;(snek:with (snk :zwidth farl)
    ;  (print (snek:verts-in-rad snk (nrep 2 (rnd:rnd size)) farl)))

    (format t "make 10")
    (time
      (loop for i from 0 below itt
            do (bzspl:make (rnd:nin-box 10 500d0 500d0 :xy (vec:vec 500d0)))))

    (format t "make 100")
    (time
      (loop for i from 0 below itt
            do (bzspl:make (rnd:nin-box 100 500d0 500d0 :xy (vec:vec 500d0)))))

    (format t "pos 10")
    (time
      (loop with pos = (rnd:nrnd 100)
            with bz = (bzspl:make (rnd:nin-box 10 500d0 500d0 :xy (vec:vec 500d0)))
            for i from 0 below itt
            do (bzspl:pos* bz pos)))

    ;(format t "pos 100")
    ;(time
    ;  (loop pos = (rnd:nrnd 100)
    ;        with bz = (bzspl:make (rnd:nin-box 100 500d0 500d0 :xy (vec:vec 500d0)))
    ;        for i from 0 below itt
    ;        do (bzspl:pos (rnd:rnd))))

    ))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         :mode :cpu
                         ;:mode :alloc
                         ;:mode :time
                         :report :graph)
 (main))
