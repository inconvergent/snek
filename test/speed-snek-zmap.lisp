#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-snek (n mid)
  (let ((snk (snek:make
               :max-verts 5000000
               :grp-size 500000)))
    (math:nrep n (snek:add-vert! snk (rnd:in-box mid mid :xy (vec:v mid))))
    snk))


(defun main ()
  (let* ((itt 10000)
        (size 1000)
        (num 20000)
        (farl 100.d0)
        (snk (init-snek num (* 0.5d0 size))))

    ;(snek:with (snk :zwidth farl)
    ;  (print (snek:verts-in-rad snk (nrep 2 (rnd:rnd size)) farl)))

    (time (loop for i from 0 below itt do
      (print-every i 2000)

      (snek:with (snk :zwidth farl)
        (let ((v (rnd:rndi num)))
          (map nil (lambda (w)
             (snek:with-dx (snk (list v w) dx d)
               (list dx d)))
           (snek:verts-in-rad snk (snek:get-vert snk v) farl))))))

    (time (loop for i from 0 below itt do
      (print-every i 2000)

      (snek:with (snk :zwidth farl)
        (let ((v (rnd:rndi num)))
          (snek:with-verts-in-rad (snk (snek:get-vert snk v) farl w)
             (snek:with-dx (snk (list v w) dx d)
               (list dx d)))
          ))))

    (format t "verts: ~a~%" (snek::snek-num-verts snk))))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         :mode :cpu
                         ;:mode :alloc
                         ;:mode :time
                         :report :graph)
 (main))
;(main)
