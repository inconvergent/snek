#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-snek (n size)
  (let ((snk (make-snek
               :max-verts 5000000
               :max-main-grp-edges 500000)))
    (nrep n (add-vert! snk (nrep 2 (rnd:rnd size))))
    snk))


(defun main ()
  (let* ((itt 10000)
        (size 1000)
        (num 5000)
        (farl 100.d0)
        (snk (init-snek num size)))

    (loop for i from 0 below itt do
      (if (= (mod i 100) 0)
        (format t "itt ~a~%" i))

      ;(with-snek (snk :zwidth farl)
      ;  (print (verts-in-rad snk (nrep 2 (rnd:rnd size)) farl)))

      (with-snek (snk :zwidth farl)
        (let ((v (rnd:rndi num)))
          (map 'list (lambda (w)
             (with-dx (snk (list v w) dx d)
                      (list dx d)
                      ;(print (list dx d))
               ))
           (verts-in-rad snk (get-vert snk v) farl)))))

    (format t "verts: ~a~%" (snek-num-verts snk))))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         ;:mode :cpu
                         ;:mode :alloc
                         :mode :time
                         :report :flat)
 (time (main)))

