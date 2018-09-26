#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun init-snek (n)
  (let ((snk (snek:make :max-verts 10000 :grp-size 100)))
    (math:nrep n (snek:add-vert! snk (vec:vec (rnd:rnd 100d0)
                                              (rnd:rnd 100d0))))
    snk))


(defun main ()
  (let* ((itt 5000)
         (num 10000)
         (ww (vec:vec 10d0 33.3d0))
         (uu (vec:vec 1d0 3.2d0))
         (snk (init-snek num)))

    (time (loop for i from 0 below itt do
      (snek:with (snk)
        (snek:itr-verts (snk v)
          nil
          (snek:move-vert? v ww)
          (snek:move-vert? v uu)))))

    (time (loop for i from 0 below itt do
      (snek:cwith (snk %)
        (snek:itr-verts (snk v :collect nil)
          (% nil)
          (% (snek:move-vert? v ww))
          (% (snek:move-vert? v uu))))))))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         ;:mode :cpu
                         ;:mode :alloc
                         :mode :time
                         :report :graph)
 (time (main)))

