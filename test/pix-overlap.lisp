#!/usr/bin/sbcl --script

(load "../src/load")

(rnd:set-rnd-state 1)

(defun main (size)
  (let ((snk (snek:make))
        (sand (sandpaint:make size
                :fg (color:white 1d0)
                :bg (color:gray 0.1d0))))

    (sandpaint:pix-overlap sand (list (vec:vec 101.1d0 101.1d0)
                                      (vec:vec 105.0d0 101.1d0)
                                      (vec:vec 111.7d0 101.1d0)

                                      (vec:vec 101.1d0 105.0d0)
                                      (vec:vec 105.0d0 105.0d0)
                                      (vec:vec 111.7d0 105.0d0)

                                      (vec:vec 101.1d0 111.7d0)
                                      (vec:vec 105.0d0 111.7d0)
                                      (vec:vec 111.7d0 111.7d0)))

    (sandpaint:set-fg-color sand (color:rgb 0d0 1d0 0d0))

    (sandpaint:pix-overlap sand (list (vec:vec 151.7d0 151.7d0)
                                      (vec:vec 155.0d0 151.7d0)
                                      (vec:vec 161.1d0 151.7d0)

                                      (vec:vec 151.7d0 155.0d0)
                                      (vec:vec 155.0d0 155.0d0)
                                      (vec:vec 161.1d0 155.0d0)

                                      (vec:vec 151.7d0 161.1d0)
                                      (vec:vec 155.0d0 161.1d0)
                                      (vec:vec 161.1d0 161.1d0)))

    (loop for v in (rnd:nin-box 5000000 50d0 50d0 :xy (vec:vec 240d0))
          do (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd)))
             (sandpaint:pix-overlap* sand v))

    (loop for v in (rnd:nin-box 5000000 50d0 50d0 :xy (vec:vec 240d0))
          do (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd)))
             (sandpaint:pix-overlap* sand v))

    (sandpaint:save sand "pix-overlap")))


(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         :mode :cpu
                         ;:mode :alloc
                         ;:mode :time
                         :report :graph)
  (main 300))
;(time (main 300))

