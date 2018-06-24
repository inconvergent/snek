#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun main (size fn)
  (let ((psvg (plot-svg:make* :width 1000d0
                              :height 1000d0
                              :stroke-width 1d0
                              :rep-scale 0.5d0)))

    (loop for x in (math:linspace 7 80d0 920d0) do
      (loop for y in (math:linspace 7 80d0 920d0) do
        (let ((path (rnd:nin-box 5 40d0 40d0 :xy (vec:vec x y))))
          (plot-svg:path psvg (vec:ladd* path (vec:vec 20d0 0d0)))
          (plot-svg:cpath psvg (vec:ladd* path (vec:vec -20d0 0d0)) :simplify 10d0
                          :width 10d0))))

    (plot-svg:save psvg "plot-simplify")))

(time (main 1000 (second (cmd-args))))

