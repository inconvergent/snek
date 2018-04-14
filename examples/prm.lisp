#!/usr/bin/sbcl --script

(load "../src/load")


(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)
  (let* ((psvg (plot-svg:make* :width 1000d0
                               :height 1000d0
                               :stroke-width 1d0))
         (left 150d0)
         (right 850d0)
         (n 7)
         (bs 50d0))

    (vec:with-loop-grid ((math:linspace n left right) xy)
      (let* ((snk (snek:make :prms (snek:psvg-get-prm-types psvg)))
             (p (snek:add-prm! snk :type :bzspl :props 3d0)))

        (snek:add-verts! snk (rnd:nin-box 3 bs bs :xy xy) :p p)
        (loop repeat 10
              do (snek:with (snk)
                   (snek:add-vert? (rnd:in-box bs bs :xy xy) :p p))
                 (snek:prmr snk :p p))

        (print (snek:prmr snk :p p :type :v))
        (print (snek:prmr snk :p p :type :vv))

        (print (snek:get-prm-props snk :p p))

        (snek:prmr snk :p p :type :circs)
        (snek:prmr snk :p p :type :hatch
                            :args (list :closed t :rs 0.2 :angles (rnd:nrnd 2)))))

    (plot-svg:save psvg fn)))


(time (main 1000 (second (cmd-args))))

