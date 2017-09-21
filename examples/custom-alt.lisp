#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defun circ-stroke (sand vv)
  (sandpaint:circ sand
    (lin-path:pos* (lin-path:make vv) (math:linspace 100 0 0.99))
    1d0 20))


(defun draw-path (sand fn n rad mid)
  (let ((curr nil)
        (i 0))

    (labels ((new-append-edge-alt* (snk a)
               (if (<= (length (snek:verts-in-rad snk (snek::append-edge-alt-xy a) rad)) 1)
                 (aif (snek::do-append-edge-alt snk a)
                   (progn
                     (incf i)
                     (setf curr it)
                     (sandpaint:set-rgba sand (list 0.0 0.7 0.7 0.01))
                     (sandpaint:circ sand (list (snek::append-edge-alt-xy a)) 4d0 3000)
                     (sandpaint:set-rgba sand (list 0.0 0.0 0.0 0.01))
                     (circ-stroke sand (snek:get-verts snk (list it (snek::append-edge-alt-v a))))
                     (sandpaint:save sand (format nil "~a-~3,'0d" fn i)))))))

      (let ((snk (snek:make
                   :max-verts n
                   :alts `((snek::append-edge-alt ,#'new-append-edge-alt*)))))

        (setf curr (snek:add-vert! snk mid))

        (loop for i from 0 below n do
          (snek:with (snk :zwidth rad)
            (snek:append-edge?
              curr (vec:add (snek:get-vert snk curr) (rnd:in-circ rad))
              :rel nil)))))))


(defun main (size fn)
  (let ((sand (sandpaint:make size
                :active (list 0 0 0 0.01)
                :bg (list 1 1 1 1.0))))

    (draw-path sand fn 5000 10.0d0 (vec:vec 250d0 250d0))))

(time (main 500 (second (cmd-args))))

