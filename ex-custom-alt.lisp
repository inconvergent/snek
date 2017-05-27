#!/usr/bin/sbcl --script

(load "src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defun circ-stroke (sand vv)
  (sandpaint:circ sand
    (lin-path:pos* (lin-path:make vv) (linspace 0 0.99 100))
    1 20))


(defun draw-path (sand fn n rad mid)
  (let ((curr nil)
        (i 0))

    (labels ((do-append-edge-alt* (snk a)
               (if (<= (length (verts-in-rad snk (append-edge-alt-xy a) rad)) 1)
                 (aif (do-append-edge-alt snk a)
                   (progn
                     (incf i)
                     (setf curr it)
                     (sandpaint:set-rgba sand (list 0.0 0.7 0.7 0.01))
                     (sandpaint:circ sand (list (append-edge-alt-xy a)) 4 3000)
                     (sandpaint:set-rgba sand (list 0.0 0.0 0.0 0.01))
                     (circ-stroke sand (get-verts snk (list it (append-edge-alt-v a))))
                     (sandpaint:save sand (format nil "~a-~3,'0d" fn i)))))))

      (let ((snk (make-snek
                   :max-verts n
                   :max-grp-edges 0
                   :max-main-grp-edges (* 4 n)
                   :alts `((append-edge-alt ,#'do-append-edge-alt*)))))

        (setf curr (add-vert! snk mid))

        (loop for i from 0 below n do
          (with-snek (snk :zwidth rad)
            (append-edge?
              curr (add (get-vert snk curr)
                        (rnd:in-circ rad))
              :rel nil)))))))


(defun main (size fn)
  (let ((sand (sandpaint:make size
                :active (list 0 0 0 0.01)
                :bg (list 1 1 1 1.0))))

    (draw-path sand fn 5000 10.0d0 (list 250 250))))

(time (main 500 (second (cmd-args))))

