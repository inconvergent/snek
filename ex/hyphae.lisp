#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun hyphae (sand size fn itt rad mid)
  (let ((curr (make-hash-table :test #'equal))
        (hits 0))

    (labels ((init (snk n)
               (loop for i from 0 below n do
                 (setf (gethash
                         (snek:add-vert! snk (rnd:in-box 250 250 :xy '(250 250)))
                         curr)
                       0)))

             (draw (snk a w)
               (sandpaint:set-rgba sand (list 0.0 0.7 0.7 0.1))
               (sandpaint:circ sand (list (snek::append-edge-alt-xy a)) 4 300)
               (sandpaint:set-rgba sand (list 0.0 0.0 0.0 0.1))
               (sandpaint:lin-path sand
                 (snek:get-verts snk (list w (snek::append-edge-alt-v a)))
                 2.0
                 50))

             (count-control (v)
               (multiple-value-bind (c exists)
                 (gethash v curr)
                 (if exists
                   (if (> c 20)
                     (print (remhash v curr))
                     (incf (gethash v curr))))))

             (do-append-edge-alt* (snk a)
               (let ((v (snek::append-edge-alt-v a)))
                 (count-control v)
                 (inside-border (size (snek::append-edge-alt-xy a) 10)
                   (if (<= (length (snek:verts-in-rad snk (snek::append-edge-alt-xy a) rad)) 1)
                     (aif (snek::do-append-edge-alt snk a)
                       (progn
                         (incf hits)
                         (setf (gethash it curr) 0)
                         (draw snk a it))))))))

      (let ((snk (snek:make
                   :max-verts itt
                   :alts `((snek::append-edge-alt ,#'do-append-edge-alt*)))))

        (init snk 20)

        (loop for i from 0 below itt do
          (snek:with (snk :zwidth rad)
            (loop for k being the hash-keys in curr collect
              (snek:append-edge? k
                (math:add (snek:get-vert snk k)
                     (rnd:in-circ rad))
                :rel nil))
            (sandpaint:save sand (append-number fn hits))))))))


(defun main (size fn)
  (let ((sand (sandpaint:make size
                :active (list 0 0 0 0.01)
                :bg (list 1 1 1 1.0))))

    (hyphae sand size fn 5000 10.0d0 (list 250 250))))

(time (main 500 (second (cmd-args))))

