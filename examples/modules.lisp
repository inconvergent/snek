#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defun run-join (snk)
  (snek:with (snk :zwidth 100.0d0)
    (snek:with-rnd-vert (snk v)
      (snek:join-verts? (rnd:aget (snek:verts-in-rad snk (snek:get-vert snk v) 100.0d0)) v))))

(defun run-move (snk)
  (snek:with (snk)
    (snek:with-rnd-vert (snk v)
      (snek:move-vert? v (rnd:in-circ 20d0)))))


(defun main (size fn)
  (let ((grains 2)
        (itt 100000)
        (noise 0.1)
        (rep 7)
        (snk (snek:make :max-verts 100000))
        (sand (sandpaint:make size
                :active (color:black 0.01)
                :bg (color:white))))

    (snek:add-verts! snk (rnd:nin-box 100 450d0 450d0 :xy (vec:vec 500d0 500d0)))

    (let ((fns (list 'run-join 'run-move)))
      (loop for i from 0 to itt do
        (print-every i 1000)
        (funcall (rnd:lget fns) snk)))

    ;(snek-export-2obj snk (append-postfix fn ".2obj"))
    (snek:draw-edges snk sand 1000)

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn :gamma 1.5)))


(time (main 1000 (second (cmd-args))))

