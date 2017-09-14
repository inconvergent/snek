#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun circ-stroke (sand vv)
 (sandpaint:circ sand
   (lin-path:pos* (lin-path:make vv) (math:linspace 10000 0 1 :end nil))
   1 20))


(defun rnd-path (n)
  (let ((curr nil))
    (labels ((do-append-edge-alt* (snk a)
               (if (> (vec:dst
                        (append-edge-alt-xy a)
                        (snek:get-vert snk (append-edge-alt-v a)))
                      200d0)
                 (aif (do-append-edge-alt snk a)
                    (setf curr it)))))

      (let ((snk (snek:make
                   :alts `((append-edge-alt ,#'do-append-edge-alt*)))))

        (setf curr (snek:add-vert! snk (rnd:in-box 1000d0 1000d0 :xy (vec:vec 500d0 500d0))))

        (loop for i from 0 below n do
          (snek:with (snk)
            (snek:append-edge?
              curr
              (rnd:in-box 1000d0 1000d0 :xy (vec:vec 500d0 500d0))
              :rel nil)))

        (snek:get-grp-verts snk)))))


(defun main (size fn)
  (let ((itt 100000)
        (noise 0.00000005d0)
        (snk (snek:make))
        (sand (sandpaint:make size
                :active (list 1 1 1 0.01)
                :bg (list 0.05 0.05 0.05 1)))
        (pa (lin-path:make (rnd-path 4)))
        (pb (lin-path:make (rnd-path 4)))
        (lsa (rnd:get-acc-lin-stp (rnd:rnd)))
        (lsb (rnd:get-acc-lin-stp (rnd:rnd)))
        (mut (snek:make-mutate :noise 100)))

    (let ((v1 (snek:add-vert! snk (vec:vec 0d0 0d0)))
          (v2 (snek:add-vert! snk (vec:vec 0d0 0d0))))

      (snek:add-edge! snk (list v1 v2))

      (loop for p in (math:linspace itt 0 1 :end nil) do
        (snek:with (snk)
          (snek:mutate (mut)
            (list
              (snek:move-vert? v1 (lin-path:pos pa (funcall lsa noise)) :rel nil)
              (snek:move-vert? v2 (lin-path:pos pb (funcall lsb noise)) :rel nil))))
        (snek:draw-edges snk sand 100)))

    (sandpaint:save sand fn)))

(time (main 1000 (second (cmd-args))))

