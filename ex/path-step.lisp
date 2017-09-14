#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)
  (let ((grains 60)
        (itt 300000)
        (lsa (rnd:get-acc-lin-stp))
        (lsb (rnd:get-acc-lin-stp))
        (lsc (rnd:get-acc-lin-stp))
        (noise 0.00000002d0)
        (snk (snek:make))
        (points-a (close-path (rnd:nin-box 3 1500d0 1500d0 :xy (vec:vec 1000d0 1000d0))))
        (points-b (close-path (rnd:nin-box 3 1500d0 1500d0 :xy (vec:vec 1000d0 1000d0))))
        (points-c (close-path (rnd:nin-box 3 1500d0 1500d0 :xy (vec:vec 1000d0 1000d0))))
        (sand (sandpaint:make size
                :active (list 0 0 0 0.01)
                :bg (list 1 1 1 1.0))))

    (let ((v1 (snek:add-vert! snk (vec:vec 0d0 0d0)))
          (v2 (snek:add-vert! snk (vec:vec 0d0 0d0)))
          (v3 (snek:add-vert! snk (vec:vec 0d0 0d0)))
          (path-a (lin-path:make points-a))
          (path-b (lin-path:make points-b))
          (path-c (lin-path:make points-c)))

      (snek:add-edge! snk (list v1 v2))
      (snek:add-edge! snk (list v2 v3))
      (snek:add-edge! snk (list v3 v1))
      ;(snek:add-edge! snk (list v1 v3))

      (loop for i from 0 to itt do
        (snek:with (snk)
          ;(lin-path:move path-a (rnd:nin-circ 4 0.3d0) t)
          ;(lin-path:move path-b (rnd:nin-circ 4 0.3d0) t)
          ;(lin-path:move path-c (rnd:nin-circ 4 0.1d0) t)
          (snek:move-vert? v1 (lin-path:pos path-a (funcall lsa noise)) :rel nil)
          (snek:move-vert? v2 (lin-path:pos path-b (funcall lsb noise)) :rel nil)
          (snek:move-vert? v3 (lin-path:pos path-c (funcall lsc noise)) :rel nil))
        (snek:draw-edges snk sand grains))

      ;(sandpaint:set-rgba sand (list 1 0 0 0.01))
      ;(sandpaint:pix sand (lin-path:pos* path-a (rnd:rndspace 10 0d0 1d0)))
      ;(sandpaint:pix sand (lin-path:pos* path-c (rnd:rndspace 10 0d0 1d0)))

      (sandpaint:pixel-hack sand)
      (sandpaint:save sand fn :gamma 1.3))))


(time (main 2000 (second (cmd-args))))

