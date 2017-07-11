#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-snek (n m size xy)
  (let ((snk (snek:make :max-verts 5000)))
    (mapcar
      (lambda (g) (let ((mid (rnd:in-circ (half (- size 200)) :xy xy)))
                    (snek:init-path snk
                      (math:rep (p (math:linspace 0 1 n :end nil))
                        (math:on-circ p 20 :xy mid))
                      :g g
                      :closed t)))
      (math:nrep m (snek:add-grp! snk)))
  snk))



(defun main (size fn)
  (let ((itt 1000)
        (grains 30)
        (snk (init-snek 5 800 size (math:nrep 2 (half size))))
        (sand (sandpaint:make size
                :active (color:white 0.05)
                :bg (color:dark))))

    (loop for i from 0 below itt
      do
        (print-every i 100)

        (snek:with (snk :zwidth 60.0d0)
          (snek:itr-all-verts (snk v)
            (map 'list (lambda (w) (snek:force? snk v w -0.05))
                       (snek:verts-in-rad snk (snek:get-vert snk v) 60.0d0)))
          (snek:itr-grps (snk g)
            (snek:itr-edges (snk e :g g)
              (snek:force? snk (first e) (second e) 0.1))))

        (snek:itr-grps (snk g)
          (sandpaint:pix
            sand
            (bzspl:rndpos
              (bzspl:make (snek:get-grp-verts snk :g g) :closed t)
              grains))))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 1000 (second (cmd-args))))

