#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-snek (n m size xy)
  (let ((snk (make-snek
               :max-verts 5000
               :max-main-grp-edges 20
               :max-grp-edges 10)))
    (mapcar
      (lambda (g) (let ((mid (rnd:in-box (half (- size 100))
                                         (half (- size 100)) :xy xy)))
                    (snek-init-path snk
                      (rep (p (linspace 0 1 n :end nil))
                           (on-circ p 20 :xy mid))
                      :g g
                      :closed t)))
      (nrep m (add-grp! snk)))
  snk))



(defun main (size fn)
  (let ((itt 1000)
        (grains 30)
        (snk (init-snek 5 800 size (nrep 2 (half size))))
        (sand (sandpaint:make size
                :active (color:white 0.05)
                :bg (color:dark))))

    (loop for i from 0 below itt
      do
        (print-every i 100)

        (with-snek (snk :zwidth 60.0d0)
          (itr-all-verts (snk v)
            (map 'list (lambda (w) (force? snk v w -0.05))
                       (verts-in-rad snk (get-vert snk v) 60.0d0)))
          (itr-grps (snk g)
            (itr-edges (snk e :g g)
              (force? snk (first e) (second e) 0.1))))

        (itr-grps (snk g)
          (sandpaint:pix
            sand
            (bzspl:rndpos
              (bzspl:make (get-grp-vert-vals snk :g g) :closed t)
              grains))))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 1000 (second (cmd-args))))

