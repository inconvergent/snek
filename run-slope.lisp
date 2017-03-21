#!/usr/bin/sbcl --script

(load "src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)

  (let ((p1 (rnd-in-circ 100 :x 800.0 :y 200.0))
        (p2 (rnd-in-circ 100 :x 200.0 :y 800.0)))
    (let ((va (add
                (scale
                   (norm
                     (mult (reverse (sub p1 p2)) (list -1.0 1.0)))
                   40.0)
                 (rnd-on-circ 20 :x 0.0 :y 0.0)))
         (repeat 10)
         (noise (random 4.0))
         (grains 70)
         (itt 6000)
         (sand (sandpaint:make size
                 :active (list 0 0 0 0.01)
                 :bg (list 1 1 1 1))))

      (loop for j from 1 to repeat
        do
          (print-every j 2)
          (let ((snk (snek*)))
            (setf va (add va (rnd-in-circ noise :x 0.0 :y 0.0)))

            (loop for k from 1 to itt
              do
                (insert-vert snk (rnd-on-line p1 p2))
                (with-snek (snk)
                  (with-rnd-vert (snk v)
                    (append-edge v va))
                  (with-rnd-vert (snk v)
                    (with-rnd-vert (snk w)
                      (join-verts w v)))))
              (snek-draw-edges snk sand grains)))


   (sandpaint:pixel-hack sand)
   (format t "~%writing to ~a" fn)
   (sandpaint:save sand fn))))


(time (main 1000 (second (cmd-args))))

