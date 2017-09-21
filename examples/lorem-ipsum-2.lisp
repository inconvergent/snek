#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/lorem")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))

(defvar *text* "lorem ipsum dolor sit amet, consectetur adipiscing")


(defun get-fxn* (bbox)
  (vec:with-xy ((vec:scale bbox 0.5d0) bx by)
    (lambda (n)
      (mapcar (lambda (v) (vec:rot v (* PI 0.15d0)))
              (rnd:nin-box n bx by)))))


(defun scale-fxn* (bbox)
  (if (< (rnd:rnd) 0.15)
    (vec:mult bbox (vec:vec 1d0 3.5d0))
    bbox))


(defun main (size fn)
  (let ((left 65d0)
        (top 450d0)
        (bottom 10000d0)
        (right 10000d0)
        (grains 20)
        (nc 3)
        (ncn 2)
        (bbox (vec:vec 40d0 100d0))
        (spacebox (vec:vec 40d0 35d0))
        (sand (sandpaint:make size
                :active (color:black 0.009)
                :bg (color:white))))

    (let ((alphabet (get-alphabet #'get-fxn* #'scale-fxn* bbox nc ncn))
          (snk (snek:make))
          (state-gen (math:get-state-gen (lambda () (rnd:get-acc-circ-stp*)))))

      (loop for a across "abcdfg" and x in (math:linspace 6 100d0 800d0) do
        (loop for b across "abcdfg" and y in (math:linspace 6 150d0 850d0) do
          (do-write snk alphabet spacebox y right bottom x (format nil "~a~a" a b))))
      ;(do-write snk alphabet spacebox 200d0 right bottom left "ae be ce de")
      ;(do-write snk alphabet spacebox 400d0 right bottom left "af bf cf df")
      ;(do-write snk alphabet spacebox 600d0 right bottom left "ag bg cg dg")
      ;(do-write snk alphabet spacebox 800d0 right bottom left "ah bh ch dh")
      (loop for i from 0 below 500 do
        (snek:with (snk)
          (snek:itr-all-verts (snk v)
            (snek:move-vert? v (rnd:in-circ 0.1d0))
            (snek:move-vert? v (funcall state-gen v 0.00001d0))))
        (snek:itr-grps (snk g)
          (aif (snek:get-grp-as-bzspl snk g)
               (sandpaint:pix sand (bzspl:rndpos it
                                     (* (rnd:rndi (- grains 5) (+ grains 5))
                                        (bzspl::bzspl-n it))))))))

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn :gamma 1.5)))

(time (main 1000 (second (cmd-args))))

