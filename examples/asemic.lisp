#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/state")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun init-line (sxy width line-chars char-num char-rad)
  (let ((snk (snek:make)))
    (vec:with-xy (sxy sx sy)
      (loop
        for char-pos-x in (rnd:rndspace line-chars sx (+ sx width) :order t)
        for char-pos-y in (rnd:rndspace line-chars (- sy 10d0) (+ sy 10d0))
        for char-height in (math:add
                             (rnd:rndspace line-chars 0.4d0 1d0)
                             (math:scale*
                               (rnd:bernoulli line-chars 0.05d0)
                               2d0))
        collect
          (snek:add-verts! snk
            (mapcar
              (lambda (v)
                (vec:add
                  (vec:mult v (vec:vec 2.0d0 char-height))
                  (vec:vec char-pos-x char-pos-y)))
              (rnd:nin-circ (rnd:rndi* char-num) (rnd:rnd char-rad))))))
    snk))


(defun main (size fn)
  (let ((border 100d0)
        (char-num (list 1 7))
        (char-rad 22d0)
        (line-chars 30)
        (line-num 20)
        ;(mut (snek:make-mutate
        ;       :prob 0.001d0
        ;       :noise 10.d0))
        (sand (sandpaint:make size
                :active (color:black 0.009)
                :bg (color:white))))

    (loop for y in (math:linspace line-num border (- size border)) do
      (format t "~a ~%" y)
      (let ((snk (init-line (vec:vec border y)
                            (- size (* 2.0 border))
                            line-chars char-num char-rad))
            (drift (rnd:get-acc-circ-stp*))
            (state-gen (get-state-gen
                         (lambda () (rnd:get-acc-circ-stp*)))))

        (snek:with (snk)
          (snek:itr-all-verts (snk v)
            (snek:move-vert? v (funcall drift 0.04d0))))

        (loop for i from 0 below 200 do
          (snek:with (snk)
            ;(snek:mutate (mut)
            (snek:itr-all-verts (snk v)
              ;(snek:move-vert? v (funcall state-gen v 0.00009d0))
              (snek:move-vert? v (rnd:in-circ 0.4d0))))
          (sandpaint:bzspl-stroke sand (bzspl:make (snek:get-all-verts snk))
                                  2500))))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 1000 (second (cmd-args))))

