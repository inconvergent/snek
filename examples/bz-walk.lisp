#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)
  (let ((itt 1000000)
        (noise 0.000000005d0)
        (grains 20)
        (sand (sandpaint:make size
                :active (color:white 0.05)
                :bg (color:dark))))


    (let* (
           (pts-1 (rnd:nin-box 6 1500d0 1500d0 :xy (vec:vec 1000d0 1000d0)))
           (pts (rnd:nin-box 6 1500d0 1500d0 :xy (vec:vec 1000d0 1000d0)))
           ;(pts-a (math:nrep 5 (list 50.0d0 (rnd:rndbtwn 50 1950))))
           (pts-a (math:rep (p (math:linspace 5 50 1950)) (vec:vec 50.0d0 p)))
           ;(pts-b (math:nrep 5 (list 1950.0d0 (rnd:rndbtwn 50 1950))))
           (pts-b (math:rep (p (math:linspace 5 50 1950)) (vec:vec 50.0d0 p)))
           (b (bzspl:make pts :closed t))
           (b-1 (bzspl:make pts-1 :closed t))
           (ba (bzspl:make pts-a :closed t))
           (bb (bzspl:make pts-b :closed t))
           (wa (rnd:get-acc-lin-stp (rnd:rnd)))
           (wb (rnd:get-acc-lin-stp (rnd:rnd))))

      (loop
        for p in (math:linspace itt 0 1 :end nil)
        for i from 0
        do
          (print-every i 100000)
          (sandpaint:set-rgba sand (color:hsv p 1 1 0.05))
          (sandpaint:stroke sand
            (list
             (bzspl:pos b p)
             (bzspl:pos b-1 p))
            grains)))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 2000 (second (cmd-args))))

