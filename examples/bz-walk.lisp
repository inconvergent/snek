#!/usr/bin/sbcl --script

(load "../src/load")



(defun main (size fn)
  (let ((itt 1000000)
        (noise 0.000000005d0)
        (grains 20)
        (sand (sandpaint:make size
                :fg (pigment:white 0.05)
                :bg (pigment:dark))))


    (let* (
           (pts-1 (rnd:nin-box 6 1500d0 1500d0 :xy (vec:vec 1000d0 1000d0)))
           (pts (rnd:nin-box 6 1500d0 1500d0 :xy (vec:vec 1000d0 1000d0)))
           ;(pts-a (math:nrep 5 (list 50.0d0 (rnd:rndbtwn 50 1950))))
           (pts-a (math:rep (p (math:linspace 5 50d0 1950d0)) (vec:vec 50.0d0 p)))
           ;(pts-b (math:nrep 5 (list 1950.0d0 (rnd:rndbtwn 50 1950))))
           (pts-b (math:rep (p (math:linspace 5 50d0 1950d0)) (vec:vec 50.0d0 p)))
           (b (bzspl:make pts :closed t))
           (b-1 (bzspl:make pts-1 :closed t))
           (ba (bzspl:make pts-a :closed t))
           (bb (bzspl:make pts-b :closed t))
           (wa (rnd:get-acc-lin-stp (rnd:rnd)))
           (wb (rnd:get-acc-lin-stp (rnd:rnd))))

      (loop
        for p in (math:linspace itt 0d0 1d0 :end nil)
        for i from 0
        do (print-every i 100000)
           (sandpaint:set-fg-color sand (pigment:hsv p 1 1 0.05))
           (sandpaint:stroke sand
             (list (bzspl:pos b p)
                   (bzspl:pos b-1 p))
             grains)))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 2000 (second (cmd-args))))

