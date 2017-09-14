#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun main (size fn)
  (let ((sand (sandpaint:make
                size
                :active '(0 0 0 0.01)
                :bg '(1 1 1 1)))
        (path-a (close-path (rnd:nin-box 10 500d0 500d0 :xy (vec:vec 500d0 500d0))))
        (path-b (close-path (rnd:nin-box 4 500d0 500d0 :xy (vec:vec 500d0 500d0))))
        (na (rnd:get-lin-stp))
        (nb (rnd:get-lin-stp))
        (pa (random 1.0d0))
        (pb (random 1.0d0))
        (noise 0.000000001d0))

    (let ((fa (bzspl:make path-a))
          (fb (bzspl:make path-b)))

      (loop for i from 0 to 1000000 do
        (print-every i 10000)

        (setf pa (math:inc pa (funcall na noise)))
        (setf pb (math:inc pb (funcall nb noise)))

        (let ((a (bzspl:pos fa pa))
              (b (bzspl:pos fb pb)))
          (sandpaint:strokes sand (list (list a b)) 20))))

      ;(sandpaint:chromatic-aberration sand (list 500 500) :s 200.0)
      (sandpaint:save sand fn)))



(time (main 1000 (second (cmd-args))))

