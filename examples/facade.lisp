#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
(setf *random-state* (make-random-state t))



(defun mpath (lines p)
  (mapcar
    (lambda (l)
      (lin-path:pos (lin-path:make l) p))
    lines))


(defun get-lines (a b n)
  (loop for x in (rnd:rndspace n a b :order t)
    collect
      (loop for y in (rnd:rndspace n a b)
            collect
          (vec:vec x y))))



(defun main (size fn)
  (let ((itt 100000)
        (nlines 300)
        (nsteps 1000)
        (grains 50)
        (sand (sandpaint:make size
                :active (list 1 1 1 0.05)
                :bg (list 0.2 0.2 0.2 1))))
          (let ((lines (get-lines -500d0 1500d0 40)))

            (loop
              for u in (rnd:rndspace nlines 0d0 1d0)
              do
                (let ((pa (lin-path:make (mpath lines u)))
                      (pb (lin-path:make (mpath lines (+ (rnd:rnd* 0.05d0) u)))))
                  (let ((r1 (rnd:rnd))
                        (r2 (rnd:rnd)))
                    ;(sandpaint:lin-path sand (lin-path:pos* pa (math:linspace 100 r1 r2)) 1.0 20)
                    ;(sandpaint:lin-path sand (lin-path:pos* pb (math:linspace 100 r1 r2)) 1.0 20)
                    (rnd:with-rndspace (nsteps r1 r2 v)
                      (sandpaint:stroke sand
                        (list (lin-path:pos pa v) (lin-path:pos pb v))
                        grains))))))

    (sandpaint:save sand fn)))

(time (main 1000 (second (cmd-args))))

