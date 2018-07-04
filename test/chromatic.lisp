#!/usr/bin/sbcl --script

(load "../src/load")

(rnd:set-rnd-state 1)


(defun main (size fn)
  (let ((snk (snek:make))
        (sand (sandpaint:make size
                :fg (color:white 0.1d0)
                :bg (color:gray 0.1d0))))


    (loop for x in (math:linspace 9 200d0 1800d0) do
      (loop for y in (math:linspace 9 200d0 1800d0) do
        (sandpaint:set-fg-color sand (color:white 0.3d0))
        (sandpaint:circ sand (list (vec:vec x y)) 120d0 300000)
        (sandpaint:set-fg-color sand (color:black))
        (sandpaint:circ sand (list (vec:vec x y)) 100d0 300000)))

    (sandpaint:set-fg-color sand (color:white 0.3d0))
    (sandpaint:circ sand (list (vec:vec 1000d0)) 5d0 30000)

    (sandpaint:chromatic-aberration sand :s 10d0)
    (sandpaint:pixel-hack sand)
    (sandpaint:save sand "chromatic")))


(time (main 2000 (second (cmd-args))))

