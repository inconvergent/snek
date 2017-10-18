#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/test")

(setf *print-pretty* t)
;(setf *random-state* (make-random-state t))

(defun test-color ()

  (do-test
    (color:rgb 0.1 1.0 0.5)
    (color:rgb 0.1 1.0 0.5))

  (do-test
    (color:to-list(color:rgb 0.1 1.0 0.5 0.2))
    (list 0.10000000149011612d0 1.0d0 0.5d0 0.20000000298023224d0))

  (do-test
    (color:hsv 0.5 1.0 1.0)
    (color:rgb 0 1.0 1.0))

  (do-test
    (color:to-list (color:rgb 0 1.0 1.0 0.5))
    (list 0.0d0 1.0d0 1.0d0 0.5d0))

  (do-test
    (color:to-list* (color:rgb 0 1.0 1.0 0.5))
    (list 0.0d0 0.5d0 0.5d0 0.5d0))

  (do-test
    (color:cmyk 1 0 0 0)
    (color:rgb 0 1.0 1.0))

  (do-test
    (color:cmyk 0.5 0 0 0.5)
    (color:rgb 0.25 0.5 0.5)))


(defun get-sample-pix (sand)
  (let ((vals (sandpaint::sandpaint-vals sand)))
    (flatten (loop for i in (list 10 45 45 92 23)
          and j in (list 39 78 49 92 89) collect
          (list
            (aref vals i j 0)
            (aref vals i j 1)
            (aref vals i j 2)
            (aref vals i j 3))))))


(defun test-sandpaint ()
  (let ((sand (sandpaint:make 100
                              :fg (color:black)
                              :bg (color:white))))
    (loop for p in (rnd:nin-box 100000 50d0 50d0 :xy (vec:vec 50d0 50d0)) do
          (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 0.004))
          (sandpaint:pix sand (list p)))

    (do-test
      (get-sample-pix sand)
      (list 0.9770396294438961d0 0.9801971801802848d0 0.9820774517045686d0 1.0d0
            0.9798218752437813d0 0.9860460888368332d0 0.984990799442834d0 1.0d0
            0.9825994424773152d0 0.9809261465057241d0 0.9815416215658388d0 1.0d0
            0.9826617202763499d0 0.9808148642070308d0 0.98304645575613d0 1.0d0
            0.98222873852567d0 0.9845816204446133d0 0.9879567413958947d0 1.0d0)))

  (let ((sand (sandpaint:make 100
                              :fg (color:black)
                              :bg (color:transparent))))

   (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 0.3d0))
   (sandpaint:pix sand (rnd:nin-box 100000 40d0 50d0 :xy (vec:vec 50d0 50d0)))

   (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 0.3d0))
   (sandpaint:pix sand (rnd:nin-box 100000 50d0 20d0 :xy (vec:vec 50d0 50d0)))

   (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 1d0))
   (sandpaint:pix sand (rnd:nin-box 100000 20d0 50d0 :xy (vec:vec 50d0 50d0)))

    (do-test
      (get-sample-pix sand)
      (list 0.7923387230876728d0 0.3737097252973584d0 0.5418670927856052d0
            0.9999774606597089d0 0.2894221927545413d0 0.8660510959909917d0
            0.15506417423225605d0 1.0d0 0.2894221927545413d0 0.8660510959909917d0
            0.15506417423225605d0 1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.4569639654062936d0
            0.9693292839928664d0 0.6799782642280339d0 0.9932177692715098d0))))


(defun main ()
  (test-title (test-color))
  (test-title (test-sandpaint))
  (test-title (test-summary)))

(main)

