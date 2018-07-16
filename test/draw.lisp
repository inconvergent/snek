#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/test")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)

(defun test-color ()

  (do-test (color:rgb 0.1 1.0 0.5) (color:rgb 0.1 1.0 0.5))

  (do-test (color:to-list(color:rgb 0.1 1.0 0.5 0.2))
           (list 0.10000000149011612d0 1.0d0 0.5d0 0.20000000298023224d0))

  (do-test (color:hsv 0.5 1.0 1.0) (color:rgb 0 1.0 1.0))

  (do-test (color:to-list (color:rgb 0 1.0 1.0 0.5))
           (list 0.0d0 1.0d0 1.0d0 0.5d0))

  (do-test (color:to-list* (color:rgb 0 1.0 1.0 0.5))
           (list 0.0d0 0.5d0 0.5d0 0.5d0))

  (do-test (color:cmyk 1 0 0 0) (color:rgb 0 1.0 1.0))

  (do-test (color:cmyk 0.5 0 0 0.5) (color:rgb 0.25 0.5 0.5)))


(defun get-sample-pix (sand)
  (let ((vals (sandpaint::sandpaint-vals sand))
        (indfx (sandpaint::sandpaint-indfx sand)))
    (flatten (loop for i in (list 10 45 45 92 23)
                   and j in (list 39 78 49 92 89)
                   collect (list (aref vals (funcall indfx i j 0))
                                 (aref vals (funcall indfx i j 1))
                                 (aref vals (funcall indfx i j 2))
                                 (aref vals (funcall indfx i j 3)))))))


(defun test-sandpaint ()
  (let ((sand (sandpaint:make 100
                              :fg (color:black)
                              :bg (color:white))))
    (loop for p in (rnd:nin-box 100000 50d0 50d0 :xy (vec:vec 50d0 50d0)) do
          (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 0.004))
          (sandpaint:pix sand (list p)))

    (do-test
      (get-sample-pix sand)
      (list 0.9681936465458644d0 0.9853978043949597d0 0.9779575571520513d0 1.0d0
            0.9850031698578189d0 0.9847191146807868d0 0.9818651665563702d0 1.0d0
            0.9810825011577217d0 0.9774148613591062d0 0.9785530577286031d0 1.0d0
            0.9617585443476347d0 0.9684279445612141d0 0.9686700238368381d0 1.0d0
            0.9829785654522593d0 0.9840476095774362d0 0.9852693651002268d0 1.0d0)))

  (let ((sand (sandpaint:make 100 :fg (color:black)
                                  :bg (color:transparent))))

   (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 0.3d0))
   (sandpaint:pix sand (rnd:nin-box 100000 40d0 50d0 :xy (vec:vec 50d0 50d0)))

   (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 0.3d0))
   (sandpaint:pix sand (rnd:nin-box 100000 50d0 20d0 :xy (vec:vec 50d0 50d0)))

   (sandpaint:set-fg-color sand (color:rgb (rnd:rnd) (rnd:rnd) (rnd:rnd) 1d0))
   (sandpaint:pix sand (rnd:nin-box 100000 20d0 50d0 :xy (vec:vec 50d0 50d0)))

    (do-test
      (get-sample-pix sand)
      (list 0.6428169411853405d0 0.13168206068658486d0 0.6498467311662349d0
            0.9999945883043961d0 0.5129926107428067d0 0.37464299479913565d0
            0.15938469273885625d0 1.0d0 0.5129926107428067d0 0.37464299479913565d0
            0.15938469273885625d0 1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.8828667582669707d0
            0.0037578027642338877d0 0.04668942596643424d0 0.9596463929999997d0))))


(defun main ()
  (test-title (test-color))
  (test-title (test-sandpaint))
  (test-title (test-summary)))

(main)

