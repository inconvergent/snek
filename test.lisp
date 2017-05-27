#!/usr/bin/sbcl --script

(load "src/load")
(load "./src/test-utils")

(setf *print-pretty* t)
;(setf *random-state* (make-random-state t))


(defun test-utils ()
  (do-test
    (norm '(3 0))
    '(1.0 0.0))

  (do-test
    (sub '(1 2) '(2 3))
    '(-1 -1))

  (do-test
    (add '(1 2) '(2 3))
      '(3 5))

  (do-test
    (nsub '(1 2) '(2 10))
    '(-0.12403473430574564d0 -0.9922778744459652d0))

  (do-test
    (len2 '(1 2))
    5)

  (do-test
    (len '(1 2))
    2.236068)

  (do-test
    (len '(1.0d0 2.0d0))
    2.23606797749979d0)

  (do-test
    (dst '(1 2) '(1 3))
    1.0)

  (do-test
    (mid '(1 2) '(3 4))
    '(2 3))

  (do-test
    (lmid '((1 2) (3 4) (5 6)))
    '(3 4))

  (do-test
    (lget '((1 2) (3 4) (5 6)) '(0 2))
    '((1 2) (5 6)))

  (do-test
    (inc 0.1 0.4)
    0.5)

  (do-test
    (inc 0.1 -0.4)
    0.7)

  (do-test
    (linspace 0 10 1)
    (list 0.0))

  (do-test
    (linspace 0 10 3)
    (list 0.0 5.0 10.0))

  (do-test
    (linspace 0 10 2 :end nil)
    (list 0.0 5.0))

  (do-test
    (linspace 0 10 2 :end t)
    (list 0.0 10.0))

  (do-test
    (range 2 5)
    (list 2 3 4))

  (do-test
    (range 5)
    (list 0 1 2 3 4)))


(defun test-rnd ()

  (do-test
    (length (rnd:rndspace 0 10 10))
    10)

  (do-test
    (rnd:rndspace 0 10 10)
     '(3.152262934102661d0 9.411859332177082d0 9.143334482781892d0
        9.707515698488775d0 6.005604715628142d0 2.8377247312878073d0
        8.435221790928992d0 8.314710996278352d0 5.844153198534443d0
        9.189848934771323d0))

  (do-test
    (rnd:rndspace 0 10 10 :order t)
     '(0.7142292146110663d0 3.109552134181708d0 3.1148128311978818d0
        3.4237318221390423d0 3.5746993898250556d0 4.862646815859608d0
        5.154807478401608d0 6.982586020701715d0 7.8445379456298925d0
        8.67986375197924d0))

  (do-test
    (rnd:rndspacei 0 10 10)
     '(7 9 2 0 0 2 4 4 9 9))

  (do-test
    (rnd:rndspacei 0 10 10 :order t)
     '(1 1 3 3 4 6 7 8 8 9)))


(defun test-bin ()
  (let ((edges (make-array
        (list 10 2)
        :adjustable nil
        :initial-contents
          '((1 0) (1 2) (1 5) (3 4) (10 3) (0 0) (0 0) (0 0) (0 0) (0 0)))))

    (do-test
      (-binary-edge-insert-search edges '(3 4) 5)
      4)

    (do-test
      (-binary-edge-insert-search edges '(0 0) 5)
      0)

    (do-test
      (-binary-edge-insert-search edges '(1 6) 5)
      3)

    (-add-edge edges '(11 1) 5 5)
    (-add-edge edges '(11 0) 5 6)

    (-del-edge edges 0 7)

    (do-test
      edges
      (make-array
        (list 10 2)
        :adjustable nil
        :initial-contents
          '((1 2) (1 5) (3 4) (10 3) (11 0) (11 1) (0 0) (0 0) (0 0) (0 0)
            )))))



(defun main ()
  (test-title (test-utils))
  (test-title (test-rnd))
  (test-title (test-bin))
  (test-title (test-summary)))

(main)

