#!/usr/bin/sbcl --script

(load "src/load")

(setf *print-pretty* t)
;(setf *random-state* (make-random-state t))


(defvar *tests* 0)
(defvar *fails* 0)
(defvar *passes* 0)


; TODO: approximately similar to


(defmacro title (&body body)
  `(progn
     (format t "~%~a ##############~%" ',@body)
     ,@body))


(defmacro do-test (a &optional (b nil))
  (with-gensyms (aname bname)
    (incf *tests*)
    `(let ((,aname ,a)
           (,bname ,b))
      (if (funcall #'equalp ,aname ,bname)
        (progn
          (incf *passes*)
          (format t "~%~a ~%--> ok" ',a))
        (progn
          (incf *fails*)
          (format t "~%~a ~%#-> not ok. #################################### ~%--  wanted: ~% ~a ~%--  got: ~% ~a"
            ',a
            ',b
            ,aname)))
      (format t "~%---------------------------~%"))))


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


(defun test-snek (snk)
  (do-test
    (add-vert! snk '(0 0))
    0)

  (do-test
    (add-vert! snk '(10 0))
    1)

  (do-test
    (add-vert! snk '(3 3))
    2)

  (do-test
    (add-vert! snk '(4 3))
    3)

  (do-test
    (add-vert! snk '(7 200))
    4)

  (do-test
    (add-vert! snk '(2 10))
    5)

  (do-test
    (add-vert! snk '(4 11))
    6)

  (do-test
    (add-vert! snk '(3 10))
    7)

  (do-test
    (add-vert! snk '(0 0.5))
    8)

  (do-test
    (add-vert! snk '(2 1.0d0))
    9)

  (do-test
    (add-vert! snk '(3.0d0 10))
    10)

  (do-test
    (add-edge! snk '(0 0))
    nil)

  (do-test
    (add-edge! snk '(0 2))
    '(0 2))

  (do-test
    (add-edge! snk '(0 1))
    '(0 1))

  (do-test
    (add-edge! snk '(5 0))
    '(0 5))

  (do-test
    (add-edge! snk '(1 0))
    nil)

  (do-test
    (add-edge! snk '(5 0))
    nil)

  (do-test
    (add-edge! snk '(0 2))
    nil)

  (do-test
    (add-edge! snk '(5 2))
    '(2 5))

  (do-test
    (add-edge! snk '(4 1))
    '(1 4))

  (do-test
    (add-edge! snk '(4 0))
    '(0 4))

  (do-test
    (add-edge! snk '(5 1))
    '(1 5))

  (do-test
    (add-edge! snk '(9 9))
    nil)

  (do-test
    (add-edge! snk '(3 9))
    '(3 9))

  (do-test
    (add-edge! snk '(0 1))
    nil)

  (do-test
    (add-edge! snk '(0 4))
    nil)

  (do-test
    (add-edge! snk '(10 9))
    '(9 10))

  (do-test
    (get-vert snk 2)
    '(3.0 3.0))

  (do-test
    (add-vert! snk '(0 1))
    11)

  (do-test
    (add-edge! snk '(0 1))
    nil)

  (do-test
    (add-vert! snk '(0 7))
    12)

  (do-test
    (get-one-ring snk 5)
    '((5 0) (5 1) (5 2)))

  (do-test
    (get-one-ring snk 0)
    '((0 1) (0 2) (0 4) (0 5)))

  (do-test
    (edge-length snk '(0 4))
    200.12246250733574d0)

  (do-test
    (edge-length snk '(2 5))
    7.0710678118654755d0)

  (do-test
    (edge-length snk '(1 2))
    7.615773105863909d0))


(defun test-snek-2 (snk)
  (do-test
    (add-vert! snk '(0 0))
    0)

  (do-test
    (add-vert! snk '(20 20))
    1)

  (do-test
    (add-vert! snk '(30 30))
    2)

  (do-test
    (add-vert! snk '(40 40))
    3)

  (do-test
    (add-edge! snk '(0 1))
    '(0 1))

  (do-test
    (add-edge! snk '(1 2))
    '(1 2))

  (do-test
    (add-edge! snk '(2 3))
    '(2 3))

  (do-test
    (-binary-edge-insert-search
      (get-edge-arr snk) '(1 2) 2)
    2)

  (do-test
    (get-one-ring snk 0)
    '((0 1)))

  (do-test
    (get-one-ring snk 1)
    '((1 0) (1 2)))

  (do-test
    (get-edges snk)
    '((0 1) (1 0) (1 2) (2 1) (2 3) (3 2)))

  (do-test
    (del-edge! snk '(0 1))
    2)

  (do-test
    (del-edge! snk '(0 1))
    0)

  (do-test
    (del-edge! snk '(3 2))
    2)

  (do-test
    (del-edge! snk '(1 2))
    2)

  (do-test
    (get-num-edges snk)
    0)

  (do-test
    (snek-num-verts snk)
    4)
  )


(defun test-snek-3 (snk)
  (do-test
    (add-vert! snk '(10 10))
    0)
  (do-test
    (add-vert! snk '(20 10))
    1)

  (do-test
    (add-vert! snk '(30 10))
    2)

  (do-test
    (add-vert! snk '(40 10))
    3)

  (do-test
    (add-edge! snk '(0 1))
    '(0 1))
  (do-test
    (add-edge! snk '(1 2))
    '(1 2))
  (do-test
    (add-edge! snk '(2 3))
    '(2 3))
  (do-test
    (add-edge! snk '(2 3))
    nil)

  (do-test
    (-binary-edge-search
      (get-edge-arr snk)
      '(2 3)
      (get-num-edges snk))
    4)

  (do-test
    (-binary-edge-search
      (get-edge-arr snk)
      '(0 1)
      (get-num-edges snk))
    0)

  (do-test
    (-binary-edge-search
      (get-edge-arr snk)
      '(10 1)
      (get-num-edges snk))
    nil))

(defun init-snek ()
  (let ((snk (make-snek
                :max-verts 16
                :max-main-grp-edges 16)))
    (add-vert! snk '(0 2))
    (add-vert! snk '(2 3))
    (add-vert! snk '(3 4))
    (add-vert! snk '(4 7))
    (add-vert! snk '(5 4))
    (add-vert! snk '(0 6))
    (add-vert! snk '(-1 7))
    (add-vert! snk '(0 8))
    (add-vert! snk '(0 9))
    (add-vert! snk '(10 1))
    (add-vert! snk '(3 1))

    (add-edge! snk '(1 2))
    (add-edge! snk '(0 1))
    (add-edge! snk '(3 1))
    (add-edge! snk '(5 6))
    (add-edge! snk '(7 3))
    snk))

(defun test-snek-add ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (add-vert? '(10 3)))

    (do-test
      (get-vert snk 11)
      '(10 3))

    (with-snek (snk)
      (add-vert? '(80 3))
      (add-vert? '(70 3)))

    (do-test
      (snek-num-verts snk)
      14)

    (with-snek (snk)
      (add-edge? '(4 3))
      (add-edge? '(4 3))
      (add-edge? '(2 3)))

    (with-snek (snk)
      (add-edge*? '(7 3) '(100 0.99)))

    (do-test
      (get-edges snk)
      '((0 1) (1 0) (1 2) (1 3) (2 1) (2 3) (3 1) (3 2)
        (3 4) (3 7) (4 3) (5 6) (6 5) (7 3) (14 15) (15 14)))))

(defun test-snek-move ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (move-vert? 0 '(3 3))
      (move-vert? 1 '(1 3))
      (move-vert? 3 '(2 3) :rel nil)
      (move-vert? 2 '(3 4)))

    (do-test
      (get-vert snk 0)
      '(3 5))

    (do-test
      (get-vert snk 1)
      '(3 6))

    (do-test
      (get-vert snk 3)
      '(2 3))

    (do-test
      (get-vert snk 2)
      '(6 8))))

(defun test-snek-join ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (join-verts? 3 3)
      (join-verts? 3 3)
      (join-verts? 3 6)
      (join-verts? 7 1))

  (do-test
    (get-num-edges snk)
    14)

  (do-test
    (get-edge-arr snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0 1) (1 0) (1 2) (1 3) (1 7) (2 1) (3 1) (3 6)
          (3 7) (5 6) (6 3) (6 5) (7 1) (7 3) (0 0) (0 0))))))


(defun test-snek-append ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (append-edge? 3 '(3 4))
      (append-edge? 3 '(8 5) :rel nil)
      (append-edge? 7 '(1 2)))

  (do-test
    (get-num-edges snk)
    16)

  (do-test
    (snek-num-verts snk)
    14)

  (do-test
    (get-edge-arr snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0 1) (1 0) (1 2) (1 3) (2 1) (3 1) (3 7) (3 10)
          (3 11) (5 6) (6 5) (7 3) (7 12) (10 3) (11 3) (12 7))))

  (do-test
    (snek-verts snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0.0 2.0) (2.0 3.0) (3.0 4.0) (4.0 7.0) (5.0 4.0) (0.0 6.0)
          (-1.0 7.0) (0.0 8.0) (0.0 9.0) (10.0 1.0) (3.0 1.0) (7.0 11.0)
          (8.0 5.0) (1.0 10.0) (0.0 0.0) (0.0 0.0))
        ))))


(defun test-snek-split ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (split-edge? '(1 2))
      (split-edge? '(1 2))
      (split-edge? '(5 6)))

  (do-test
    (get-num-edges snk)
    14)

  (do-test
    (snek-num-verts snk)
    13)

  (do-test
    (get-edge-arr snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0 1) (1 0) (1 3) (1 11) (2 11) (3 1) (3 7) (5 12) (6 12)
          (7 3) (11 1) (11 2) (12 5) (12 6) (0 0) (0 0))))

  (do-test
    (snek-verts snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0.0 2.0) (2.0 3.0) (3.0 4.0) (4.0 7.0) (5.0 4.0) (0.0 6.0)
          (-1.0 7.0) (0.0 8.0) (0.0 9.0) (10.0 1.0) (3.0 1.0) (2.5 3.5)
          (-0.5 6.5) (0.0 0.0) (0.0 0.0) (0.0 0.0))))))


(defun test-snek-withs ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (with-rnd-vert (snk v)
        (append-edge? v (list 3 2))
        (move-vert? v (list 2 2))))

    (do-test
      (get-num-edges snk)
      12)

    (do-test
      (snek-num-verts snk)
      12)

    (do-test
      (snek-wc snk)
      1)

    (with-snek (snk)
      (itr-verts (snk v)
        (move-vert? v (list 2 2))))

    (do-test
      (itr-verts (snk i) i)
      '((0) (1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11)))

    (do-test
      (itr-edges (snk e) e)
      '(((0 1)) ((1 2)) ((1 3)) ((3 7)) ((5 6)) ((6 10))))

    (do-test
      (itr-edges (snk e) (edge-length snk e))
      '((2.23606797749979d0) (1.4142135623730951d0)
        (4.47213595499958d0) (4.123105625617661d0)
        (3.1622776601683795d0) (8.246211251235321d0)))

    (do-test
      (snek-wc snk)
      2)

    (with-snek (snk)
      (with-rnd-edge (snk e)
        (split-edge? e)))

    (do-test
      (get-num-edges snk)
      14)

    (do-test
      (snek-num-verts snk)
      13)))

(defun test-snek-zmap ()
  (let ((snk (make-snek)))

    (add-vert! snk '(100 200))
    (add-vert! snk '(200 300))
    (add-vert! snk '(300 400))
    (add-vert! snk '(400 500))
    (add-vert! snk '(500 600))
    (add-vert! snk '(600 700))
    (add-vert! snk '(700 800))
    (add-vert! snk '(800 900))

    (zmap:make (snek-verts snk) (snek-num-verts snk) 100.0d0)

    (with-snek (snk :zwidth 50.0d0)
      (do-test
        (sort (verts-in-rad snk (list 500 500) 50.0d0) #'<)
        #())

      (do-test
        (sort (verts-in-rad snk (list -500 500) 50.0d0) #'<)
        #()))

    (with-snek (snk :zwidth 200.0d0)
      (do-test
        (sort (verts-in-rad snk (list 800 800) 200.0d0) #'<)
        #(6 7))

      (do-test
        (sort (verts-in-rad snk (list 500 500) 200.0d0) #'<)
        #(3 4)))

    (with-snek (snk :zwidth 1000.0d0)
      (do-test
        (sort (verts-in-rad snk (list 500 500) 1000.0d0) #'<)
        #(0 1 2 3 4 5 6 7)))))


(defun test-snek-grp ()
  (let ((snk (make-snek
               :max-verts 22
               :max-main-grp-edges 20
               :max-grp-edges 21)))

    (let ((g1 (add-grp! snk :type 'path :closed t))
          (g2 (add-grp! snk))
          (g3 (add-grp! snk :type 'path :closed t)))
      (add-vert! snk '(100 200) :g g1)
      (add-vert! snk '(200 300) :g g1)
      (add-vert! snk '(300 400) :g (add-grp! snk))
      (add-vert! snk '(400 500) :g (add-grp! snk))
      (add-vert! snk '(600 700) :g g2)
      (add-vert! snk '(700 800) :g g3)
      (add-vert! snk '(800 900) :g g1)
      (add-vert! snk '(500 600))
      (add-vert! snk '(900 600))

      (do-test
        (flatten (itr-verts (snk i :g g2) i))
        '(4))

      (do-test
        (flatten (itr-verts (snk i :g nil) i))
        '(7 8))

      (do-test
        (itr-edges (snk e :g g1) e)
        '())

      (do-test
        (get-vert-grp snk 0)
        g1)

      (do-test
        (get-vert-grp snk 4)
        g2)

      (do-test
        (get-vert-grp snk 5)
        g3)

      (do-test
        (get-vert-grp snk 6)
        g1)

      (do-test
        (get-vert-grp snk 8)
        nil)

      (do-test
        (get-grp-verts snk :g g1)
        '(0 1 6))

      (do-test
        (get-grp-verts snk :g g3)
        '(5))

      (do-test
        (length (get-grp-verts snk))
        2)

      (do-test
        (length (itr-grps (snk g) g))
        5)

      (do-test
        (with-prob 1.0 1)
        '(1))

      (do-test
        (with-prob 0.5 1)
        nil)

      (do-test
        (array-dimensions (grp-edges (gethash nil (snek-grps snk))))
        '(20 2))

      (do-test
        (array-dimensions (grp-edges (gethash g1 (snek-grps snk))))
        '(21 2))

      (do-test
        (array-dimensions (snek-verts snk))
        '(22 2)))))


(defun summary ()
  (format t "~% tests:  ~a~% fails:  ~a~% passes: ~a~%"
          *tests* *fails* *passes*))


(defun main ()
  (title (test-utils))
  (title (test-bin))
  (title (test-snek (make-snek)))
  (title (test-snek-2 (make-snek)))
  (title (test-snek-3 (make-snek)))
  (title (test-snek-add))
  (title (test-snek-move))
  (title (test-snek-join))
  (title (test-snek-append))
  (title (test-snek-split))
  (title (test-snek-withs))
  (title (test-snek-zmap))
  (title (test-snek-grp))
  (title (summary)))

(main)

