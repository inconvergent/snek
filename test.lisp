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
          (format t "~%~a ~%--> not ok. ~%--  wanted: ~% ~a ~%--  got: ~% ~a"
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
    (lenn '(1 2))
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
    (lget '(0 2) '((1 2) (3 4) (5 6)))
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

    (-insert-edge edges '(11 1) 5 5)
    (-insert-edge edges '(11 0) 5 6)

    (-remove-edge edges 0 7)

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
    (insert-vert snk '(0 0))
    0)

  (do-test
    (insert-vert snk '(10 0))
    1)

  (do-test
    (insert-vert snk '(3 3))
    2)

  (do-test
    (insert-vert snk '(4 3))
    3)

  (do-test
    (insert-edge snk '(0 0))
    nil)

  (do-test
    (insert-edge snk '(0 2))
    '(0 2))

  (do-test
    (insert-edge snk '(0 1))
    '(0 1))

  (do-test
    (insert-edge snk '(5 0))
    '(0 5))

  (do-test
    (insert-edge snk '(1 0))
    nil)

  (do-test
    (insert-edge snk '(5 0))
    nil)

  (do-test
    (insert-edge snk '(0 2))
    nil)

  (do-test
    (insert-edge snk '(5 2))
    '(2 5))

  (do-test
    (insert-edge snk '(4 1))
    '(1 4))

  (do-test
    (insert-edge snk '(4 0))
    '(0 4))

  (do-test
    (insert-edge snk '(5 1))
    '(1 5))

  (do-test
    (insert-edge snk '(100 100))
    nil)

  (do-test
    (insert-edge snk '(3 100))
    '(3 100))

  (do-test
    (insert-edge snk '(0 1))
    nil)

  (do-test
    (insert-edge snk '(0 4))
    nil)

  (do-test
    (insert-edge snk '(100 99))
    '(99 100))

  (do-test
    (get-vert snk 2)
    '(3.0 3.0))

  (do-test
    (insert-vert snk '(0 1))
    4)

  (do-test
    (insert-edge snk '(0 1))
    nil)

  (do-test
    (insert-vert snk '(0 7))
    5)

  (do-test
    (get-one-ring snk 5)
    '((5 0) (5 1) (5 2)))

  (do-test
    (get-one-ring snk 0)
    '((0 1) (0 2) (0 4) (0 5)))

  (do-test
    (edge-length snk '(0 4))
    1.0)

  (do-test
    (edge-length snk '(2 5))
    5.0)

  (do-test
    (edge-length snk '(1 2))
    7.615773105863909d0))


(defun test-snek-2 (snk)
  (do-test
    (insert-vert snk '(0 0))
    0)

  (do-test
    (insert-vert snk '(20 20))
    1)

  (do-test
    (insert-vert snk '(30 30))
    2)

  (do-test
    (insert-vert snk '(40 40))
    3)

  (do-test
    (insert-edge snk '(0 1))
    '(0 1))

  (do-test
    (insert-edge snk '(1 2))
    '(1 2))

  (do-test
    (insert-edge snk '(2 3))
    '(2 3))

  (do-test
    (-binary-edge-insert-search (snek-edges snk) '(1 2) 2)
    2)

  (do-test
    (get-one-ring snk 0)
    '((0 1)))

  (do-test
    (get-one-ring snk 1)
    '((1 0) (1 2)))

  (do-test
    (remove-edge snk '(0 1))
    2)

  (do-test
    (remove-edge snk '(0 1))
    0)

  (do-test
    (remove-edge snk '(3 2))
    2)

  (do-test
    (remove-edge snk '(1 2))
    2)

  (do-test
    (snek-num-edges snk)
    0)

  (do-test
    (snek-num-verts snk)
    4))


(defun test-snek-3 (snk)
  (do-test
    (insert-vert snk '(10 10))
    0)
  (do-test
    (insert-vert snk '(20 10))
    1)

  (do-test
    (insert-vert snk '(30 10))
    2)

  (do-test
    (insert-vert snk '(40 10))
    3)

  (do-test
    (insert-edge snk '(0 1))
    '(0 1))
  (do-test
    (insert-edge snk '(1 2))
    '(1 2))
  (do-test
    (insert-edge snk '(2 3))
    '(2 3))
  (do-test
    (insert-edge snk '(2 3))
    nil)

  (do-test
    (-binary-edge-search
      (snek-edges snk)
      '(2 3)
      (snek-num-edges snk))
    4)

  (do-test
    (-binary-edge-search
      (snek-edges snk)
      '(0 1)
      (snek-num-edges snk))
    0)

  (do-test
    (-binary-edge-search
      (snek-edges snk)
      '(10 1)
      (snek-num-edges snk))
    nil))

(defun init-snek ()
  (let ((snk (snek* 16)))
    (insert-vert snk '(0 2))
    (insert-vert snk '(2 3))
    (insert-vert snk '(3 4))
    (insert-vert snk '(4 7))
    (insert-vert snk '(5 4))
    (insert-vert snk '(0 6))
    (insert-vert snk '(-1 7))
    (insert-vert snk '(0 8))
    (insert-vert snk '(0 9))
    (insert-vert snk '(10 1))

    (insert-edge snk '(1 2))
    (insert-edge snk '(0 1))
    (insert-edge snk '(3 1))
    (insert-edge snk '(5 6))
    (insert-edge snk '(7 3))
    snk))

(defun test-snek-move ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (move-vert 0 '(3 3))
      (move-vert 1 '(1 3))
      (move-vert 3 '(2 3) :rel nil)
      (move-vert 2 '(3 4)))

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
      (join-verts 3 3)
      (join-verts 3 3)
      (join-verts 3 6)
      (join-verts 7 1))

  (do-test
    (snek-num-edges snk)
    14)

  (do-test
    (snek-edges snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0 1) (1 0) (1 2) (1 3) (1 7) (2 1) (3 1) (3 6)
          (3 7) (5 6) (6 3) (6 5) (7 1) (7 3) (0 0) (0 0))))))


(defun test-snek-append ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (append-edge 3 '(3 4))
      (append-edge 3 '(8 5) :rel nil)
      (append-edge 7 '(1 2)))

  (do-test
    (snek-num-edges snk)
    16)

  (do-test
    (snek-num-verts snk)
    13)

  (do-test
    (snek-edges snk)
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
          (-1.0 7.0) (0.0 8.0) (0.0 9.0) (10.0 1.0) (7.0 11.0) (8.0 5.0)
          (1.0 10.0) (0.0 0.0) (0.0 0.0) (0.0 0.0))))))


(defun test-snek-split ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (split-edge '(1 2))
      (split-edge '(1 2))
      (split-edge '(5 6)))

  (do-test
    (snek-num-edges snk)
    14)

  (do-test
    (snek-num-verts snk)
    12)

  (do-test
    (snek-edges snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0 1) (1 0) (1 3) (1 10) (2 10) (3 1) (3 7) (5 11) (6 11)
          (7 3) (10 1) (10 2) (11 5) (11 6) (0 0) (0 0))))

  (do-test
    (snek-verts snk)
    (make-array
      (list 16 2)
      :adjustable nil
      :initial-contents
        '((0.0 2.0) (2.0 3.0) (3.0 4.0) (4.0 7.0) (5.0 4.0) (0.0 6.0)
          (-1.0 7.0) (0.0 8.0) (0.0 9.0) (10.0 1.0) (2.5 3.5) (-0.5 6.5)
          (0.0 0.0) (0.0 0.0) (0.0 0.0) (0.0 0.0))))))


(defun test-snek-withs ()
  (let ((snk (init-snek)))
    (with-snek (snk)
      (with-rnd-vert (snk v)
        (append-edge v (list 3 2))
        (move-vert v (list 2 2))))

    (do-test
      (snek-num-edges snk)
      12)

    (do-test
      (snek-num-verts snk)
      11)

    (do-test
      (snek-wc snk)
      1)

    (with-snek (snk)
      (with-all-verts (snk v)
        (move-vert v (list 2 2))))

    (do-test
      (snek-wc snk)
      2)

    (with-snek (snk)
      (with-rnd-edge (snk e)
        (split-edge e)))

    (do-test
      (snek-num-edges snk)
      14)

    (do-test
      (snek-num-verts snk)
      12)))

(defun test-snek-zmap ()
  (let ((snk (snek*)))

    (insert-vert snk '(100 200))
    (insert-vert snk '(200 300))
    (insert-vert snk '(300 400))
    (insert-vert snk '(400 500))
    (insert-vert snk '(500 600))
    (insert-vert snk '(600 700))
    (insert-vert snk '(700 800))
    (insert-vert snk '(800 900))

    (zmap-update snk 100.0d0)

    (do-test
      (sort (verts-in-rad snk (list 500 500) 50.0d0) #'<)
      #())

    (do-test
      (sort (verts-in-rad snk (list 800 800) 200.0d0) #'<)
      #(6 7))

    (do-test
      (sort (verts-in-rad snk (list -500 500) 50.0d0) #'<)
      #())

    (zmap-update snk 1000.0d0)

    (do-test
      (sort (verts-in-rad snk (list 500 500) 1000.0d0) #'<)
      #(0 1 2 3 4 5 6 7))

    (do-test
      (sort (verts-in-rad snk (list 500 500) 200.0d0) #'<)
      #(3 4))))


(defun summary ()
  (format t "~% tests:  ~a~% fails:  ~a~% passes: ~a~%"
          *tests* *fails* *passes*))


(defun main ()

  (title (test-utils))
  (title (test-bin))
  (title (test-snek (snek*)))
  (title (test-snek-2 (snek*)))
  (title (test-snek-3 (snek*)))
  (title (test-snek-move))
  (title (test-snek-join))
  (title (test-snek-append))
  (title (test-snek-split))
  (title (test-snek-withs))
  (title (test-snek-zmap))
  (title (summary)))

(main)
