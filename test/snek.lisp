#!/usr/bin/sbcl --script

(load "../src/load")
(load "../src/test-utils")

(setf *print-pretty* t)
;(setf *random-state* (make-random-state t))



(defun test-snek (snk)
  (do-test
    (snek:add-vert! snk '(0 0))
    0)

  (do-test
    (snek:add-vert! snk '(10 0))
    1)

  (do-test
    (snek:add-vert! snk '(3 3))
    2)

  (do-test
    (snek:add-vert! snk '(4 3))
    3)

  (do-test
    (snek:add-vert! snk '(7 200))
    4)

  (do-test
    (snek:add-vert! snk '(2 10))
    5)

  (do-test
    (snek:add-vert! snk '(4 11))
    6)

  (do-test
    (snek:add-vert! snk '(3 10))
    7)

  (do-test
    (snek:add-vert! snk '(0 0.5))
    8)

  (do-test
    (snek:add-vert! snk '(2 1.0d0))
    9)

  (do-test
    (snek:add-vert! snk '(3.0d0 10))
    10)

  (do-test
    (snek:add-edge! snk '(0 0))
    nil)

  (do-test
    (snek:add-edge! snk '(0 2))
    '(0 2))

  (do-test
    (snek:add-edge! snk '(0 1))
    '(0 1))

  (do-test
    (snek:add-edge! snk '(5 0))
    '(0 5))

  (do-test
    (snek:add-edge! snk '(1 0))
    nil)

  (do-test
    (snek:add-edge! snk '(5 0))
    nil)

  (do-test
    (snek:add-edge! snk '(0 2))
    nil)

  (do-test
    (snek:add-edge! snk '(5 2))
    '(2 5))

  (do-test
    (snek:add-edge! snk '(4 1))
    '(1 4))

  (do-test
    (snek:add-edge! snk '(4 0))
    '(0 4))

  (do-test
    (snek:add-edge! snk '(5 1))
    '(1 5))

  (do-test
    (snek:add-edge! snk '(9 9))
    nil)

  (do-test
    (snek:add-edge! snk '(3 9))
    '(3 9))

  (do-test
    (snek:add-edge! snk '(0 1))
    nil)

  (do-test
    (snek:add-edge! snk '(0 4))
    nil)

  (do-test
    (snek:add-edge! snk '(10 9))
    '(9 10))

  (do-test
    (snek:get-vert snk 2)
    '(3.0 3.0))

  (do-test
    (snek:add-vert! snk '(0 1))
    11)

  (do-test
    (snek:add-edge! snk '(0 1))
    nil)

  (do-test
    (snek:add-vert! snk '(0 7))
    12)

  ;(do-test
  ;  (get-one-ring snk 5)
  ;  '((5 0) (5 1) (5 2)))

  ;(do-test
  ;  (get-one-ring snk 0)
  ;  '((0 1) (0 2) (0 4) (0 5)))

  (do-test
    (snek:edge-length snk '(0 4))
    200.12246250733574d0)

  (do-test
    (snek:edge-length snk '(2 5))
    7.0710678118654755d0)

  (do-test
    (snek:edge-length snk '(1 2))
    7.615773105863909d0))


(defun test-snek-2 (snk)
  (do-test
    (snek:add-vert! snk '(0 0))
    0)

  (do-test
    (snek:add-vert! snk '(20 20))
    1)

  (do-test
    (snek:add-vert! snk '(30 30))
    2)

  (do-test
    (snek:add-vert! snk '(40 40))
    3)

  (do-test
    (snek:add-edge! snk '(0 1))
    '(0 1))

  (do-test
    (snek:add-edge! snk '(1 2))
    '(1 2))

  (do-test
    (snek:add-edge! snk '(2 3))
    '(2 3))

  ;(do-test
  ;  (get-one-ring snk 0)
  ;  '((0 1)))

  ;(do-test
  ;  (get-one-ring snk 1)
  ;  '((1 0) (1 2)))

  (do-test
    (snek:get-edges snk)
    '#((0 1) (1 2) (2 3)))

  (do-test
    (snek:del-edge! snk '(0 1))
    t)

  (do-test
    (snek:del-edge! snk '(0 1))
    nil)

  (do-test
    (snek:del-edge! snk '(3 2))
    t)

  (do-test
    (snek:del-edge! snk '(1 2))
    t)

  (do-test
    (snek:get-num-edges snk)
    0)

  (do-test
    (snek::snek-num-verts snk)
    4)
  )


(defun test-snek-3 (snk)
  (do-test
    (snek:add-vert! snk '(10 10))
    0)
  (do-test
    (snek:add-vert! snk '(20 10))
    1)

  (do-test
    (snek:add-vert! snk '(30 10))
    2)

  (do-test
    (snek:add-vert! snk '(40 10))
    3)

  (do-test
    (snek:add-edge! snk '(0 1))
    '(0 1))
  (do-test
    (snek:add-edge! snk '(1 2))
    '(1 2))
  (do-test
    (snek:add-edge! snk '(2 3))
    '(2 3))
  (do-test
    (snek:add-edge! snk '(2 3))
    nil))


(defun init-snek ()
  (let ((snk (snek:make :max-verts 16)))
    (snek:add-vert! snk '(0 2))
    (snek:add-vert! snk '(2 3))
    (snek:add-vert! snk '(3 4))
    (snek:add-vert! snk '(4 7))
    (snek:add-vert! snk '(5 4))
    (snek:add-vert! snk '(0 6))
    (snek:add-vert! snk '(-1 7))
    (snek:add-vert! snk '(0 8))
    (snek:add-vert! snk '(0 9))
    (snek:add-vert! snk '(10 1))
    (snek:add-vert! snk '(3 1))

    (snek:add-edge! snk '(1 2))
    (snek:add-edge! snk '(0 1))
    (snek:add-edge! snk '(3 1))
    (snek:add-edge! snk '(5 6))
    (snek:add-edge! snk '(7 3))
    snk))


(defun test-snek-with ()
  (let ((snk (init-snek)))
    (snek:with (snk)

      (snek:add-vert? '(11 3))
      (list
        4.5
        (snek:move-vert? 0 '(1 0))
        nil
        t
        (list
          5
          (snek:add-vert? '(12 3))
          (snek:add-vert? '(13 3)))
        (list nil)
        (list (list))))

    (do-test
      (sort (snek:get-vert-inds snk) #'<)
      (list 0 1 2 3 5 6 7))))


(defun test-snek-add ()
  (let ((snk (init-snek)))
    (snek:with (snk)
      (snek:add-vert? '(10 3)))

    (do-test
      (snek:get-vert snk 11)
      '(10 3))

    (snek:with (snk)
      (snek:add-vert? '(80 3))
      (snek:add-vert? '(70 3)))

    (do-test
      (snek::snek-num-verts snk)
      14)

    (snek:with (snk)
      (snek:add-edge*? '(7 3) '(100 0.99)))

    (do-test
      (snek:get-edges snk)
      '#((1 2) (1 3) (0 1) (3 7) (5 6) (14 15)))))

(defun test-snek-move ()
  (let ((snk (init-snek)))
    (snek:with (snk)
      (snek:move-vert? 0 '(3 3))
      (snek:move-vert? 1 '(1 3))
      (snek:move-vert? 3 '(2 3) :rel nil)
      (snek:move-vert? 2 '(3 4)))

    (do-test
      (snek:get-vert snk 0)
      '(3 5))

    (do-test
      (snek:get-vert snk 1)
      '(3 6))

    (do-test
      (snek:get-vert snk 3)
      '(2 3))

    (do-test
      (snek:get-vert snk 2)
      '(6 8))))

(defun test-snek-join ()
  (let ((snk (init-snek)))
    (snek:with (snk)
      (snek:join-verts? 3 3)
      (snek:join-verts? 3 3)
      (snek:join-verts? 3 6)
      (snek:join-verts? 7 1))

  (do-test
    (snek:get-num-edges snk)
    14)))


(defun test-snek-append ()
  (let ((snk (init-snek)))

    (do-test
      (snek::snek-num-verts snk)
      11)

    (snek:with (snk)
      (snek:append-edge? 3 '(3 4))
      (snek:append-edge? 3 '(8 5) :rel nil)
      (snek:append-edge? 7 '(1 2)))

    (do-test
      (snek:get-num-edges snk)
      16)

    (do-test
      (snek::snek-num-verts snk)
      14)

    (do-test
      (snek::snek-verts snk)
     '#2A((0.0d0 2.0d0) (2.0d0 3.0d0) (3.0d0 4.0d0) (4.0d0 7.0d0) (5.0d0 4.0d0)
          (0.0d0 6.0d0) (-1.0d0 7.0d0) (0.0d0 8.0d0) (0.0d0 9.0d0) (10.0d0 1.0d0)
          (3.0d0 1.0d0) (7.0d0 11.0d0) (8.0d0 5.0d0) (1.0d0 10.0d0) (0.0d0 0.0d0)
          (0.0d0 0.0d0)))))


(defun test-snek-split ()
  (let ((snk (init-snek)))
    (snek:with (snk)
      (snek:split-edge? '(1 2))
      (snek:split-edge? '(1 2))
      (snek:split-edge? '(5 6)))

  (do-test
    (snek:get-num-edges snk)
    14)

  (do-test
    (snek::snek-num-verts snk)
    13)

  (do-test
    (snek::snek-verts snk)
    '#2A((0.0d0 2.0d0) (2.0d0 3.0d0) (3.0d0 4.0d0) (4.0d0 7.0d0) (5.0d0 4.0d0)
         (0.0d0 6.0d0) (-1.0d0 7.0d0) (0.0d0 8.0d0) (0.0d0 9.0d0) (10.0d0 1.0d0)
         (3.0d0 1.0d0) (2.5d0 3.5d0) (-0.5d0 6.5d0) (0.0d0 0.0d0) (0.0d0 0.0d0)
         (0.0d0 0.0d0)))))


(defun test-snek-itrs ()
  (let ((snk (init-snek)))
    (snek:with (snk)
      (snek:with-rnd-vert (snk v)
        (snek:append-edge? v (list 3 2))
        (snek:move-vert? v (list 2 2))))

    (do-test
      (snek:get-num-edges snk)
      12)

    (do-test
      (snek::snek-num-verts snk)
      12)

    (do-test
      (snek::snek-wc snk)
      1)

    (snek:with (snk)
      (snek:itr-all-verts (snk v)
        (snek:move-vert? v (list 2 2))))

    (do-test
      (sort (flatten (snek:itr-all-verts (snk i) i)) #'<)
      '(0 1 2 3 4 5 6 7 8 9 10 11))

    (do-test
      (sort (flatten (snek:itr-verts (snk i) i)) #'<)
      '(0 1 2 3 5 6 7 11))

    (do-test
      (snek:itr-edges (snk e)
        e)
      '(((1 2)) ((1 3)) ((0 1)) ((3 7)) ((5 6)) ((6 11))))

    (do-test
      (sort (flatten (snek:itr-edges (snk e) (snek:edge-length snk e))) #'<)
      '(1.0d0 1.4142135623730951d0 2.23606797749979d0 3.1622776601683795d0
        4.123105625617661d0 4.47213595499958d0))

    (do-test
      (snek::snek-wc snk)
      2)

    (snek:with (snk)
      (snek:with-rnd-edge (snk e)
        (snek:split-edge? e)))

    (do-test
      (snek:get-num-edges snk)
      14)

    (do-test
      (snek::snek-num-verts snk)
      13)

    (do-test
      (snek:with-prob 1.0 1)
      '(1))

    (do-test
      (snek:with-prob 0.5 1)
      nil)))

(defun test-snek-zmap ()
  (let ((snk (snek:make)))

    (snek:add-vert! snk '(100 200))
    (snek:add-vert! snk '(200 300))
    (snek:add-vert! snk '(300 400))
    (snek:add-vert! snk '(400 500))
    (snek:add-vert! snk '(500 600))
    (snek:add-vert! snk '(600 700))
    (snek:add-vert! snk '(700 800))
    (snek:add-vert! snk '(800 900))

    (zmap:make (snek::snek-verts snk) (snek::snek-num-verts snk) 100.0d0)

    (snek:with (snk :zwidth 50.0d0)
      (do-test
        (sort (snek:verts-in-rad snk (list 500 500) 50.0d0) #'<)
        #())

      (do-test
        (sort (snek:verts-in-rad snk (list -500 500) 50.0d0) #'<)
        #()))

    (snek:with (snk :zwidth 200.0d0)
      (do-test
        (sort (snek:verts-in-rad snk (list 800 800) 200.0d0) #'<)
        #(6 7))

      (do-test
        (sort (snek:verts-in-rad snk (list 500 500) 200.0d0) #'<)
        #(3 4)))

    (snek:with (snk :zwidth 1000.0d0)
      (do-test
        (sort (snek:verts-in-rad snk (list 500 500) 1000.0d0) #'<)
        #(0 1 2 3 4 5 6 7)))))


(defun test-snek-grp ()
  (let ((snk (snek:make :max-verts 22 :grp-size 30)))

    (let ((g1 (snek:add-grp! snk :type 'path :closed t))
          (g2 (snek:add-grp! snk))
          (g3 (snek:add-grp! snk :type 'path :closed t)))
      (snek:add-vert! snk '(100 200))
      (snek:add-vert! snk '(200 300))
      (snek:add-vert! snk '(300 400))
      (snek:add-vert! snk '(400 500))
      (snek:add-vert! snk '(600 700))
      (snek:add-vert! snk '(700 800))
      (snek:add-vert! snk '(800 900))
      (snek:add-vert! snk '(500 600))
      (snek:add-vert! snk '(900 600))

      (snek:add-edge! snk '(1 2) :g g1)
      (snek:add-edge! snk '(1 2))
      (snek:add-edge! snk '(1 2) :g g2)
      (snek:add-edge! snk '(3 2) :g g2)
      (snek:add-edge! snk '(1 5) :g g3)

      (do-test
        (sort (flatten (snek:itr-verts (snk i :g g2) i)) #'<)
        '(1 2 3))

      (do-test
        (sort (flatten (snek:itr-verts (snk i :g nil) i)) #'<)
        '(1 2))

      (do-test
        (sort (flatten (snek:itr-edges (snk e :g g1) e)) #'<)
        '(1 2))

      (do-test
        (sort (snek:get-vert-inds snk :g g1) #'<)
        '(1 2))

      (do-test
        (sort (snek:get-vert-inds snk :g g3) #'<)
        '(1 5))

      (do-test
        (length (snek:get-vert-inds snk))
        2)

      (do-test
        (length (snek:itr-grps (snk g) g))
        3))))


(defun main ()
  (test-title (test-snek (snek:make)))
  (test-title (test-snek-2 (snek:make)))
  (test-title (test-snek-3 (snek:make)))
  (test-title (test-snek-with))
  (test-title (test-snek-add))
  (test-title (test-snek-move))
  (test-title (test-snek-join))
  (test-title (test-snek-append))
  (test-title (test-snek-split))
  (test-title (test-snek-itrs))
  (test-title (test-snek-zmap))
  (test-title (test-snek-grp))
  (test-title (test-summary)))

(main)

