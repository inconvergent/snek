
(defpackage :lin-path
  (:use :common-lisp)
  (:export
    :pos
    :pos*
    :make
    :move)
  (:import-from :common-lisp-user
    :diff-scale
    :get-as-list
    :set-from-list
    :add
    :dst
    :scale
    :sub
    :make-dfloat-array
    :to-dfloat
    :to-dfloat*
    :with-struct))

(in-package :lin-path)

(defstruct path
  (n nil :type integer :read-only t)
  (lens nil)
  (points nil))


(defun -set-path-lens (points lens n)
  (let ((total (loop
                  for i from 0 below (1- n)
                  sum (dst
                        (get-as-list points i)
                        (get-as-list points (1+ i))) into total
                  do
                    (setf (aref lens (1+ i) 0) total)
                  finally
                    (return total))))
    (loop
      for i from 1 below n
      do
        (setf
          (aref lens i 0)
          (/ (aref lens i 0) total)))))


; TODO: binary search
(defun -find-seg-ind (lens f n)
  (loop
    for ind from 0 below n
    until (< f (aref lens ind 0))
    finally (return ind)))


(defun -calc-pos (points lens n f)
  (let ((ind (-find-seg-ind lens f n)))
        (let ((pb (get-as-list points ind))
              (pa (get-as-list points (1- ind)))
              (s (diff-scale
                   (aref lens (1- ind) 0)
                   f
                   (- (aref lens ind 0)
                      (aref lens (1- ind) 0)))))
          (add
            pa
            (scale (sub pb pa) s)))))


(defun pos (path f)
  (with-struct (path- lens points n) path
    (-calc-pos points lens n (mod (to-dfloat f) 1.0d0))))


(defun pos* (path ff)
  (with-struct (path- lens points n) path
    (mapcar
      (lambda (f) (-calc-pos points lens n (mod f 1.0d0)))
      (to-dfloat* ff))))


(defun make (points)
  (let ((n (length points)))
    (let ((p (make-dfloat-array n))
          (l (make-dfloat-array n :cols 1)))
      (loop
        for d in points
        for i from 0
        do
          (set-from-list p i d))
      (-set-path-lens p l n)
      (make-path :n n :points p :lens l))))


(defun move (path rel &optional (closed nil))
  (with-struct (path- points lens n) path
    (loop
      for ab in rel
      for i from 0
      do
        (incf (aref points i 0) (first ab))
        (incf (aref points i 1) (second ab)))

    (if closed
      (progn
        (setf (aref points (1- n) 0) (aref points 0 0))
        (setf (aref points (1- n) 1) (aref points 0 1))))

    (-set-path-lens points lens n)))

