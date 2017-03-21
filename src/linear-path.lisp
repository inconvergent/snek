
(defpackage :lin-path
  (:use :common-lisp)
  (:export
    :pos
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
    :make-float-array
    :to-float
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
(defun -find-seg-ind (lens ff n)
  (loop
    for ind from 0 below n
    until (< ff (aref lens ind 0))
    finally (return ind)))


(defun pos (path f)
  (with-struct (path- lens points n) path
    (let ((ff (mod (to-float f) 1.0)))
      (let ((ind (-find-seg-ind lens ff n)))
        (let ((pb (get-as-list points ind))
              (pa (get-as-list points (1- ind)))
              (s (diff-scale
                   (aref lens (1- ind) 0)
                   ff
                   (- (aref lens ind 0)
                      (aref lens (1- ind) 0)))))
          (add
            pa
            (scale (sub pb pa) s)))))))


(defun make (points)
  (let ((n (length points)))
    (let ((p (make-float-array n))
          (l (make-float-array n :cols 1)))
      (loop
        for d in points
        for i from 0
        do
          (set-from-list p i d))
      (-set-path-lens p l n)
      (make-path :n n :points p :lens l))))


(defun move (path rel)
  (with-struct (path- points lens n) path
    (loop
      for ab in rel
      for i from 0
      do
        (incf (aref points i 0) (first ab))
        (incf (aref points i 1) (second ab)))
    (-set-path-lens points lens n)))

