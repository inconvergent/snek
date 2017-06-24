
(defpackage :lin-path
  (:use :common-lisp)
  (:export
    :pos
    :pos*
    :rndpos
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
  (closed nil :type boolean)
  (pts nil))


(defun -set-path-lens (pts lens n)
  (let ((total (loop
                  for i from 0 below (1- n)
                  sum (dst
                        (get-as-list pts i)
                        (get-as-list pts (1+ i))) into total
                  do
                    (setf (aref lens (1+ i) 0) total)
                  finally
                    (return total))))
    (loop for i from 1 below n do
      (setf (aref lens i 0)
            (/ (aref lens i 0) total)))))


; TODO: binary search
(defun -find-seg-ind (lens f n)
  (loop
    for ind from 0 below n
    until (< f (aref lens ind 0))
    finally (return ind)))


(defun -calc-pos (pts lens n f)
  (let ((ind (-find-seg-ind lens f n)))
        (let ((pb (get-as-list pts ind))
              (pa (get-as-list pts (1- ind)))
              (s (diff-scale
                   (aref lens (1- ind) 0)
                   f
                   (- (aref lens ind 0)
                      (aref lens (1- ind) 0)))))
          (add
            pa
            (scale (sub pb pa) s)))))


(defun pos (path f)
  (with-struct (path- lens pts n) path
    (-calc-pos pts lens n (mod (to-dfloat f) 1.0d0))))


(defun pos* (path ff)
  (with-struct (path- lens pts n) path
    (mapcar
      (lambda (f) (-calc-pos pts lens n (mod f 1.0d0)))
      (to-dfloat* ff))))


(defmacro rndpos (path n)
  `(pos* ,path (rnd:rndspace 0.0d0 1.0d0 ,n)))


(defun make (pts &key closed &aux (n (length pts)))
  (let ((p (make-dfloat-array n))
        (l (make-dfloat-array n :cols 1)))
    (loop for d in pts and i from 0 do
      (set-from-list p i (to-dfloat* d)))
    (-set-path-lens p l n)
    (make-path :n n :pts p :lens l :closed closed)))


(defun -move-rel (pts i xy)
  (destructuring-bind (x y)
    xy
    (incf (aref pts i 0) x)
    (incf (aref pts i 1) y)))


(defun -move (pts i xy)
  (destructuring-bind (x y)
    xy
    (setf (aref pts i 0) x
          (aref pts i 1) y)))


(defun move (path pos &key rel)
  (let ((do-move (if rel #'-move-rel #'-move)))
    (with-struct (path- pts lens n closed) path
      (loop for xy in pos and i from 0 do
        (funcall do-move pts i xy))

      (if closed
        (setf (aref pts (1- n) 0) (aref pts 0 0)
              (aref pts (1- n) 1) (aref pts 0 1)))

      (-set-path-lens pts lens n))))

