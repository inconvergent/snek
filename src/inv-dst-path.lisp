(defpackage :inv-dst-path
  (:use :common-lisp)
  (:import-from :lin-path
    :-find-seg-ind
    :-set-path-lens
    :move
    :make)
  (:import-from :common-lisp-user
    :diff-scale
    :get-as-list
    :add
    :scale
    :to-dfloat
    :range
    :x-or-large
    :with-gensyms
    :with-struct)
  (:export
    :pos
    :make
    :move))

(in-package :inv-dst-path)


(defstruct path
  (n nil :type integer :read-only t)
  (lens nil)
  (points nil))

(defun  -get-dst (lens inds f)
  (mapcar
    (lambda (i) (x-or-large (abs (- (aref lens i 0) f))))
    inds))


(defun -i-weight (d &optional (p 2.0d0))
  (/ 1.0d0 (expt d p)))


(defun -get-inv-weights (dd)
  (let ((iws (mapcar #'-i-weight dd)))
    (let ((tt (reduce #'+ iws)))
      (mapcar (lambda (w) (/ w tt)) iws))))


(defun pos (path f)
  (with-struct (path- lens points n) path
    (let ((ff (mod (to-dfloat f) 1.0d0))
          (inds (range n)))
      (reduce
        #'add
        (mapcar
          (lambda (w i) (scale (get-as-list points i) w))
          (-get-inv-weights (-get-dst lens inds ff))
          inds)))))

