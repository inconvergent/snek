
;http://cl-cookbook.sourceforge.net/os.html
(defun cmd-args ()
  (or #+SBCL *posix-argv*
      #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words*
       nil))


(defun 2* (l) (* l 2))


(defun 2+ (l) (+ l 2))


(defun half (l) (/ l 2))


(defmacro print-every (i n)
  `(if (= 0 (mod ,i ,n)) (print ,i)))


(defun make-dfloat-array (rows &key (cols 2) (initial 0.0d0))
  (make-array
    (list rows cols)
    :adjustable t
    :initial-element initial
    :element-type 'double-float ))


(defun make-int-array (rows  &key (cols 2) (initial 0))
  (make-array
    (list rows cols)
    :adjustable t
    :initial-element initial
    :element-type 'integer))


(defun arr (n &optional (adjustable nil))
  (make-array n :fill-pointer 0 :adjustable adjustable))


(defun to-list (aa)
  (loop for i from 0 below (length aa)
    collect (aref aa i)))


(defun to-int (x)
  (coerce x 'integer))


(defun to-int* (xx)
  (mapcar (lambda (x) (coerce x 'integer)) xx))


(defun to-dfloat (x)
  (coerce x 'double-float))


(defun to-dfloat* (xx)
  (mapcar (lambda (x) (coerce x 'double-float)) xx))


(defun x-or-large (x &optional (l 100000.0d0) (lim 0.00001d0))
  (if (> x lim) x l))

