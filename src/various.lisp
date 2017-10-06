
(defvar PII (* PI 2d0))


;http://cl-cookbook.sourceforge.net/os.html
(defun cmd-args ()
  (or #+SBCL *posix-argv*
      #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words*
      nil))


(defun append-postfix (fn postfix)
  (concatenate 'string fn postfix))


(defun append-number (fn i)
  (format nil "~a-~8,'0d" fn i))


(defmacro print-every (i n)
  `(if (= 0 (mod ,i ,n)) (format t "~%itt: ~a~%" ,i)))


(defun dhalf (l)
  (declare (double-float l))
  (* l 0.5d0))


(defun half (l)
  (/ l 2))


(defun make-dfloat-array (rows &key (cols 2) (initial 0.0d0))
  (make-array (list rows cols) :initial-element initial :element-type 'double-float))


(defun make-symb-array (rows &key (initial nil))
  (make-array rows :initial-element initial :element-type 'symbol))


(defun make-int-array (rows  &key (cols 2) (initial 0))
  (make-array (list rows cols) :initial-element initial :element-type 'integer))


(defun make-vec (&optional (s 100))
  (make-array s :fill-pointer 0 :initial-element nil))


(defun make-int-vec (&optional (s 100))
  (make-array s :fill-pointer 0 :element-type 'integer))


(defun get-atup (a i)
  (declare (integer i))
  (list (aref a i 0) (aref a i 1)))


(defun get-int-tup (a i)
  (declare (integer i))
  (declare (type (array integer) a))
  (list (aref a i 0) (aref a i 1)))


(defun get-dfloat-tup (a i)
  (declare (integer i))
  (declare (type (array double-float) a))
  (list (aref a i 0) (aref a i 1)))


(defun set-atup (a i vv)
  (declare (integer i))
  (declare (list vv))
  (destructuring-bind (v1 v2)
    vv
    (setf (aref a i 0) v1
          (aref a i 1) v2)))


(defun set-int-tup (a i vv)
  (declare (integer i))
  (declare (type (array integer) a))
  (declare (list vv))
  (destructuring-bind (v1 v2)
    vv
    (declare (integer v1 v2))
    (setf (aref a i 0) v1
          (aref a i 1) v2)))


(defun set-dfloat-tup (a i vv)
  (declare (integer i))
  (declare (type (array double-float) a))
  (declare (list vv))
  (destructuring-bind (v1 v2)
    vv
    (declare (double-float v1 v2))
    (setf (aref a i 0) v1
          (aref a i 1) v2)))


(defun rep-list (colors &aux (n (length colors)))
  (let ((i 0))
    (lambda () (nth (setf i (mod (+ 1 i) n)) colors))))


(defun close-path (p)
  (append p (list (nth 0 p))))


(defmacro square-loop ((x y s) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,s))
      (loop for ,x integer from 0 below ,sname do
        (loop for ,y integer from 0 below ,sname do
          ,@body)))))

