
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


(defun 2* (l) (* l 2))


(defun 2+ (l) (+ l 2))


(defun half (l) (/ l 2))


(defun make-dfloat-array (rows &key (cols 2) (initial 0.0d0))
  (make-array
    (list rows cols)
    :initial-element initial
    :element-type 'double-float))


(defun make-symb-array (rows &key (initial nil))
  (make-array
    rows
    :initial-element initial
    :element-type 'symbol))


(defun make-int-array (rows  &key (cols 2) (initial 0))
  (make-array
    (list rows cols)
    :initial-element initial
    :element-type 'integer))


(defun make-vec (&optional (n 10))
  (make-array
    n
    :fill-pointer 0
    :initial-element nil))


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


(defun get-atup (arr row)
  (list
    (aref arr row 0)
    (aref arr row 1)))


(defun set-atup (arr row ab)
  (destructuring-bind (a b)
    ab
    (setf (aref arr row 0) a
          (aref arr row 1) b)))


(defun rep-list (colors &aux (n (length colors)))
  (let ((i 0))
    (lambda () (nth (setf i (mod (+ 1 i) n)) colors))))


(defmacro val-if-eql (a b)
  (with-gensyms (aname)
    `(let ((,aname ,a))
      (if (eql ,aname ,b) ,aname nil))))

