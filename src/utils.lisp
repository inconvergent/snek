
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


(defun close-path (p)
  (append p (list (nth 0 p))))


(defmacro square-loop ((x y s) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,s))
      (loop for ,x from 0 below ,sname do
        (loop for ,y from 0 below ,sname do
          ,@body)))))


(defmacro inside-border ((size xy b) &body body)
  (with-gensyms (xname yname sname small large)
    `(let* ((,sname ,size)
            (,small ,b)
            (,large (- ,sname ,small)))
      (destructuring-bind (,xname ,yname)
        ,xy
        (if (and (>= ,xname ,small) (< ,xname ,large)
                 (>= ,yname ,small) (< ,yname ,large))
          (progn
            ,@body))))))


(defun -lround (l)
  (mapcar #'round l))


; TODO: remove external use where lround does not make sense.
(defmacro inside ((size xy) &body body)
  (with-gensyms (xname yname sname)
    `(let ((,sname ,size))
      (destructuring-bind (,xname ,yname)
        (-lround ,xy)
        (if (and (>= ,xname 0) (< ,xname ,sname)
                 (>= ,yname 0) (< ,yname ,sname))
          (progn
            ,@body))))))


(defmacro inside* ((size xy x y) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,size))
      (destructuring-bind (,x ,y)
        (-lround ,xy)
        (if (and (>= ,x 0) (< ,x ,sname)
                 (>= ,y 0) (< ,y ,sname))
          (progn
            ,@body))))))

