
(abbrev vextend vector-push-extend)


(defconstant PII (the double-float (* PI 2d0)))
(defconstant PI5 (the double-float (* PI 0.5d0)))

(declaim (type double-float PII PI5))


;http://cl-cookbook.sourceforge.net/os.html
(defun cmd-args ()
  (or #+SBCL *posix-argv*
      #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words*
      nil))


(defun append-postfix (fn postfix)
  (declare (string fn postfix))
  (concatenate 'string fn postfix))


(defun append-number (fn i)
  (declare (string fn) (fixnum i))
  (format nil "~a-~8,'0d" fn i))


(defun ensure-filename (fn &optional (postfix "") (silent nil))
  (let ((fn* (append-postfix (if fn fn "tmp") postfix)))
    (declare (string fn*))
    (format (not silent) "~%file: ~a~%~%" fn*)
    fn*))


(defun print-every (i n)
  (declare (fixnum i n))
  (when (= 0 (mod i n)) (format t "~%itt: ~a~%" i)))


(defun string-list-concat (l)
  (declare (list l))
  (format nil "~{~a~}" l))


(defun numshow (a)
  (declare (double-float a))
  (if (< 1d-6 (the double-float (abs a)) 1d6)
    (format nil "~,6f " a)
    (format nil "~,1e " a)))


(defun exec-with-args (fxn args &optional ea)
  (declare (function fxn) (list args ea))
  (apply fxn (if ea (append args ea) args)))


(defun dhalf (l)
  (declare (double-float l))
  (coerce (* l 0.5d0) 'double-float))


(defun half (l)
  (declare (number l))
  (coerce (/ (coerce l 'double-float) 2d0) 'double-float))


(defun length-1 (a)
  (declare (sequence a))
  (1- (the fixnum (length a))))


(defun vector-last (a)
  (declare (vector a))
  (aref a (the fixnum (length-1 a))))


(defun vector-first (a)
  (declare (vector a))
  (aref a 0))


(defun -vector-add (a vv)
  (declare (vector a) (sequence vv))
  (if (eql (type-of vv) 'cons)
    (loop for v in vv do (vextend v a))
    (loop for v across vv do (vextend v a))))


(defun make-adjustable-vector (&key init (type t) (size 256))
  (let ((res (if init (make-array (length init) :fill-pointer 0
                                                :initial-contents init
                                                :element-type type
                                                :adjustable t)
                      (make-array size :fill-pointer 0
                                       :element-type type
                                       :adjustable t))))
    (when init (-vector-add res init))
    res))


(defun to-vector (init)
  (declare (list init))
  (make-array (length init) :initial-contents init))


(defun ensure-vector (o &key (fx #'to-vector))
  (declare (sequence o) (function fx))
  (if (equal (type-of o) 'cons) (funcall fx o) o))


; TODO: array push macro?
(defun vextend* (xx arr)
  (declare (sequence xx) (vector arr))
  (if (eql (type-of xx) 'cons)
    (loop for x in xx do (vextend x arr))
    (loop for x across xx do (vextend x arr))))


(defun to-adjustable-vector (init &key (type t))
  (declare (sequence init))
  (make-array (length init) :fill-pointer (length init)
                            :initial-contents init
                            :element-type type
                            :adjustable t))


(defun make-generic-hash-table (&key init (test #'equal))
  (declare (sequence init) (function test))
  (if (< (length init) 1)
    (make-hash-table :test test)
    (if (equal (type-of init) 'cons)
      (loop with res = (make-hash-table :test test)
            for (k v) in init
            do (setf (gethash k res) v)
            finally (return res))
      (loop with res = (make-hash-table :test test)
            for (k v) across init
            do (setf (gethash k res) v)
            finally (return res)))))


(defun count-things (data &key (test #'equal)
                               (getter (lambda (x) x))
                               (key (lambda (x) (second x)))
                               (compare #'>)
                          &aux (data* (ensure-vector data
                                                     :fx #'to-adjustable-vector)))
  (declare (function test compare getter key))
  (loop with res = (make-hash-table :test test)
        for d across data*
        do (incf (gethash (funcall getter d) res 0))
        finally (return (sort
                          (loop for k being the hash-keys of res
                                  using (hash-value v)
                                collect (list k v)) compare :key key))))


(defun to-list (a)
  (declare (sequence a))
  (coerce a 'list))


(defun close-path (p)
  (declare (list p))
  (append p (list (nth 0 p))))

