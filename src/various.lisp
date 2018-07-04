
(abbrev array-push vector-push-extend)


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
  (concatenate 'string fn postfix))


(defun append-number (fn i)
  (format nil "~a-~8,'0d" fn i))


(defun ensure-filename (fn &optional (postfix "") (silent nil))
  (let ((fn* (append-postfix (if fn fn "tmp") postfix)))
    (format (not silent) "~%file: ~a~%~%" fn*)
    fn*))


(defun print-every (i n)
  (when (= 0 (mod i n)) (format t "~%itt: ~a~%" i)))


(defun string-list-concat (l)
  (format nil "~{~a~}" l))

(defun numshow (a)
  (if (< 1d-6 (abs a) 1d6)
    (format nil "~,6f " a)
    (format nil "~,1e " a)))


(defun exec-with-args (fxn args &optional ea)
  (apply fxn (if ea (append args ea) args)))


(defun dhalf (l)
  (declare (double-float l))
  (* l 0.5d0))


(defun half (l)
  (/ l 2))


(defun length-1 (a)
  (1- (length a)))


(defun array-last (a)
  (aref a (length-1 a)))


(defun array-add (a vv)
  (if (eql (type-of vv) 'cons)
    (loop for v in vv do (array-push v a))
    (loop for v across vv do (array-push v a))))


(defun make-generic-array (&key init (type t) (size 100))
  (let ((res (if init (make-array (length init) :fill-pointer 0
                                                :initial-contents init
                                                :element-type type
                                                :adjustable t)
                      (make-array size :fill-pointer 0
                                       :element-type type
                                       :adjustable t))))
    (when init (array-add res init))
    res))


(defun to-array (init)
  (declare (list init))
  (make-array (length init) :initial-contents init))


(defun ensure-array (o &key (fx #'to-array))
  (if (equal (type-of o) 'cons) (funcall fx o) o))


; TODO: array push macro?
(defun array-push* (xx arr)
  (if (eql (type-of xx) 'cons)
    (loop for x in xx do (array-push x arr))
    (loop for x across xx do (array-push x arr))))


(defun to-generic-array (init &key (type t))
  (make-array (* (length init))
              :fill-pointer (length init)
              :initial-contents init
              :element-type type
              :adjustable t))


(defun make-generic-hash-table (&key init (test #'equal)
                                &aux (init* (ensure-array init
                                              :fx #'to-generic-array)))
  (if (< (length init) 1)
    (make-hash-table :test test)
    (loop with res = (make-hash-table :test test)
          for (k v) across init*
          do (setf (gethash k res) v)
          finally (return res))))


; TODO: what is num for?
(defun count-things (data &key (test #'equal)
                               (getter (lambda (x) x))
                               (key (lambda (x) (second x)))
                               (compare #'>)
                               num
                          &aux (data* (ensure-array data :fx #'to-generic-array))
                               (num* (if num num (* 2 (length data)))))
  (loop with res = (make-hash-table :test test)
        for d across data*
        do (incf (gethash (funcall getter d) res 0))
        finally (return (sort
                          (loop for k being the hash-keys of res
                                  using (hash-value v)
                                collect (list k v)) compare :key key))))


(defun to-list (a)
  (coerce a 'list))


(defun rep-list (colors &aux (n (length colors)))
  (let ((i 0))
    (lambda () (nth (setf i (mod (+ 1 i) n)) colors))))


(defun close-path (p)
  (append p (list (nth 0 p))))

