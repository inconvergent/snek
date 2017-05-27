
(defvar *tests* 0)
(defvar *fails* 0)
(defvar *passes* 0)


; TODO: approximately similar to


(defmacro test-title (&body body)
  `(progn
     (format t "~%~a ##############~%" ',@body)
     ,@body))


(defmacro do-test (a &optional (b nil))
  (with-gensyms (aname bname)
    (incf *tests*)
    `(let ((,aname ,a)
           (,bname ,b))
      (if (funcall #'equalp ,aname ,bname)
        (progn
          (incf *passes*)
          (format t "~%~a ~%--> ok" ',a))
        (progn
          (incf *fails*)
          (format t "~%~a ~%#-> not ok. #################################### ~%--  wanted: ~% ~a ~%--  got: ~% ~a"
            ',a
            ',b
            ,aname)))
      (format t "~%---------------------------~%"))))


(defun test-summary ()
  (format t "~% tests:  ~a~% fails:  ~a~% passes: ~a~%"
          *tests* *fails* *passes*))


