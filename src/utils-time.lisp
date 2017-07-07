
(defvar *t*)
(defvar *tsum*)
(setf *t* (make-hash-table :test #'equal))
(setf *tsum* (make-hash-table :test #'equal))


(defun start-timer (tname)
  (multiple-value-bind (val exists)
    (gethash tname *tsum*)
    (if (not exists)
      (setf (gethash tname *tsum*) 0)))
  (setf (gethash tname *t*) (get-internal-real-time)))


(defun sum-timer (tname)
  (multiple-value-bind (val exists)
    (gethash tname *t*)
    (incf (gethash tname *tsum*) (- (get-internal-real-time) val)))
  (start-timer tname))


(defun show-timers ()
  (format t "~%timers: ~%")
  (loop for ti being the hash-keys of *tsum* do
    (format t "  ~a:  ~a ~%" ti (gethash ti *tsum*))))


(defmacro with-timer ((name) &body body)
  (with-gensyms (bname tname)
    `(let ((,tname ,name))
      (start-timer ,tname)
      (let ((,bname (progn ,@body)))
        (sum-timer ,tname)
        ,bname))))
