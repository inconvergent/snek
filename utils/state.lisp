
(defun get-state-gen (get-state-fun)
  (let ((state (make-hash-table :test #'equal)))
    (lambda (i noise)
      (multiple-value-bind (curr exists)
        (gethash i state)
        (if (not exists)
          (setf (gethash i state) (setf curr (funcall get-state-fun))))
        (funcall curr noise)))))

