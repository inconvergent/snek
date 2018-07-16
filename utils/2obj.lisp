
(defun export-2obj (snk fn)
  (let ((verts (snek:get-all-verts snk))
        (edges (snek:get-edges snk))
        (fnobj (append-postfix fn ".2obj")))
    (with-open-file (stream fnobj :direction :output
                                  :if-exists :supersede)
      (format stream "o mesh~%")
      (dolist (ll verts)
        (format stream "v ~f ~f~%" (vec::vec-x ll) (vec::vec-y ll)))
      (dolist (ll (coerce edges 'list))
        (destructuring-bind (a b)
          (math:add ll '(1 1))
          (format stream "e ~d ~d~%" a b))))

    (format t "~%num verts: ~a ~%" (length verts))
    (format t "num edges: ~a ~%" (length edges))
    (format t "~%file: ~a" fnobj)))

