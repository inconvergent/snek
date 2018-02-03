
(in-package :plot)


(defun make-coverage-array (size)
  (make-array
    (list size size)
    :adjustable nil
    :initial-element 0
    :element-type 'integer))


(defstruct plot
  (verts nil :read-only nil)
  (edges nil :read-only nil)
  (lines nil :read-only nil)
  (coverage nil :read-only nil)
  (num-verts 0 :type integer :read-only nil)
  (num-edges 0 :type integer :read-only nil)
  (num-lines 0 :type integer :read-only nil)
  (discards 0 :type integer :read-only nil)
  (size nil :type integer :read-only nil))


(defun make (&optional (size 1000))
  (make-plot
    :size size
    :verts (make-generic-array)
    :edges (make-generic-array)
    :lines (make-generic-array)
    :coverage (make-coverage-array size)))


(defun dot-stroke (plt line num)
  (with-struct (plot- size verts coverage) plt
    (destructuring-bind (u v)
      line
      (loop for xy in (rnd:non-line num u v) do
        (vec:inside* (size xy x y)
          (incf (aref coverage x y))
          (vector-push-extend xy verts))))))


(defun -ok-coverage (size coverage offset a b)
  (let ((itt (round (* 2.0d0 (vec:len offset))))
        (cov-count 0)
        (cov (make-generic-array)))
    (loop for s in (math:linspace itt 0.0 1.0) do
      (vec:inside* (size (vec:on-line s a b) x y)
        (incf cov-count (if (> (aref coverage x y) 0) 1 0))
        (vector-push-extend (list x y) cov)))

    (if (< cov-count (half itt))
      (progn
        (loop for (x y) in (coerce cov 'list) do
          (incf (aref coverage x y)))
        t)
      nil)))


(defun -coverage-path (size coverage path)
  (loop
    for a in path and b in (cdr path) do
      (let ((n (* 2 (round (vec:dst a b)))))
        (loop for s in (math:linspace n 0.0 1.0) do
          (vec:inside* (size (vec:on-line s a b) x y)
            (incf (aref coverage x y)))))))


(defun path (plt path &aux (n (length path)))
  (with-struct (plot- size verts lines num-verts coverage) plt
    (dolist (p path)
      (vector-push-extend p verts))
    (vector-push-extend
      (math:range num-verts (+ num-verts n)) lines)
    (incf (plot-num-verts plt) n)
    (incf (plot-num-lines plt))
    (-coverage-path size coverage path)))


(defun line (plt a b)
  (with-struct (plot- size verts lines num-verts coverage) plt
    (dolist (p (list a b))
      (vector-push-extend p verts))
    (vector-push-extend
      (math:range num-verts (+ num-verts 2)) lines)
    (incf (plot-num-verts plt) 2)
    (incf (plot-num-lines plt))
    (-coverage-path size coverage (list a b))))


(defun -get-path-from-circ (xy rad)
  (let ((n (math:int (* 2.0d0 PI rad))))
    (values
      (loop for i in (math:linspace n 0d0 1d0)
            collect (vec:on-circ i rad :xy xy))
      n)))

(defun circ (plt xy rad)
  (with-struct (plot- size verts lines num-verts coverage) plt
    (multiple-value-bind (path n)
      (-get-path-from-circ xy rad)
      (dolist (p path)
        (vector-push-extend p verts))
      (vector-push-extend
        (math:range num-verts (+ num-verts n)) lines)
      (incf (plot-num-verts plt) n)
      (incf (plot-num-lines plt))
      (-coverage-path size coverage path))))


(defun -stipple (plt xy offset)
  (with-struct (plot- size verts edges coverage) plt
    (let ((nv (plot-num-verts plt)))
      (let ((a (vec:sub xy offset))
            (b (vec:add xy offset)))

        (if (-ok-coverage size coverage offset a b)
          (progn
            (vector-push-extend a verts)
            (vector-push-extend b verts)
            (vector-push-extend (list nv (1+ nv)) edges)
            (incf (plot-num-verts plt) 2)
            (incf (plot-num-edges plt) 1)
            0)
          1)))))


(defun -get-offset (u v s perp)
  (let ((off (vec:scale (vec:nsub u v) s)))
    (if perp (vec:perp off) off)))


(defun stipple-stroke (plt line num s &key perp)
  (with-struct (plot- size) plt
    (destructuring-bind (u v)
      line
      (let ((offset (-get-offset u v s perp)))
        (loop for xy in (rnd:non-line num u v) do
          (vec:inside (size xy x y)
            (incf (plot-discards plt)
                  (-stipple plt xy offset))))))))


; this wrapper is probably inefficient.
(defun stipple-strokes (plt lines num s &key perp)
  (loop for line in lines do
    (stipple-stroke plt line num s :perp perp)))


(defun -png-tuple (v) (list v v v 255))


(defun -write-png (coverage size fn)
  (let ((png (make-instance
               'zpng::pixel-streamed-png
               :color-type :truecolor-alpha
               :width size
               :height size)))

        (with-open-file
          (stream fn
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create
            :element-type '(unsigned-byte 8))
          (zpng:start-png png stream)

          (square-loop (x y size)
            (zpng:write-pixel
              (if (> (aref coverage y x) 0)
                (-png-tuple 0)
                (-png-tuple 255))
              png))
          (zpng:finish-png png))))


(defun -write-2obj (verts edges lines fn)
  (with-open-file (stream fn :direction :output :if-exists :supersede)
    (format stream "o mesh~%")
    (dolist (ll (coerce verts 'list))
      (format stream "v ~f ~f~%" (vec::vec-x ll)
                                 (vec::vec-y ll)))
    (if edges
      (dolist (ee (coerce edges 'list))
        (destructuring-bind (a b)
          (math:add ee '(1 1))
          (format stream "e ~d ~d~%" a b))))
    (if lines
      (dolist (ll (coerce lines 'list))
        (format stream "l")
        (dolist (l ll)
          (format stream " ~d" (1+ l)))
        (format stream "~%")))))


(defun save (plt fn)
  (with-struct (plot- size verts edges lines coverage
                      num-verts num-edges num-lines discards) plt
    (format t "~%writing result ...")
    (format t "~%~%num verts: ~a ~%" num-verts)
    (format t "num edges: ~a ~%" num-edges)
    (format t "num lines ~a ~%" num-lines)
    (format t "num discards: ~a ~%" discards)
    (-write-png coverage size (ensure-filename fn ".png"))
    (-write-2obj verts edges lines (ensure-filename fn ".2obj"))))

