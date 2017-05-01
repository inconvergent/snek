
(defpackage :plot
  (:use :common-lisp)
  (:export
    :make
    :dot-stroke
    :path
    :save
    :stipple-stroke
    :stipple-strokes)
  (:import-from :common-lisp-user
    :add
    :append-postfix
    :dst
    :get-as-list
    :half
    :iscale
    :len
    :linspace
    :lround
    :make-vec
    :nrep
    :nsub
    :on-line
    :range
    :rnd-on-line
    :scale
    :square-loop
    :sub
    :with-struct))


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


(defun make (size)
  (make-plot
    :size size
    :verts (make-vec)
    :edges (make-vec)
    :lines (make-vec)
    :coverage (make-coverage-array size)))


(defun dot-stroke (plt line num)
  (with-struct (plot- size verts coverage) plt
    (destructuring-bind (u v)
      line
      (loop for xy in (nrep num (rnd-on-line u v)) do
        (destructuring-bind (x y)
          (lround xy)
          (if (and (>= x 0) (< x size) (>= y 0) (< y size))
            (progn
              (incf (aref coverage x y))
              (vector-push-extend xy verts))))))))


(defun -ok-coverage (size coverage offset a b)
  (let ((itt (round (* 2.0d0 (len offset))))
        (cov-count 0)
        (cov (make-vec)))
    (loop for s in (linspace 0.0 1.0 itt) do
      (destructuring-bind (x y)
        (lround (on-line s a b))
        (if (and (>= x 0) (< x size) (>= y 0) (< y size))
          (progn
            (incf cov-count (if (> (aref coverage x y) 0) 1 0))
            (vector-push-extend (list x y) cov)))))

    (if (< cov-count (half itt))
      (progn
        (loop for xy in (coerce cov 'list) do
          (destructuring-bind (x y)
            xy
            (incf (aref coverage x y))))
        t)
      nil)))


(defun vflip (v)
  (destructuring-bind (a b)
    v
    (list b (- 0 a))))


(defun -coverage-path (size coverage path)
  (loop
    for a in path
    for b in (cdr path)
    do
      (let ((n (* 2 (round (dst a b)))))
        (loop for s in (linspace 0.0 1.0 n) do
          (destructuring-bind (x y)
            (lround (on-line s a b))
            (if (and (>= x 0) (< x size) (>= y 0) (< y size))
              (incf (aref coverage x y))))))))


(defun path (plt path)
  (let ((n (length path)))
  (with-struct (plot- size verts lines num-verts coverage) plt
    ; todo: test if path is outside boundary
    (dolist (p path)
      (vector-push-extend p verts))
    (vector-push-extend
      (range num-verts (+ num-verts n)) lines)
    (incf (plot-num-verts plt) n)
    (incf (plot-num-lines plt))
    (-coverage-path size coverage path))))


(defun -stipple (plt xy offset)
  (with-struct (plot- size verts edges coverage) plt
    (let ((nv (plot-num-verts plt)))
      (let ((a (sub xy offset))
            (b (add xy offset)))

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
  (let ((off (scale (nsub u v) s)))
    (if perp (vflip off) off)))


(defun stipple-stroke (plt line num s &key perp)
  (with-struct (plot- size) plt
    (destructuring-bind (u v)
      line
      (let ((offset (-get-offset u v s perp)))
        (loop for xy in (nrep num (rnd-on-line u v)) do
          (destructuring-bind (x y) (lround xy)
            (if (and (>= x 0) (< x size) (>= y 0) (< y size))
              (incf (plot-discards plt)
                    (-stipple plt xy offset)))))))))


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
      (destructuring-bind (a b)
        ll
        (format stream "v ~f ~f~%" a b)))
    (if edges
      (dolist (ee (coerce edges 'list))
        (destructuring-bind (a b)
          (add ee '(1 1))
          (format stream "e ~d ~d~%" a b))))
    (if lines
      (dolist (ll (coerce lines 'list))
        (format stream "l")
        (dolist (l ll)
          (format stream " ~d" (1+ l)))
        (format stream "~%")))))


(defun save (plt fn)
  (if (not fn) (error "missing result file name."))
  (let ((fnimg (append-postfix fn ".png"))
        (fnobj (append-postfix fn ".2obj")))
    (with-struct (plot- size verts edges lines coverage
                        num-verts num-edges num-lines discards) plt
      (-write-png coverage size fnimg)
      (-write-2obj verts edges lines fnobj)
      (format t "~%~%num verts: ~a ~%" num-verts)
      (format t "num edges: ~a ~%" num-edges)
      (format t "num lines ~a ~%" num-lines)
      (format t "num discards: ~a ~%" discards))
    (format t "~%files ~a" fnimg)
    (format t "~%      ~a~%~%" fnobj)))

