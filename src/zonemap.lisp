
(in-package :zmap)


; this macro exists so we can use the code externally in the same way
; regardless of whether we are using the parallel version of zmap (not yet
; implemented)
(defmacro with* ((zw verts num-verts context-fxn) &body body)
  (with-gensyms (zw* verts* num-verts*)
    `(let ((,zw* ,zw))
      (when ,zw*
        (let ((,num-verts* ,num-verts)
              (,verts* ,verts))
          (declare (type (simple-array double-float) ,verts*)
                   (fixnum ,num-verts*))
          (funcall ,context-fxn (make ,verts* ,num-verts* ,zw*))))
      (progn ,@body))))


;TODO: test this
;http://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function
;https://github.com/perrygeo/pairing/blob/master/pairing/main.py
;(defun pair (zw i j &aux (x (truncate i zw)) (y (truncate j zw)) )
;  (declare (optimize (safety 0) speed (debug 0))
;           (fixnum x y)
;           (double-float zw i j))
;  (truncate (/ (+ (* (+ x y) (+ x y 1)) y) 2)))

;(defun unpair (z)
;  (declare (optimize (safety 0) speed (debug 0)) (fixnum z))
;  (let* ((w (truncate (/ (expt (1+ (+ (* 8 z) 1)) 2))))
;         (tt (/ (+ (expt w 2) w) 2)))
;    (list (truncate #1=(- z tt))
;          (truncate (- w #1#)))))

(defun -xy-to-zone (zwidth x y)
  (declare (double-float zwidth x y))
  (values (the fixnum (floor x zwidth))
          (the fixnum (floor y zwidth))))


(defstruct (zmap (:constructor -make-zmap))
  (zwidth nil :type double-float :read-only t)
  (num-verts nil :type fixnum :read-only t)
  (zone-to-verts nil :type hash-table :read-only t))


(defun make (verts num-verts zwidth)
  (declare (optimize (safety 0) speed (debug 0))
           (double-float zwidth) (fixnum num-verts)
           (type (simple-array double-float) verts))
  (let ((zone-to-verts (make-hash-table :test #'equal)))
    (loop for v of-type fixnum from 0 below num-verts do
      ; add v to zone
      (let ((z (list (floor (aref verts #1=(* 2 v)) zwidth)
                     (floor (aref verts (1+ #1#)) zwidth))))
        (declare (list z))
        (multiple-value-bind (vals exists) (gethash z zone-to-verts)
          (when (not exists)
            (setf vals (make-adjustable-vector :type 'fixnum)
                  (gethash z zone-to-verts) vals))
          (vextend v vals))))

    (-make-zmap :zwidth zwidth :num-verts num-verts
                :zone-to-verts zone-to-verts)))


(defmacro with-verts-in-rad ((zm verts xy rad v) &body body)
  (with-gensyms (rad2 zm* zwidth zone-to-verts xy* za zai zb
                 vals verts* exists i j xx yy)
    `(let* ((,rad2 (expt ,rad 2d0))
            (,verts* ,verts)
            (,xy* ,xy)
            (,xx (vec::vec-x ,xy*))
            (,yy (vec::vec-y ,xy*))
            (,zm* ,zm)
            (,zwidth (zmap-zwidth ,zm*))
            (,zone-to-verts (zmap-zone-to-verts ,zm*)))
      (declare (double-float ,rad2 ,zwidth ,xx ,yy)
               (type (simple-array double-float) ,verts*)
               (hash-table ,zone-to-verts) (vec:vec ,xy*))
      (multiple-value-bind (,za ,zb) (-xy-to-zone ,zwidth ,xx ,yy)
        (declare (fixnum ,za ,zb))
        (loop for ,i of-type fixnum from -1 below 2 do
          (loop with ,zai of-type fixnum = (+ ,za ,i)
                for ,j of-type fixnum from -1 below 2 do
            (multiple-value-bind (,vals ,exists)
              (gethash (list ,zai (+ ,j ,zb)) ,zone-to-verts)
              (when ,exists
                (loop for ,v of-type fixnum across ,vals
                      if (< (+ (expt (- ,xx (aref ,verts #2=(* 2 ,v))) 2d0)
                               (expt (- ,yy (aref ,verts (1+ #2#))) 2d0)) ,rad2)
                      do (progn ,@body))))))))))


(defun verts-in-rad (zm verts xy rad)
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) verts) (zmap zm)
           (vec:vec xy) (double-float rad))
  (let ((inds (make-adjustable-vector :type 'fixnum)))
    (declare (vector inds))
    (with-verts-in-rad (zm verts xy rad v) (vextend v inds))
    inds))

