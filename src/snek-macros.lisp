
(in-package :snek)

(defmacro with ((snk &key zwidth collect include-alts) &body body)
  "
  creates a context for manipulating snek via alterations.
  all alterations created in this context will be flattened
  and applied to snk at the end of the context.
  "
  (declare (symbol snk)
           (type boolean collect include-alts))
  (with-gensyms (sname zw aname rec x y resalts)
    (let* ((do-funcall `(funcall (gethash (type-of ,x) ,aname) ,sname ,x))
           (wrap-funcall (cond ((and collect include-alts)
                                  `(vextend (list ,do-funcall ,x) ,resalts))
                               (collect `(vextend ,do-funcall ,resalts))
                               (t do-funcall)))
           (finally (if collect `(to-list ,resalts)
                                nil)))
      `(let* ((,sname ,snk)
              (,zw ,zwidth)
              (,resalts (make-adjustable-vector))
              (,aname (snek-alt-names ,sname)))

        (incf (snek-wc ,sname))

        (labels ((,rec (,x)
                  (cond ((null ,x))
                  ((atom ,x)
                     ; if atom is also alteration (else ignore):
                     (when (gethash (type-of ,x) ,aname) ,wrap-funcall))
                  (t (,rec (car ,x))
                     (,rec (cdr ,x))))))

          (zmap::with* (,zw (snek-verts ,sname)
                            (snek-num-verts ,sname)
                            (lambda (,y) (setf (snek-zmap ,sname) ,y)))
            ; this lets @body be executed in the context of zmap:with;
            ; useful if we want to have a parallel context inside zmap.
            (,rec (list ,@body))))

        ,finally))))


(defmacro zwith ((snk zwidth) &body body)
  "
  creates a snek context. the zmap is constant inside this context.
  "
  (declare (symbol snk))
  (with-gensyms (sname zw y)
    `(let ((,sname ,snk)
           (,zw ,zwidth))
      (zmap::with* (,zw (snek-verts ,sname)
                        (snek-num-verts ,sname)
                        (lambda (,y) (setf (snek-zmap ,sname) ,y)))
          (progn ,@body)))))


(defmacro with-verts-in-rad ((snk xy rad v) &body body)
  (declare (symbol snk))
  (with-gensyms (sname)
    `(let ((,sname ,snk))
      (zmap:with-verts-in-rad ((snek-zmap ,sname) (snek-verts ,sname) ,xy ,rad ,v)
        (progn ,@body)))))


(defmacro with-dx ((snk vv dx d) &body body)
  (declare (symbol snk))
  (with-gensyms (sname)
    `(let ((,sname ,snk))
       (let* ((,dx (apply #'vec:isub (get-verts ,sname ,vv)))
              (,d (vec:len ,dx)))
         (declare (double-float ,d) (vec:vec ,dx))
         (when (> ,d 0d0)
           (list ,@body))))))


(defmacro with-grp ((snk g* g) &body body)
  "
  select grp g from snek instance. g will be available in this context as g*.
  "
  (declare (symbol snk))
  (with-gensyms (grps exists gname sname)
    `(let ((,sname ,snk)
           (,gname ,g))
      (let ((,grps (snek-grps ,sname)))
        (multiple-value-bind (,g* ,exists)
          (gethash ,gname ,grps)
            (unless ,exists (error "attempted to access invalid group: ~a" ,gname))
            (progn ,@body))))))


(defmacro with-rnd-edge ((snk i &key g) &body body)
  "
  select an arbitrary edge from a snek instance. the edge will be
  available in the context as i.

  if a grp is supplied it will select an edge from g, otherwise it will
  use the main grp.
  "
  (declare (symbol snk))
  (with-gensyms (grp edges grph ln)
    `(with-grp (,snk ,grp ,g)
      (let ((,grph (grp-grph ,grp)))
        (let* ((,edges (graph:get-edges ,grph))
               (,ln (length ,edges)))
          (declare (fixnum ,ln))
          (when (> ,ln 0)
            (let ((,i (aref ,edges (rnd:rndi ,ln))))
              (declare (list ,i))
              (list ,@body))))))))


(defmacro with-rnd-vert ((snk i) &body body)
  "
  select an arbitrary vert from a snek instance. the vert will be
  available in the context as i.
  "
  (declare (symbol snk))
  (with-gensyms (num)
    `(let ((,num (snek-num-verts ,snk)))
       (when (> ,num 0)
         (let ((,i (rnd:rndi ,num)))
           (declare (fixnum ,i))
           (list ,@body))))))


(defmacro itr-verts ((snk i &key (collect t)) &body body)
  "
  iterates over ALL verts in snk as i.
  "
  (declare (symbol snk)
           (type boolean collect))
  (with-gensyms (sname)
    `(let ((,sname ,snk))
      (loop for ,i of-type fixnum from 0 below (snek-num-verts ,sname)
        ,(if collect 'collect 'do) (list ,@body)))))


(defmacro itr-grp-verts ((snk i &key g (collect t)) &body body)
  "
  iterates over all verts in grp g as i.

  NOTE: this will only yield vertices that belong to at least one edge that is
  part of g. if you want all vertices in snek you should use itr-verts instead.
  itr-verts is also faster, since it does not rely on the underlying graph
  structure.

  if g is not provided, the main grp wil be used.
  "
  (declare (symbol snk) (type boolean collect))
  (with-gensyms (grp sname)
    `(let ((,sname ,snk))
      (with-grp (,sname ,grp ,g)
        (map ',(if collect 'list 'nil)
          (lambda (,i) (declare (fixnum ,i)) (list ,@body))
          (graph:get-verts (grp-grph ,grp)))))))


(defmacro itr-prm-verts ((snk i &key p (collect t)) &body body)
  "
  iterates over all verts in prm p as i.
  "
  (declare (symbol snk)
           (type boolean collect))
  (with-gensyms (sname)
    `(let* ((,sname ,snk))
      (map ',(if collect 'list 'nil)
          (lambda (,i) (declare (fixnum ,i)) (list ,@body))
          (get-prm-vert-inds ,sname :p ,p)))))


(defmacro itr-edges ((snk i &key g (collect t)) &body body)
  "
  iterates over all edges in grp g as i.

  if g is not provided, the main grp will be used.
  "
  (declare (symbol snk)
           (type boolean collect))
  (with-gensyms (grp grph)
    `(with-grp (,snk ,grp ,g)
      (let ((,grph (grp-grph ,grp)))
        (map ',(if collect 'list 'nil)
             (lambda (,i) (list ,@body))
             (graph:get-edges ,grph))))))


(defmacro itr-grps ((snk g &key (collect t) main) &body body)
  "
  iterates over all grps of snk as g.
  "
  (declare (symbol snk) (type boolean collect))
  (with-gensyms (grps sname main*)
    `(let ((,sname ,snk)
           (,main* ,main))
      (let ((,grps (snek-grps ,sname)))
        (loop for ,g being the hash-keys of ,grps
          if (or ,main* ,g)
          ,(if collect 'collect 'do) (list ,@body))))))


(defmacro itr-prms ((snk p &key (collect t)) &body body)
  "
  iterates over all prms of snk as p.
  "
  (declare (symbol snk)
           (type boolean collect))
  (with-gensyms (prms sname)
    `(let ((,sname ,snk))
      (let ((,prms (snek-prms ,sname)))
        (loop for ,p being the hash-keys of ,prms
          ,(if collect 'collect 'do) (list ,@body))))))

