
(defun -get-force-alterations (u v f)
  (list (snek:move-vert? v f) (snek:move-vert? u (vec:scale f -1.0d0))))


(defmacro force? (snk v1 v2 r)
  "
  creates relative movement (move-vert alteration) between verts
  v1 and v2.
  "
  (with-gensyms (vname v1name v2name rname)
    `(let ((,vname (snek::snek-verts ,snk))
           (,v1name ,v1)
           (,v2name ,v2)
           (,rname (math:dfloat ,r)))
      (-get-force-alterations
        ,v1name ,v2name
        (vec:scale (vec:nsub (vec:sarr-get ,vname ,v1name)
                             (vec:sarr-get ,vname ,v2name))
          ,rname)))))

