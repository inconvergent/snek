
(defpackage :vec
  (:use :common-lisp)
  (:export
    :add
    :arr-get
    :arr-set
    :copy
    :cos-sin
    :div
    :dot
    :dst
    :dst2
    :flip
    :idiv
    :inside
    :inside*
    :iscale
    :isub
    :ladd
    :ladd*
    :ldiv
    :ldiv*
    :ldst
    :ldst*
    :len
    :len2
    :lmid
    :lmid
    :lmult
    :lmult*
    :lround
    :lsub
    :lsub*
    :mid
    :mult
    :norm
    :nsub
    :on-circ
    :on-line
    :on-spiral
    :one
    :perp
    :polygon
    :rep
    :rot
    :scale
    :sin-cos
    :sub
    :sum
    :tolist
    :v
    :vec
    :vec*
    :vec-coerce
    :zero
    :with-xy)
  (:import-from :common-lisp-user
    :PII
    :with-gensyms))

(defpackage :math
  (:use :common-lisp)
  (:export
    :add
    :close-path
    :dfloat
    :dfloat*
    :div
    :dst
    :inc
    :int
    :int*
    :iscale
    :iscale*
    :lget
    :linspace
    :mult
    :nrep
    :range
    :rep
    :scale
    :scale*
    :sfloat
    :sfloat*
    :sub
    :sum
    :with-linspace)
  (:import-from :common-lisp-user
    :with-gensyms))


(defpackage :rnd
  (:use :common-lisp)
  (:export
    :aget
    :bernoulli
    :get-acc-circ-stp*
    :get-acc-lin-stp
    :get-acc-lin-stp*
    :get-circ-stp*
    :get-lin-stp
    :get-lin-stp*
    :in-box
    :in-circ
    :lget
    :nin-box
    :nin-circ
    :non-circ
    :non-line
    :norm
    :nrnd
    :nrnd*
    :nrndi
    :nrndi*
    :on-circ
    :on-line
    :rnd
    :rnd*
    :rndbtwn
    :rndi
    :rndi*
    :rndspace
    :rndspacei
    :with-on-line
    :with-in-circ
    :with-rndspace)
  (:import-from :common-lisp-user
    :PII
    :with-gensyms))


(defpackage :color
  (:use :common-lisp)
  (:export
    :black
    :dark
    :gray
    :hsv
    :mdark
    :rgb
    :transparent
    :vdark
    :white)
  (:import-from :common-lisp-user
    :with-gensyms))


(defpackage :hset
  (:use :common-lisp)
  (:export
    :add
    :add*
    :del
    :del*
    :make
    :mem
    :mem*
    :num
    :to-list))

(defpackage :graph
  (:use :common-lisp)
  (:export
    :add
    :del
    :get-edges
    :get-num-edges
    :get-num-verts
    :get-verts
    :make
    :mem
    :to-list
    :with-graph-edges)
  (:import-from :common-lisp-user
    :flatten
    :make-vec
    :with-struct
    :with-gensyms))


(defpackage :bzspl
  (:use :common-lisp)
  (:export
    :pos
    :pos*
    :rndpos
    :make
    :with-rndpos)
  (:import-from :common-lisp-user
    :with-gensyms
    :make-dfloat-array
    :with-struct))


(defpackage :lin-path
  (:use :common-lisp)
  (:export
    :pos
    :pos*
    :rndpos
    :make
    :move)
  (:import-from :common-lisp-user
    :make-dfloat-array
    :set-dfloat-tup
    :with-struct))

(defpackage :zmap
  (:use :common-lisp)
  (:export
    :make
    :with*
    :verts-in-rad
    :with-verts-in-rad)
  (:import-from :common-lisp-user
    :get-dfloat-tup
    :make-int-vec
    :with-struct
    :with-gensyms))

(defpackage :sandpaint
  (:use :common-lisp)
  (:export
    :chromatic-aberration
    :circ
    :circ*
    :clear
    :dens-stroke
    :lin-path
    :make
    :bzspl-stroke
    :pix
    :pix*
    :pixel-hack
    :save
    :set-rgba
    :stroke
    :strokes)
  (:import-from :common-lisp-user
    :aif
    :append-postfix
    :get-dfloat-tup
    :square-loop
    :with-struct))

(defpackage :plot
  (:use :common-lisp)
  (:export
    :dot-stroke
    :make
    :path
    :save
    :stipple-stroke
    :stipple-strokes)
  (:import-from :common-lisp-user
    :aif
    :append-postfix
    :half
    :make-vec
    :square-loop
    :with-struct))

(defpackage :snek
  (:use :common-lisp)
  (:export
    :add-edge!
    :add-edge*?
    :add-grp!
    :add-path!
    :add-path*!
    :add-polygon!
    :add-vert!
    :add-vert?
    :add-verts!
    :append-edge?
    :del-edge!
    :draw-circ
    :draw-edges
    :draw-verts
    :edge-length
    :export-2obj
    :force?
    :get-all-grps
    :get-all-verts
    :get-edges
    :get-grp-as-bzspl
    :get-grp-verts
    :get-num-edges
    :get-num-verts
    :get-vert
    :get-vert-inds
    :get-verts
    :itr-all-verts
    :itr-edges
    :itr-grps
    :itr-verts
    :join-verts?
    :make
    :make-mutate
    :move-vert?
    :mutate
    :split-edge?
    :verts-in-rad
    :with
    :with-dx
    :with-grp
    :with-prob
    :with-rnd-edge
    :with-rnd-vert
    :with-verts-in-rad)
  (:import-from :common-lisp-user
    :append-postfix
    :flatten
    :get-dfloat-tup
    :make-dfloat-array
    :with-gensyms
    :with-struct))

