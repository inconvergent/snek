
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
    :len
    :len2
    :lmid
    :lround
    :mid
    :mult
    :norm
    :nsub
    :perp
    :rep
    :scale
    :sin-cos
    :sub
    :sum
    :vec
    :vec*
    :vec-coerce
    :with-xy)
  (:import-from :common-lisp-user
    :with-gensyms))

(defpackage :math
  (:use :common-lisp)
  (:export
    :add
    :close-path
    :dfloat
    :dfloat*
    :div
    :inc
    :int
    :int*
    :iscale
    :linspace
    :make-perspective-transform
    :mult
    :nrep
    :on-circ
    :on-line
    :polygon
    :range
    :rep
    :scale
    :sub
    :sum)
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
    :mixed
    :on-circ
    :on-line
    :rnd
    :rnd*
    :rndbtwn
    :rndi
    :rndi*
    :rndspace
    :rndspacei)
  (:import-from :common-lisp-user
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
    :with-struct))


(defpackage :bzspl
  (:use :common-lisp)
  (:export
    :pos
    :pos*
    :rndpos
    :rndpos*
    :make
    :move)
  (:import-from :common-lisp-user
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
    :verts-in-rad)
  (:import-from :common-lisp-user
    :get-dfloat-tup
    :make-int-vec
    :with-struct))

(defpackage :sandpaint
  (:use :common-lisp)
  (:export
    :chromatic-aberration
    :circ
    :circ*
    :clear
    :lin-path
    :make
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
    :get-all-verts
    :get-edges
    :get-grp-verts
    :get-num-edges
    :get-num-verts
    :get-vert
    :get-vert-inds
    :get-verts
    :init-path
    :init-polygon
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
    :with-rnd-vert)
  (:import-from :common-lisp-user
    :append-postfix
    :flatten
    :get-dfloat-tup
    :make-dfloat-array
    :with-gensyms
    :with-struct))

