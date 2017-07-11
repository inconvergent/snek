
(defpackage :math
  (:use :common-lisp)
  (:export
    :add
    :close-path
    :cos-sin
    :dfloat
    :dfloat*
    :div
    :dot
    :dst
    :dst2
    :idiv
    :inc
    :int
    :int*
    :iscale
    :isub
    :len
    :len2
    :linspace
    :lmid
    :lround
    :make-perspective-transform
    :mid
    :mult
    :norm
    :nrep
    :nsub
    :on-circ
    :on-line
    :on-spiral
    :polygon
    :range
    :rep
    :scale
    :sin-cos
    :sub
    :sum)
  (:import-from :common-lisp-user
    :with-gensyms))

(defpackage :rnd
  (:use :common-lisp)
  (:export
    :aget
    :get-acc-circ-stp*
    :get-acc-lin-stp
    :get-acc-lin-stp*
    :get-circ-stp*
    :get-lin-stp
    :get-lin-stp*
    :in-box
    :in-circ
    :rndi
    :rndbtwn
    :lget
    :mixed
    :on-circ
    :on-line
    :on-spiral
    :rnd
    :rnd*
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

(defpackage :lin-path
  (:use :common-lisp)
  (:export
    :pos
    :pos*
    :rndpos
    :make
    :move)
  (:import-from :common-lisp-user
    :get-atup
    :set-atup
    :make-dfloat-array
    :with-struct))

(defpackage :bzspl
  (:use :common-lisp)
  (:export
    :pos
    :pos*
    :rndpos
    :make
    :move)
  (:import-from :common-lisp-user
    :get-atup
    :make-dfloat-array
    :with-struct))

(defpackage :zmap
  (:use :common-lisp)
  (:export
    :make
    :verts-in-rad)
  (:import-from :common-lisp-user
    :get-atup
    :with-struct))

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
    :inside
    :inside*
    :make-vec
    :square-loop
    :with-struct))

(defpackage :sandpaint
  (:use :common-lisp)
  (:export
    :chromatic-aberration
    :circ
    :circ*
    :clear
    :make
    :pix
    :pix*
    :pixel-hack
    :save
    :lin-path
    :set-rgba
    :stroke
    :strokes)
  (:import-from :common-lisp-user
    :aif
    :append-postfix
    :get-atup
    :inside
    :inside*
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
    :get-atup
    :make-dfloat-array
    :with-gensyms
    :with-struct
    :flatten))

