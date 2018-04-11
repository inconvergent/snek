
(defpackage :vec
  (:use :common-lisp)
  (:export
    :add
    :angle
    :arr-get
    :arr-set
    :copy
    :cos-sin
    :cross
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
    :lrot
    :lscale*
    :lsub
    :lsub*
    :mid
    :mult
    :neg
    :norm
    :nsub
    :on-circ
    :on-line
    :on-line*
    :on-spiral
    :one
    :op
    :perp
    :polygon
    :ptinside
    :rep
    :rot
    :rect
    :scale
    :segdst
    :segx
    :segx*
    :sin-cos
    :square
    :sub
    :sum
    :tolist
    :v
    :vec
    :vec*
    :vec-coerce
    :with-loop-grid
    :with-loop-grid*
    :with-xy
    :with-xy-short
    :zero)
  (:import-from :common-lisp-user
    :close-path
    :array-push
    :PII
    :with-gensyms))

(defpackage :math
  (:use :common-lisp)
  (:export
    :add
    :close-path
    :convex-split
    :cpath
    :dfloat
    :dfloat*
    :div
    :dst
    :hatch
    :inc
    :int
    :int*
    :iscale
    :iscale*
    :lget
    :linspace
    :mid-rad
    :mod+
    :mod-
    :mod2
    :mult
    :nrep
    :path-angles
    :path-normals-closed
    :path-normals-open
    :path-offset
    :path-simplify-par
    :path-simplify-rdp
    :range
    :rep
    :scale
    :scale*
    :sfloat
    :sfloat*
    :stitch
    :sub
    :sum
    :with-linspace)
  (:import-from :common-lisp-user
    :array-push
    :to-array
    :to-list
    :make-generic-array
    :with-gensyms))


(defpackage :rnd
  (:use :common-lisp)
  (:export
    :aget
    :array-split
    :bernoulli
    :either
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
    :non-line*
    :norm
    :nrnd
    :nrnd*
    :nrnd-from
    :nrnd-u-from
    :nrndbtwn
    :nrndi
    :nrndi*
    :on-circ
    :on-line
    :on-line*
    :prob
    :rcond
    :rnd
    :rnd*
    :rndbtwn
    :rndi
    :rndi*
    :rndspace
    :rndspacei
    :with-in-circ
    :with-on-line
    :with-prob
    :with-rndspace)
  (:import-from :common-lisp-user
    :PII
    :to-array
    :array-push
    :make-generic-array
    :with-gensyms))


(defpackage :color
  (:use :common-lisp)
  (:export
    :black
    :cmyk
    :dark
    :from-list
    :gray
    :hsv
    :mdark
    :rgb
    :rgba
    :show
    :show
    :to-list
    :to-list*
    :transparent
    :vdark
    :white
    :with)
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
    :get-incident-edges
    :get-num-edges
    :get-num-verts
    :get-verts
    :make
    :mem
    :to-list
    :vmem
    :with-graph-edges)
  (:import-from :common-lisp-user
    :flatten
    :make-generic-array
    :with-struct
    :with-gensyms))


(defpackage :bzspl
  (:use :common-lisp)
  (:export
    :adaptive-pos
    :len
    :make
    :pos
    :pos*
    :rndpos
    :with-rndpos)
  (:import-from :common-lisp-user
    :array-push
    :to-list
    :make-generic-array
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
    :arr-circ
    :arr-pix
    :chromatic-aberration
    :circ
    :clear
    :dens-stroke
    :lin-path
    :make
    :bzspl-stroke
    :pix
    :pixel-hack
    :save
    :set-fg-color
    :stroke
    :strokes)
  (:import-from :common-lisp-user
    :aif
    :ensure-filename
    :get-dfloat-tup
    :square-loop
    :with-struct))

(defpackage :plot
  (:use :common-lisp)
  (:export
    :circ
    :dot-stroke
    :line
    :make
    :path
    :save
    :stipple-stroke
    :stipple-strokes)
  (:import-from :common-lisp-user
    :aif
    :ensure-filename
    :half
    :make-generic-array
    :square-loop
    :with-struct))


(defpackage :plot-svg
  (:use :common-lisp)
  (:export
    :a4-landscape
    :a4-portrait
    :bzspl
    :bzx
    :circ
    :circs
    :cpath
    :hatch
    :make
    :make*
    :mhatch
    :path
    :save
    :wbzspl
    :wcirc
    :wpath)
  (:import-from :common-lisp-user
    :aif
    :ensure-filename
    :make-generic-array
    :array-push
    :close-path
    :to-array
    :to-list
    :with-struct))


(defpackage :obj
  (:use :common-lisp)
  (:export
    :add-face
    :add-verts-from-vec
    :make
    :save)
  (:import-from :common-lisp-user
    :aif
    :array-push
    :ensure-filename
    :make-generic-array
    :with-struct))

(defpackage :snek
  (:use :common-lisp)
  (:export
    :add-edge!
    :add-edge*?
    :add-edges!
    :add-grp!
    :add-path!
    :add-path*!
    :add-polygon!
    :add-prm!
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
    :get-all-grps
    :get-all-prms
    :get-all-verts
    :get-edges
    :get-grp
    :get-grp-as-bzspl
    :get-grp-verts
    :get-incident-edges
    :get-num-edges
    :get-num-verts
    :get-prm
    :get-prm-vert-inds
    :get-prm-verts
    :get-props
    :get-vert
    :get-vert-inds
    :get-verts
    :is-vert-in-grp
    :itr-all-verts
    :itr-edges
    :itr-grps
    :itr-prms
    :itr-verts
    :itr-prm-verts
    :join-verts?
    :make
    :make-mutate
    :move-vert?
    :mutate
    :prmf
    :prmr
    :psvg-get-prm-types
    :split-edge?
    :verts-in-rad
    :with
    :with-dx
    :with-grp
    :with-rnd-edge
    :with-rnd-vert
    :zwith
    :with-verts-in-rad)
  (:import-from :common-lisp-user
    :append-postfix
    :close-path
    :flatten
    :get-dfloat-tup
    :make-dfloat-array
    :make-generic-array
    :make-hash-table-init
    :to-list
    :array-push
    :with-gensyms
    :with-struct))

