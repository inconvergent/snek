
(defpackage :vec
  (:use :common-lisp)
  (:export
    :*one*
    :*half*
    :*zero*
    :add
    :add-scaled
    :all-inside
    :angle
    :copy
    :cos-sin
    :cross
    :div
    :dot
    :dst
    :dst*
    :dst2
    :flip
    :idiv
    :iscale
    :isub
    :ladd
    :ladd*
    :ldiv
    :ldiv*
    :ldot
    :ldst
    :ldst*
    :len
    :len2
    :lmid
    :lmid
    :lmult
    :lmult*
    :lrot
    :lround
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
    :rect
    :rep
    :rot
    :sarr-get
    :sarr-set
    :scale
    :segdst
    :segx
    :segx*
    :shift-scale
    :shift-scale*
    :sin-cos
    :square
    :sub
    :sum
    :tolist
    :vec
    :vec*
    :vec-coerce
    :with-loop-grid
    :with-loop-grid*
    :with-xy
    :with-xy-short
    :zero)
  (:import-from :common-lisp-user
    :array-push
    :close-path
    :make-generic-array
    :with-gensyms))


(defpackage :math
  (:use :common-lisp)
  (:export
    :add
    :close-path
    :convex-split
    :copy-sort
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
    :path-simplify
    :path-tangents
    :percentiles
    :range
    :range-search
    :rep
    :scale
    :scale*
    :sfloat
    :sfloat*
    :stipple
    :stitch
    :sub
    :sum
    :with-linspace)
  (:import-from :common-lisp-user
    :array-last
    :array-push
    :ensure-array
    :make-generic-array
    :to-array
    :to-generic-array
    :to-list
    :with-gensyms))


(defpackage :rnd
  (:use :common-lisp)
  (:export
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
    :make-rnd-state
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
    :probsel
    :rcond
    :rnd
    :rnd*
    :rndbtwn
    :rndget
    :rndi
    :rndi*
    :rndspace
    :rndspacei
    :set-rnd-state
    :with-in-circ
    :with-on-line
    :with-prob
    :with-rndspace)
  (:import-from :common-lisp-user
    :array-push
    :make-generic-array
    :ensure-array
    :to-array
    :with-gensyms))


(defpackage :state
  (:use :common-lisp)
  (:export
    :awith
    :kset
    :make
    :sget
    :sset
    :with)
  (:import-from :common-lisp-user
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
    :with
    :with*)
  (:import-from :common-lisp-user
    :ensure-array
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
    :with-gensyms
    :with-struct))



(defpackage :bzspl
  (:use :common-lisp)
  (:export
    :adaptive-pos
    :len
    :make
    :normal
    :pos
    :pos*
    :rndpos
    :tangent
    :with-rndpos)
  (:import-from :common-lisp-user
    :array-push
    :make-generic-array
    :to-list
    :with-gensyms
    :with-struct))


(defpackage :lin-path
  (:use :common-lisp)
  (:export
    :make
    :move
    :pos
    :pos*
    :rndpos)
  (:import-from :common-lisp-user
    :with-struct))


(defpackage :zmap
  (:use :common-lisp)
  (:export
    :make
    :verts-in-rad
    :with*
    :with-verts-in-rad)
  (:import-from :common-lisp-user
    :make-generic-array
    :with-gensyms
    :with-struct))



(defpackage :sandpaint
  (:use :common-lisp)
  (:export
    :arr-circ
    :arr-pix
    :bzspl-stroke
    :chromatic-aberration
    :circ
    :clear
    :dens-stroke
    :lin-path
    :make
    :pix
    :pix-overlap
    :pix-overlap*
    :pixel-hack
    :save
    :set-fg-color
    :set-bg-color
    :stroke
    :strokes)
  (:import-from :common-lisp-user
    :aif
    :ensure-filename
    :with-gensyms
    :with-struct))


(defpackage :plot-svg
  (:use :common-lisp)
  (:export
    :*short*
    :*long*
    :a3-landscape
    :a3-portrait
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
    :show-boundary
    :show-crop
    :wbzspl
    :wcirc
    :wpath)
  (:import-from :common-lisp-user
    :aif
    :array-push
    :close-path
    :ensure-filename
    :make-generic-array
    :to-array
    :to-list
    :with-struct))


(defpackage :plot-tile-svg
  (:use :common-lisp)
  (:export
    :lstipple
    :lstipple*
    :make
    :path
    :rstipple
    :save)
  (:import-from :common-lisp-user
    :append-postfix
    :array-push
    :array-push*
    :ensure-array
    :ensure-filename
    :length-1
    :make-generic-array
    :to-array
    :to-generic-array
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
    :alt-then?
    :append-edge-segx?
    :append-edge?
    :del-edge!
    :del-edge?
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
    :get-grp-props
    :get-grp-verts
    :get-incident-edges
    :get-num-edges
    :get-num-verts
    :get-prm
    :get-prm-props
    :get-prm-vert-inds
    :get-prm-verts
    :get-vert
    :get-vert-by-name
    :get-vert-ind-by-name
    :get-vert-inds
    :get-vert-inds-by-name
    :get-verts
    :get-verts-by-name
    :is-vert-in-grp
    :itr-all-verts
    :itr-edges
    :itr-grps
    :itr-prm-verts
    :itr-prms
    :itr-verts
    :join-verts?
    :make
    :make-mutate
    :move-vert?
    :mutate
    :prmf
    :prmr
    :prune-edges-by-len
    :psvg-get-prm-types
    :sel-args
    :set-grp-props
    :set-prm-props
    :split-edge?
    :verts-in-rad
    :with
    :with-dx
    :with-grp
    :with-rnd-edge
    :with-rnd-vert
    :with-verts-in-rad
    :zwith)
  (:import-from :common-lisp-user
    :append-postfix
    :array-push
    :close-path
    :exec-with-args
    :flatten
    :make-generic-array
    :make-generic-hash-table
    :to-list
    :with-gensyms
    :with-struct))


;(declaim (inline rnd:rnd rnd:rndi rnd:rnd* rnd:rndbtwn rnd:on-line rnd:in-circ))
;(declaim (inline vec:zero vec:one vec:vec vec:cos-sin vec:sin-cos vec:scale
;                 vec:iscale vec:add-scaled vec:dot vec:cross vec:dst vec:dst2
;                 vec:add vec:sub vec:mult vec:mid vec:div vec:perp vec:flip
;                 vec:copy vec:neg vec:on-line))

