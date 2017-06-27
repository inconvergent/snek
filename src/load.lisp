
; TODO: difference between declaim/proclaim?
;(proclaim '(inline last1 single append1 conc1 mklist))
;(proclaim '(optimize speed))
;(declaim (optimize (speed 3) (debug 0) (safety 0)))
(proclaim '(optimize (speed 3) (space 0) (debug 0)))

; TODO: restructure this while file more once i know it works.

(load "~/quicklisp/setup.lisp")

(ql:quickload "zpng")


(asdf:defsystem "snek"
  :description "SNEK - A Generative System for Writing Generative Systems"
  :version "1.4.4"
  :author "inconvergent"
  :licence "MIT"
  :serial t
  :depends-on ("zpng")
  :components ((:file "pg-utils")
               (:file "utils")
               (:file "math")
               (:file "rnd")
               (:file "color")
               (:file "linear-path")
               (:file "bzspline")
               (:file "sandpaint")
               (:file "plot")
               (:file "zmap")
               (:file "snek-macros")
               (:file "snek")
               (:file "snek-alterations")
               (:file "snek-alterations-mutate")
               (:file "snek-utils")))

(asdf:load-system "snek")

