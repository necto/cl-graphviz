(cl:defpackage :cl-graphviz
  (:use :common-lisp :cffi :bind)
  (:nicknames :graphviz)
  (:documentation "CL-GraphViz is a CFFI binding for GraphViz.")
  (:export
   #:*graphviz-foreign-library-directories*

   #:layout-dot-format
   #:edge-between
   #:node-name
   #:node-coordinate
   #:node-size
   #:graph-bounding-box

   #:iterate-edge-beziers
   #:bezier-points))

(in-package :cl-graphviz)

(defvar *graphviz-foreign-library-directories* (list "/usr/lib/graphviz/" "/usr/lib/"))

(define-foreign-library libgvc
  (:unix (:or "libgvc.so.4" "libgvc32.so.4"))
  (:darwin "libgvc.so")
  (:windows ("libgvc.dll" "msvcrt.dll"))
  (t "libgvc"))

(let ((*foreign-library-directories* *graphviz-foreign-library-directories*))
  (load-foreign-library 'libgvc))
