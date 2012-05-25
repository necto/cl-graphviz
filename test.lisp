;;;; Simple test, borrowed from http://common-lisp.net/project/cl-graphviz/index-old.shtml
;;;	It requires installed quicklisp facility on you system. Tested with 
;;;		clisp	: clisp -i ~/.clisprc.lisp test.lisp 
;;;		sbcl	: sbcl --load test.lisp
;;;	After asdf, quicklisp and compiler messaged, it should output something like:
;;; graph G {
;;; graph [bb="0.0d0,0.0d0, 0.0d0,587.0d0"];

;;; 5 [height=0.5, width=0.75, pos="99.0d0,18.0d0"];
;;; 4 [height=0.5, width=0.75, pos="330.0d0,106.0d0"];
;;; 3 [height=0.5, width=0.75, pos="27.0d0,194.0d0"];
;;; 2 [height=0.5, width=0.75, pos="324.0d0,194.0d0"];
;;; 1 [height=0.5, width=0.75, pos="175.0d0,282.0d0"];
;;; 0 [height=0.5, width=0.75, pos="175.0d0,370.0d0"];
;;; 4--5 [label="#<DOT-EDGE <#<E> #<F> NIL>>",pos="307.5d0,95.4d0 301.5d0,92.9d0 295.0d0,90.3d0 289.0d0,88.0d0 229.4d0,64.8d0 158.5d0,39.7d0 122.5d0,27.1d0"];
;;; 3--5 [label="#<DOT-EDGE <#<D> #<F> NIL>>",pos="21.1d0,176.1d0 14.7d0,154.6d0 6.9d0,116.7d0 20.0d0,88.0d0 31.9d0,61.8d0 59.2d0,41.6d0 78.3d0,30.0d0"];
;;; 3--4 [label="#<DOT-EDGE <#<D> #<E> NIL>>",pos="52.2d0,187.5d0 99.3d0,177.1d0 203.6d0,152.7d0 289.0d0,124.0d0 295.2d0,121.8d0 301.9d0,119.2d0 308.0d0,116.7d0"];
;;; 1--3 [label="#<DOT-EDGE <#<B> #<D> NIL>>",pos="148.3d0,278.5d0 110.2d0,274.3d0 42.8d0,264.4d0 28.0d0,246.0d0 20.4d0,236.6d0 20.6d0,222.8d0 22.4d0,211.9d0"];
;;; 1--2 [label="#<DOT-EDGE <#<B> #<C> NIL>>",pos="201.9d0,280.1d0 230.0d0,277.9d0 273.8d0,270.4d0 302.0d0,246.0d0 312.0d0,237.2d0 317.6d0,223.1d0 320.6d0,211.9d0"];
;;; 0--1 [label="#<DOT-EDGE <#<A> #<B> NIL>>",pos="175.0d0,351.5d0 175.0d0,336.5d0 175.0d0,315.0d0 175.0d0,300.0d0"];
;;; }
;;;
;;; Don't worry if the namber of numbers after commas will be more (ex. 307.51633081250503d0 instead of 307.5d0) it is really what I experience.


(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cl-graphviz))

(ql:quickload :cl-graph)

(in-package :cl-graph)

(let ((g (make-container 'dot-graph :default-edge-type :undirected)))
  (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
		(add-edge-between-vertexes g a b))
  (layout-graph-with-graphviz g)
  (format t "~A" (graph->dot g nil)))

(cl-user::quit)
