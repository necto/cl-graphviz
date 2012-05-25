
(defsystem :cl-graphviz
  :version "0.1"
  :author "Attila Lendvai <attila.lendvai@gmail.com>"
  :maintainer "Attila Lendvai <attila.lendvai@gmail.com>"
  :licence "MIT Style License"
  :description "CFFI bindings for GraphViz"
  :components
  ((:file "package")
   (:file "graphviz-cffi-bindings" :depends-on ("package"))
   (:file "public-layer" :depends-on ("graphviz-cffi-bindings")))
  :depends-on (:cffi :metabang-bind))
