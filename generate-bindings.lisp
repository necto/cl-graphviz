
;;; Substract from verrazano examples. Note: verrazano (http://common-lisp.net/project/fetter/) doesn't work under on clisp. It works on sbcl.
;;; Unfortunately verrazano's output needs some preprocessing anyway.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :verrazano))

(in-package :verrazano-user)

(defun generate-binding* (name headers &rest args
                          &key (working-directory (make-pathname :directory '()))
                          (debug nil)
                          (gccxml-flags "-I/usr/include")
                          &allow-other-keys)
  (format *debug-io* "~%~%; *** Processing binding ~S~%" name)
  (remove-from-plistf args :working-directory :gccxml-flags :debug)
  (block try
    (handler-bind ((serious-condition
                    (lambda (error)
                      (unless debug
                        (warn "Failed to generated binding for ~S, error: ~A" name error)
                        (return-from try)))))
      (let ((*print-right-margin* 100))
        (generate-binding (append
                           (list :cffi
                                 :package-name name
                                 :input-files headers
                                 :working-directory working-directory
                                 :gccxml-flags gccxml-flags)
                           args)
                          :keep-temporary-files nil))))
  (values))

(defun generate-graphviz-binding ()
  (generate-binding*
   :graphviz-cffi-bindings
   '("graphviz/gvc.h"
     "graphviz/graph.h"
     "graphviz/types.h"
     "graphviz/geom.h")
   :gccxml-flags "`pkg-config --cflags libgvc`"
   :node-filter (lambda (node)
                  (or (not (typep node 'gccxml:macro))
                      (not (search "DEPRECATED_BY"
                                   (verrazano::raw-body-of node)))))
   ))

(generate-graphviz-binding)

(sb-ext:quit)

