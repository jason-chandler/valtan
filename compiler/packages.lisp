(defpackage :compiler
  (:use :cl)
  (:export :compile-stdin))

(defpackage :system
  (:use)
  (:export :unquote
           :unquote-splicing
           :quasiquote
           :fset
           :add-global-macro
           :add-symbol-macro
           :%error
           :make-structure
           :structure-ref
           :structure-set))

(defpackage :ffi
  (:use)
  (:export :console.log
           :object
           :var
           :ref
           :set
           :instanceof
           :define-function
           :require
           :typeof
           :new
           :%aget))

(defpackage :clscript-system
  (:use))

(defpackage :js
  (:use))
