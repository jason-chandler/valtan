;; -*- mode:lisp -*-

(defsystem "valtan-core"
  :serial t
  :components ((:file "lisp/valtan-package")
               (:file "lisp/host-system-compat" :if-feature (:not :valtan))
               (:file "lisp/control")
               (:file "lisp/destructuring-bind")
               (:file "lisp/setf" :if-feature :valtan)
               (:file "lisp/ffi" :if-feature :valtan)
               (:file "lisp/cons" :if-feature :valtan)
               (:file "lisp/condition" :if-feature :valtan)
               (:file "lisp/struct" :if-feature :valtan)
               (:file "lisp/symbol" :if-feature :valtan)
               (:file "lisp/type" :if-feature :valtan)
               (:file "lisp/number" :if-feature :valtan)
               (:file "lisp/array" :if-feature :valtan)
               (:file "lisp/character" :if-feature :valtan)
               (:file "lisp/string" :if-feature :valtan)
               (:file "lisp/function" :if-feature :valtan)
               (:file "lisp/sequence" :if-feature :valtan)
               (:file "lisp/hashtable" :if-feature :valtan)
               (:file "lisp/package" :if-feature :valtan)
               (:file "lisp/stream" :if-feature :valtan)
               (:file "lisp/print" :if-feature :valtan)
               (:file "lisp/read" :if-feature :valtan)
               (:file "lisp/file" :if-feature :valtan)
               (:file "lisp/pkg" :if-feature :valtan)
               (:file "lisp/clos" :if-feature :valtan)
               (:file "lisp/restart" :if-feature :valtan)
               (:file "lisp/catch-throw" :if-feature :valtan)
               (:file "compiler/packages" :if-feature :valtan)
               (:file "compiler/variables" :if-feature :valtan)
               (:file "compiler/util" :if-feature :valtan)
               (:file "compiler/error" :if-feature :valtan)
               (:file "compiler/hir" :if-feature :valtan)
               (:file "compiler/pass1" :if-feature :valtan)
               ;; (:file "compiler/hir-walker" :if-feature (:not :valtan))
               ;; (:file "compiler/type-infer" :if-feature (:not :valtan))
               ;; (:file "compiler/hir-optimize" :if-feature (:not :valtan))
               (:file "compiler/pass2" :if-feature :valtan)
               (:file "compiler/compiler" :if-feature :valtan)
               (:file "lisp/compilation" :if-feature :valtan)))
