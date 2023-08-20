#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defun js-symbol-p (symbol)
  (common-lisp:and (symbolp symbol) (common-lisp:string= #"JS" (common-lisp:package-name (common-lisp:symbol-package symbol)))))

(defmacro ffi:define-function (name arguments &body body)
  `(progn
    (ffi:var ,name)
    (ffi:set
     ,(common-lisp:cond ((common-lisp:stringp name) name)
               ((js-symbol-p name)
                name)
               (t (common-lisp:string name)))
     (lambda ,arguments ,@body))))

(defmacro ffi:define (var value)
  `(progn
    (ffi:var ,var)
    (ffi:set
     ,(common-lisp:cond ((common-lisp:stringp var) var)
               ((js-symbol-p var) var)
               (t (common-lisp:string var)))
     (ffi:cl->js ,value))))

(defun ffi::%object (&rest plist)
  (let ((object (js::-object)))
    (common-lisp:do ((common-lisp:rest plist (common-lisp:cddr common-lisp:rest)))
           ((common-lisp:null common-lisp:rest))
      (let ((key (common-lisp:car common-lisp:rest)) (value (common-lisp:cadr common-lisp:rest)))
        (ffi:set
         (ffi:aget object
                   (common-lisp:cond ((common-lisp:stringp key) (ffi:cl->js key))
                            ((common-lisp:keywordp key)
                             (ffi:cl->js (compiler::kebab-to-lower-camel-case (common-lisp:string key))))
                            (t key)))
         value)))
    object))

(defmacro ffi:object (&rest plist)
  (let ((new-plist 'nil))
    (common-lisp:do ((plist plist (common-lisp:cddr plist)))
           ((common-lisp:null plist))
      (let ((key (common-lisp:car plist)) (value (common-lisp:cadr plist)))
        (common-lisp:push
         (common-lisp:cond ((common-lisp:stringp key) `(ffi:cl->js ,key))
                  ((common-lisp:keywordp key)
                   `(ffi:cl->js ,(compiler::kebab-to-lower-camel-case (common-lisp:string key))))
                  (t key))
         new-plist)
        (common-lisp:push value new-plist)))
    `(ffi::%object ,@(common-lisp:nreverse new-plist))))

(defun ffi:array (&rest args)
  (declare (ignorable args))
  #+valtan
  (common-lisp:apply (ffi:ref "Array") args)
  #-valtan
  (common-lisp:error "unimplemented"))

(defun ffi:js-eval (x)
  (declare (ignorable x))
  #+valtan
  (let* ((code (*:string-append* "(function(lisp) { 'use strict'; " x "; });"))
         (fn (js::eval (ffi:cl->js code))))
    (common-lisp:funcall fn (ffi:ref "lisp")))
  #-valtan
  (common-lisp:error "unimplemented"))

(defun ffi:cl->js (value)
  (common-lisp:cond ((common-lisp:stringp value) (array-contents value))
           ((common-lisp:vectorp value) (array-contents value))
           ;; ((listp value)
           ;;  (*:list-to-raw-array value))
           ;; ((eq value t)
           ;;  (ffi:ref "true"))
           ;; ((eq value nil)
           ;;  (ffi:ref "false"))
           ;; ((functionp value)
           ;;  (lambda (&rest args)
           ;;    (apply value (mapcar #'ffi:cl->js args))))
           (t value)))

(defun ffi:js->cl (value)
  (common-lisp:cond ((eq (ffi:typeof value) (*:array-to-raw-string "string"))
            (*:raw-string-to-array value))
           ((ffi:instanceof value (ffi:ref "Array"))
            (*:raw-array-to-array value))
           ;; ((eq value (ffi:ref "true"))
           ;;  t)
           ;; ((eq value (ffi:ref "false"))
           ;;  nil)
           (t value)))
