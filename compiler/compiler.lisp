(in-package :compiler)

(defvar *quasiquote-readtable*
  (let ((*readtable* (copy-readtable)))
    (set-macro-character
     #\`
     (lambda (s c)
       (declare (ignore c))
       (list 'system::quasiquote
             (read s t nil t))))
    (set-macro-character
     #\,
     (lambda (s c)
       (declare (ignore c))
       (cond ((eql #\@ (peek-char nil s t nil t))
              (read-char s t nil t)
              (list 'system::unquote-splicing (read s t nil t)))
             (t
              (list 'system::unquote
                    (read s t nil t))))))
    *readtable*))

(defmacro do-forms ((var stream) &body body)
  (let ((g-eof-value (gensym))
        (g-stream (gensym)))
    `(loop :with ,g-eof-value := '#:eof-value
           :and ,g-stream := ,stream
           :for ,var := (let ((*readtable* *quasiquote-readtable*))
                          (read ,g-stream nil ,g-eof-value))
           :until (eq ,var ,g-eof-value)
           :do (progn ,@body))))

(defun call-with-compile (function)
  (let ((*require-modules* '())
        (*defined-function-names* '())
        (*called-function-names* '()))
    (let ((ir-forms (funcall function)))
      (dolist (name (set-difference *called-function-names* *defined-function-names*))
        (warn "undefined function: ~S" name))
      (write-line "import * as lisp from 'lisp';")
      (dolist (module *require-modules*)
        (format t "require('~A.lisp');~%" module))
      (pass2-toplevel-forms ir-forms))
    (values)))

(defmacro with-compile (() &body body)
  `(call-with-compile (lambda () ,@body)))

(defun compile-stdin ()
  (with-compile ()
    (let ((ir-forms '()))
      (do-forms (form *standard-input*)
        (push (pass1-toplevel form) ir-forms))
      (nreverse ir-forms))))

(defun compile-files (files)
  (unless (listp files) (setf files (list files)))
  (with-compile ()
    (let ((ir-forms '()))
      (dolist (file files)
        (with-open-file (in file)
          (do-forms (form in)
            (push (pass1-toplevel form) ir-forms))))
      (nreverse ir-forms))))

(defun compile-toplevel (form)
  (with-compile ()
    (list (pass1-toplevel form))))

(defmacro with-js-beautify (&body body)
  `(let ((output
           (with-output-to-string (*standard-output*)
             (progn ,@body))))
     (with-input-from-string (in output)
       (uiop:run-program "js-beautify"
                         :input in
                         :output t))))

(defun get-lisp-files ()
  (let ((base-path (asdf:system-relative-pathname :clscript "./lisp/")))
    (mapcar (lambda (name)
              (make-pathname :name name :type "lisp" :defaults base-path))
            '("control" "condition" "print" "cons"))))

(defun build (&optional output-file)
  (with-open-stream (*standard-output*
                     (if output-file
                         (open output-file
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
                         (make-string-output-stream)))
    (compile-files (get-lisp-files))
    (unless output-file
      (get-output-stream-string *standard-output*))))
