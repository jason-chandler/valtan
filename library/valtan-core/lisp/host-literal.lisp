(in-package :valtan-core)

(common-lisp:defvar *valtan-readtable* (common-lisp:copy-readtable common-lisp:nil))
(common-lisp:defvar *plain-readtable* (common-lisp:copy-readtable common-lisp:nil))

(common-lisp:defmethod common-lisp:make-load-form ((object structure) common-lisp:&optional environment)
  (common-lisp:declare (common-lisp:ignore environment))
  (common-lisp:make-load-form-saving-slots object))

(common-lisp:set-macro-character
 #\"
 (common-lisp:lambda (s c)
   (common-lisp:declare (common-lisp:ignore c))
   (common-lisp:unread-char #\" s)
   (common-lisp:let* ((common-lisp:*readtable* *plain-readtable*)
             (raw-string (common-lisp:read s common-lisp:t common-lisp:nil common-lisp:t)))
     (system::make-structure-array! raw-string)))
 nil
 *valtan-readtable*)

(common-lisp:set-dispatch-macro-character
 #\# #\(
 (common-lisp:lambda (s c n)
   (common-lisp:declare (common-lisp:ignore c n))
   (common-lisp:unread-char #\( s)
   (common-lisp:let* ((common-lisp:*readtable* *plain-readtable*)
             (raw-array (common-lisp:read s common-lisp:t common-lisp:nil common-lisp:t)))
     (system::make-structure-array! (common-lisp:coerce raw-array 'common-lisp:vector) t)))
 *valtan-readtable*)

(common-lisp:set-dispatch-macro-character
 #\# #\*
 (common-lisp:lambda (s c n)
   (common-lisp:declare (common-lisp:ignore c n))
   (common-lisp:let* ((common-lisp:*readtable* *plain-readtable*)
             (bits (common-lisp:coerce
                    (common-lisp:loop
                      :while (common-lisp:member (common-lisp:peek-char common-lisp:nil s common-lisp:t common-lisp:nil common-lisp:t)
                                        '(#\0 #\1))
                      :collect (common-lisp:ecase (common-lisp:read-char s common-lisp:t common-lisp:nil common-lisp:t)
                                 (#\0 0)
                                 (#\1 1)))
                    'common-lisp:vector)))
     (system::make-structure-array! bits 't)))
 *valtan-readtable*)

(common-lisp:set-dispatch-macro-character
 #\# #\"
 (common-lisp:lambda (s c n)
   (common-lisp:declare (common-lisp:ignore c n))
   (common-lisp:unread-char #\" s)
   (let ((common-lisp:*readtable* *plain-readtable*))
     (common-lisp:read s common-lisp:t common-lisp:nil common-lisp:t)))
 *valtan-readtable*)
