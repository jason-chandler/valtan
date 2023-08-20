(common-lisp:in-package :valtan-core)

(common-lisp:defparameter system:+null+ '#:null)

(common-lisp:defclass structure ()
  ((name :initarg :name :reader structure-name)
   (values :initarg :values :reader structure-values)))

(common-lisp:defmethod common-lisp:print-object ((object structure) stream)
  (common-lisp:case (structure-name object)
    ;; ((string array)
    ;;  (common-lisp:prin1 (common-lisp:first (structure-values object)) stream))
    (common-lisp:otherwise
     (common-lisp:print-unreadable-object (object stream)
       (common-lisp:format stream "STRUCTUER ~A ~S"
                  (structure-name object)
                  (structure-values object))))))

(common-lisp:defun system:structure-p (structure)
  (common-lisp:typep structure 'structure))

(common-lisp:defun system:make-structure (name &rest args)
  (common-lisp:make-instance 'structure :name name :values args))

(common-lisp:defun system:%structure-name (structure)
  (structure-name structure))

(common-lisp:defun system:%structure-slot-count (structure)
  (common-lisp:length (structure-values structure)))

(common-lisp:defun system:%structure-ref (structure index)
  (common-lisp:elt (structure-values structure) index))

(common-lisp:defun system:%copy-structure (structure)
  (common-lisp:make-instance 'structure
                    :name (structure-name structure)
                    :values (structure-values structure)))

(common-lisp:defun system:%structure-set (structure index value)
  (common-lisp:setf (common-lisp:elt (structure-values structure) index)
           value))

(common-lisp:defun system::make-structure-array! (contents &optional (element-type t element-type-p))
  (system:make-structure 'array
                         contents
                         (common-lisp:list (common-lisp:length contents))
                         (common-lisp:length contents)
                         nil
                         nil
                         nil
                         1
                         (common-lisp:cond (element-type-p element-type)
                                  ((common-lisp:stringp contents) 'character)
                                  (t 't))))


;;; lisp.jsに対応
(common-lisp:defun system:make-symbol (name)
  (common-lisp:make-symbol name))

(common-lisp:defun system:symbol-plist (symbol)
  (common-lisp:symbol-plist symbol))

(common-lisp:defun system:put-symbol-plist (symbol plist)
  (common-lisp:setf (common-lisp:symbol-plist symbol) plist))

(common-lisp:defun system:symbol-value (symbol)
  (common-lisp:symbol-value symbol))

(common-lisp:defun system:symbol-function (symbol)
  (common-lisp:symbol-function symbol))

(common-lisp:defun system:%set (symbol value)
  (common-lisp:set symbol value))

(common-lisp:defun system:symbol-name (symbol)
  (common-lisp:or (common-lisp:symbol-name symbol)
         system:+null+))

(common-lisp:defun system:symbol-package-name (symbol)
  (common-lisp:let ((package (common-lisp:symbol-package symbol)))
    (if package
        (common-lisp:package-name package)
        system:+null+)))

(common-lisp:defun system:fset (symbol function)
  (common-lisp:setf (common-lisp:symbol-function symbol) function))

(common-lisp:defun system:map-package-symbols (package function)
  (common-lisp:do-symbols (s package nil)
    (common-lisp:funcall function s)))

(common-lisp:defun system:put (symbol key value)
  (common-lisp:setf (common-lisp:get symbol key) value))

(common-lisp:defun system:package-name (package)
  (common-lisp:package-name package))

(common-lisp:defun system:package-nicknames (package)
  (common-lisp:package-nicknames package))

(common-lisp:defun system:intern (name package)
  (common-lisp:intern name package))

(common-lisp:defun system:find-symbol (name package)
  (common-lisp:find-symbol name package))

(common-lisp:defun system:make-package (name nicknames use-package-names)
  (common-lisp:make-package name :nicknames nicknames :use use-package-names))

(common-lisp:defmacro system:%defpackage (package &key export use nicknames)
  (flet ((ref-string (structure)
           (if (stringp structure)
               (system:%structure-ref structure 0)
               structure)))
    `(let ((package ',(ref-string package))
           (export ',(mapcar #'ref-string export))
           (use ',(mapcar #'ref-string use))
           (nicknames ',(mapcar #'ref-string nicknames)))
       (let ((package (or (common-lisp:find-package package)
                          (common-lisp:make-package package :use use :nicknames nicknames))))
         (common-lisp:export export package)
         package))))

(common-lisp:defun system:export (symbols package)
  (common-lisp:export symbols package))

(common-lisp:defun system:%add (x y)
  (common-lisp:+ x y))

(common-lisp:defun system:%sub (x y)
  (common-lisp:- x y))

(common-lisp:defun system:%negate (x)
  (common-lisp:- x))

(common-lisp:defun system:%mul (x y)
  (common-lisp:* x y))

(common-lisp:defun system:%rem (x y)
  (values (common-lisp:rem x y)))

(common-lisp:defun system:%floor (x y)
  (values (common-lisp:floor x y)))

(common-lisp:defun system:%logand (x y)
  (common-lisp:logand x y))

(common-lisp:defun system:%= (x y)
  (common-lisp:= x y))

(common-lisp:defun system:%/= (x y)
  (common-lisp:/= x y))

(common-lisp:defun system:%> (x y)
  (common-lisp:> x y))

(common-lisp:defun system:%< (x y)
  (common-lisp:< x y))

(common-lisp:defun system:%>= (x y)
  (common-lisp:>= x y))

(common-lisp:defun system:%<= (x y)
  (common-lisp:<= x y))

(common-lisp:defun system:apply (function args)
  (common-lisp:apply function args))

(common-lisp:defun system:%car (x)
  (common-lisp:car x))

(common-lisp:defun system:%cdr (x)
  (common-lisp:cdr x))

(common-lisp:defun system:%rplaca (cons x)
  (common-lisp:rplaca cons x))

(common-lisp:defun system:%rplacd (cons x)
  (common-lisp:rplacd cons x))

(common-lisp:defun system:raw-array-to-list (raw-array)
  raw-array)

(common-lisp:defun system:list-to-raw-array (list)
  list)

(common-lisp:defmacro system:multiple-value-call (function common-lisp:&rest args)
  `(common-lisp:multiple-value-call ,function ,@args))

(common-lisp:defun system:error (value)
  (common-lisp:error "~A" (system:%structure-ref value 0)))

(common-lisp:defun system:%code-char (code)
  (common-lisp:code-char code))

(common-lisp:defun system:%char-code (char)
  (common-lisp:char-code char))


(common-lisp:defmacro system:defmacro* (name lambda-list common-lisp:&body body)
  `(common-lisp:defmacro ,name ,lambda-list ,@body))

(common-lisp:defun system:make-raw-string ()
  (common-lisp:make-string 0))

(common-lisp:defun system:expand-raw-string (raw-string n)
  (let ((new-string (common-lisp:make-string n :initial-element #.(common-lisp:code-char 0))))
    (common-lisp:replace new-string raw-string)
    new-string))

(common-lisp:defun system:code-to-raw-string (code)
  (common-lisp:string (common-lisp:code-char code)))

(common-lisp:defun system:sub-raw-string/2 (raw-string start)
  (common-lisp:subseq raw-string start))

(common-lisp:defun system:sub-raw-string/3 (raw-string start end)
  (common-lisp:subseq raw-string start end))

(flet ((concat (&rest raw-strings)
         (common-lisp:apply #'common-lisp:concatenate
                   'common-lisp:string
                   (common-lisp:mapcar (common-lisp:lambda (raw-string)
                                (if (common-lisp:characterp raw-string)
                                    (common-lisp:string raw-string)
                                    raw-string))
                              raw-strings))))
  (common-lisp:defun system:concat-raw-string/2 (raw-string-1 raw-string-2)
    (concat raw-string-1 raw-string-2))

  (common-lisp:defun system:concat-raw-string/3 (raw-string-1 raw-string-2 raw-string-3)
    (concat raw-string-1 raw-string-2 raw-string-3)))

(common-lisp:defun system:raw-string-upcase (raw-string)
  (common-lisp:string-upcase raw-string))

(common-lisp:defun system:raw-string-downcase (raw-string)
  (common-lisp:string-downcase raw-string))

(common-lisp:defun system:number-to-raw-string (number)
  (common-lisp:princ-to-string number))

(common-lisp:defun system:make-raw-array (size)
  (common-lisp:make-array size))

(common-lisp:defun system:raw-array-length (raw-array)
  (common-lisp:length raw-array))

(common-lisp:defun system:raw-array-ref (raw-array index)
  (common-lisp:aref raw-array index))

(common-lisp:defun system:raw-array-set (raw-array index value)
  (common-lisp:setf (common-lisp:aref raw-array index) value))

(common-lisp:defun system:fill-raw-array (raw-array element)
  (common-lisp:fill raw-array element))

(common-lisp:defun system:make-map ()
  (common-lisp:make-hash-table :test 'common-lisp:equal))

(common-lisp:defun system:map-get (map key)
  (common-lisp:gethash key map))

(common-lisp:defun system:map-set (map key value)
  (common-lisp:setf (common-lisp:gethash key map) value))

(common-lisp:defun system:map-remove (map key)
  (common-lisp:remhash key map))

(common-lisp:defun system:map-length (map)
  (common-lisp:hash-table-count map))

(common-lisp:defun system:map-clear (map)
  (common-lisp:clrhash map))

(common-lisp:defun system:function-name (function)
  (common-lisp:declare (common-lisp:ignore function))
  (common-lisp:error "unimplemented"))

(common-lisp:defun system:unknown-object-to-string (object)
  (common-lisp:declare (common-lisp:ignore object))
  (common-lisp:error "unimplemented"))

(common-lisp:defun system:array-to-raw-string (array)
  (common-lisp:declare (common-lisp:ignore array))
  (common-lisp:error "unimplemented"))

(common-lisp:defun system:raw-string-to-array (raw-string)
  (common-lisp:declare (common-lisp:ignore raw-string))
  (common-lisp:error "unimplemented"))

(common-lisp:defun system:raw-array-to-array (raw-array)
  (common-lisp:declare (common-lisp:ignore raw-array))
  (common-lisp:error "unimplemented"))

(common-lisp:defun system:read-whole-file (filename)
  (common-lisp:with-open-file (in filename)
    (common-lisp:with-output-to-string (out)
      (common-lisp:let* ((buffer-size 4096)
                (buffer (common-lisp:make-array buffer-size :element-type 'common-lisp:character)))
        (common-lisp:loop
          :for bytes-read := (common-lisp:read-sequence buffer in)
          :do (common-lisp:write-sequence buffer out :start 0 :end bytes-read)
          :while (common-lisp:= bytes-read buffer-size))))))

(common-lisp:defun system:write-raw-string-to-stdout (raw-string)
  (common-lisp:write-string raw-string))

(common-lisp:defun system:random (n)
  (common-lisp:random n))


(common-lisp:defun js::-object ()
  (common-lisp:error "unimplemented"))

(common-lisp:defun js::eval (x)
  (common-lisp:declare (common-lisp:ignore x))
  (common-lisp:error "unimplemented"))

(common-lisp:defun js::console.log (raw-string)
  (common-lisp:declare (common-lisp:ignore raw-string))
  (common-lisp:error "unimplemented"))


(common-lisp:defun ffi:set (var value)
  (common-lisp:declare (common-lisp:ignore var value))
  (common-lisp:error "unimplemented"))

(common-lisp:defun ffi:aget (array index)
  (common-lisp:aref array index))

(common-lisp:defun ffi:ref (common-lisp:&rest args)
  (common-lisp:declare (common-lisp:ignore args))
  (common-lisp:error "unimplemented"))

(common-lisp:defun ffi:typeof (x)
  (common-lisp:declare (common-lisp:ignore x))
  (common-lisp:error "unimplemented"))

(common-lisp:defun ffi:instanceof (value instance)
  (common-lisp:declare (common-lisp:ignore value instance))
  (common-lisp:error "unimplemented"))
