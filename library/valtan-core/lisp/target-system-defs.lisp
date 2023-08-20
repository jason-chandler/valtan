(common-lisp:in-package :common-lisp)

#+node (ffi:require js:fs "fs")

(common-lisp:define-symbol-macro system:+null+ (ffi:ref "null"))

(common-lisp:defun system:make-raw-string ()
  (ffi:new (ffi:ref "String")))

(common-lisp:defun system:expand-raw-string (raw-string n)
  ((ffi:ref raw-string "padEnd") n))

(common-lisp:defun system:code-to-raw-string (code)
  ((ffi:ref "String" "fromCharCode") code))

(common-lisp:defun system:sub-raw-string/2 (raw-string start)
  ((ffi:ref raw-string "substring") start))

(common-lisp:defun system:sub-raw-string/3 (raw-string start end)
  ((ffi:ref raw-string "substring") start end))

(common-lisp:defun system:concat-raw-string/2 (raw-string-1 raw-string-2)
  ((ffi:ref raw-string-1 "concat") raw-string-2))

(common-lisp:defun system:concat-raw-string/3 (raw-string-1 raw-string-2 raw-string-3)
  ((ffi:ref raw-string-1 "concat") raw-string-2 raw-string-3))

(common-lisp:defun system:raw-string-upcase (raw-string)
  ((ffi:ref raw-string "toUpperCase") raw-string))

(common-lisp:defun system:raw-string-downcase (raw-string)
  ((ffi:ref raw-string "toLowerCase") raw-string))

(common-lisp:defun system:number-to-raw-string (number)
  ((ffi:ref "String") number))

(common-lisp:defun system:make-raw-array (size)
  (ffi:new (ffi:ref "Array") size))

(common-lisp:defun system:raw-array-length (raw-array)
  (ffi:ref raw-array "length"))

(common-lisp:defun system:raw-array-ref (raw-array index)
  (ffi:aget raw-array index))

(common-lisp:defun system:raw-array-set (raw-array index value)
  (ffi:set (ffi:aget raw-array index) value))

(common-lisp:defun system:fill-raw-array (raw-array element)
  ((ffi:ref raw-array "fill") element))

(common-lisp:defun system:make-map ()
  (ffi:new (ffi:ref "Map")))

(common-lisp:defun system:map-get (map key)
  (let ((value ((ffi:ref map "get") key)))
    (values value
            (if (eq value (ffi:ref "undefined"))
                nil
                t))))

(common-lisp:defun system:map-set (map key value)
  ((ffi:ref map "set") key value))

(common-lisp:defun system:map-remove (map key)
  ((ffi:ref map "delete") key))

(common-lisp:defun system:map-length (map)
  (ffi:ref map "size"))

(common-lisp:defun system:map-clear (map)
  ((ffi:ref map "clear")))

(common-lisp:defun system:function-name (function)
  (let ((name (ffi:ref function "lisp_name")))
    (if (eq (ffi:typeof name) #j"string")
        (ffi:js->cl name))))

(common-lisp:defun system:unknown-object-to-string (object)
  (let ((object (ffi:js->cl object)))
    (ffi:js->cl ((ffi:ref "String") object))))

(common-lisp:defun system:read-whole-file (filename)
  ((ffi:ref "fs" "readFileSync")
   (*:array-to-raw-string filename)
   (*:array-to-raw-string "utf-8")))

(common-lisp:defun system:write-raw-string-to-stdout (raw-string)
  #+node
  ((ffi:ref "process" "stdout" "write") raw-string)
  #-node
  (js:console.log raw-string))

(common-lisp:defun system:random (n)
  ((ffi:ref "Math" "floor")
   (* ((ffi:ref "Math" "random"))
      ((ffi:ref "Math" "floor") n))))
