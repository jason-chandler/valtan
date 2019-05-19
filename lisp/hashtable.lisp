(in-package :common-lisp)

#+(or)
(export '(hash-table
          hash-table-p
          hash-table-test
          hash-table-size
          hash-table-rehash-size
          hash-table-rehash-threshold
          make-hash-table
          hash-table-count
          gethash
          remhash
          maphash
          with-hash-table-iterator
          clrhash
          sxhash))

(defstruct (hash-table (:copier nil)
                       (:predicate hash-table-p)
                       (:constructor %make-hash-table))
  object
  (test :read-only t)
  (size :read-only t)
  (rehash-size :read-only t)
  (rehash-threshold :read-only t))

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (%make-hash-table :object (ffi:make-object)
                    :test test
                    :size size
                    :rehash-size rehash-size
                    :rehash-threshold rehash-threshold))

(defun hash-table-count (hash-table)
  (ffi:object-get ((ffi:ref "Object" "keys") (hash-table-object hash-table))
                  "length"))

(defun gethash (key hash-table &optional default)
  (let ((value (ffi:object-get (hash-table-object hash-table) key)))
    (if (eq (ffi:typeof value) "undefined")
        (values default nil)
        (values value t))))

(defun (setf gethash) (value key hash-table &optional default)
  (ffi:object-set (hash-table-object hash-table) key value)
  value)

(defun remhash (key hash-table)
  (multiple-value-bind (value found)
      (gethash key hash-table)
    (declare (ignore value))
    (setf (gethash key hash-table) (ffi:ref "undefined"))
    found))

#|
(defun maphash (function hash-table)
  )

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  )

(defun clrhash (hash-table)
  )
|#
