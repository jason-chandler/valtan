(in-package :common-lisp)

(defparameter *whitespaces* '(#\space #\tab #\newline #\linefeed #\page #\return))

(defun whitespacep (c)
  (find c *whitespaces*))

(defun peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value
                            recursive-p)
  (cond ((null peek-type)
         (let ((c (stream-peek-char stream)))
           (if (eq c :eof)
               (if eof-error-p
                   (eof-error)
                   eof-value)
               c)))
        (t
         (do ()
             ()
           (let ((c (stream-peek-char stream)))
             (cond ((eq c :eof)
                    (if eof-error-p
                        (eof-error)
                        (return eof-value)))
                   ((if (eq peek-type t)
                        (not (whitespacep c))
                        (eql c peek-type))
                    (return c))
                   (t
                    (stream-read-char stream))))))))

(defstruct (readtable (:predicate readtablep))
  (case :upcase)
  (table (make-hash-table)))

(defvar *readtable* (make-readtable))

(defun get-macro-character (char &optional (readtable *readtable*))
  (let ((value (gethash char (readtable-table readtable))))
    (if value
        (values (car value) (cdr value))
        (values nil nil))))

(defun set-macro-character (char function &optional non-terminating-p (readtable *readtable*))
  (setf (gethash char (readtable-table readtable))
        (cons function non-terminating-p))
  t)

(defun terminate-macro-character-p (c)
  (multiple-value-bind (function non-terminating-p)
      (get-macro-character c)
    (and function non-terminating-p)))

(defun number-string-p (token)
  (let ((pos (case (aref token 0)
               ((#\+ #\-) 1)
               (otherwise 0))))
    (do ((i pos (1+ i)))
        ((>= i (length token)))
      (let ((c (aref token i)))
        (cond ((char<= #\0 c #\9))
              ((char= #\. c)
               (setq pos (1+ i))
               (return))
              (t
               (return-from number-string-p nil)))))
    (do ((i pos (1+ i)))
        ((>= i (length token)))
      (let ((c (aref token i)))
        (cond ((char<= #\0 c #\9))
              (t
               (return-from number-string-p nil)))))
    t))

(defun parse-token (token)
  (if (number-string-p token)
      (ffi::parse-float token)
      (intern token)))

(defun read-multiple-escape-1 (stream)
  (with-output-to-string (out)
    (do ()
        (nil)
      (let ((c (read-char stream)))
        (case c
          (#\| (return))
          (#\\)
          (otherwise
           (write-char c out)))))))

(defun read-multiple-escape (stream)
  (parse-token (read-multiple-escape-1 stream)))

(defun read-token-1 (stream c)
  (with-output-to-string (out)
    (do ((c c (read-char stream nil nil)))
        (nil)
      (cond ((null c)
             (return))
            ((whitespacep c)
             (return))
            ((get-macro-character c)
             (unread-char c stream)
             (return))
            ((char= c #\\)
             (write-char (read-char stream) out))
            ((char= c #\|)
             (write-string (read-multiple-escape-1 stream) out))
            (t
             (write-char (case (readtable-case *readtable*)
                           (:upcase (char-upcase c))
                           (:downcase (char-downcase c))
                           (otherwise c))
                         out))))))

(defun read-token (stream c)
  (parse-token (read-token-1 stream c)))

(defun read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (do () (nil)
    (let* ((inner-eof-value '#:eof)
           (c (peek-char t stream eof-error-p inner-eof-value recursive-p)))
      (cond ((eq c inner-eof-value)
             (return eof-value))
            (t
             (read-char stream)
             (multiple-value-bind (function non-terminating-p)
                 (get-macro-character c)
               (cond
                 (function
                  (let ((values (multiple-value-list (funcall function stream c))))
                    (when values
                      (return (first values)))))
                 ((char= c #\|)
                  (return (read-multiple-escape stream)))
                 (t
                  (return (read-token stream c))))))))))

(defun read-list (stream c)
  (declare (ignore c))
  (let ((list '()))
    (do () (nil)
      (let ((c (peek-char t stream)))
        (when (char= c #\))
          (read-char stream)
          (return))
        (let ((x (read stream)))
          (push x list))))
    (nreverse list)))

(defun read-right-paren (stream c)
  (declare (ignore stream c))
  (error "unmatched close parenthesis"))

(set-macro-character #\( 'read-list)
(set-macro-character #\) 'read-right-paren)

(defun read-from-string (string &optional eof-error-p eof-value)
  (with-input-from-string (in string)
    (read in eof-error-p eof-value)))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((c (stream-read-char stream)))
    (if (eq c :eof)
        (if eof-error-p
            (eof-error)
            eof-value)
        c)))

(defun unread-char (character &optional (stream *standard-input*))
  (stream-unread-char stream))

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (multiple-value-bind (string next-line-p)
      (stream-read-line stream)
    (if (and (string= string "") (not next-line-p))
        (if eof-error-p
            (eof-error)
            (values eof-value t))
        (values string (not next-line-p)))))
