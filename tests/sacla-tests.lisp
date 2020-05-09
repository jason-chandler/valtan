(defvar *results* nil)

(defun test (filename)
  (format t "test: ~A~%" filename)
  (with-open-file (in filename)
    (let ((eof-value (gensym))
          (cl-user-package (find-package :cl-user))
          (pass 0)
          (fail 0))
      (do ((n 0 (1+ n)))
          (nil)
        (handler-case
            (let ((*package* cl-user-package)
                  (form (read in nil eof-value)))
              (when (eq form eof-value)
                (return))
              (cond ((eval form)
                     (incf pass)
                     (format t "~D Pass: ~S~%" n form))
                    (t
                     (incf fail)
                     (format t "~D Failed: ~S~%" n form))))
          (error (e)
            (incf fail)
            (format t "~D Failed: ~A~%" n e))))
      (format t "Pass: ~D, Failed: ~D~%~%" pass fail)
      (push (list filename pass fail) *results*))))

(defmacro time (form)
  (let ((start (gensym)))
    `(let ((,start (js:-date.now)))
       ,form
       (format t "~&time: ~A~%" (- (js:-date.now) ,start)))))

(defun run-sacla-tests ()
  (let ((*results* '()))
    (time
     (progn
       (test "sacla-tests/desirable-printer.lisp")
       (test "sacla-tests/must-array.lisp")
       (test "sacla-tests/must-character.lisp")
       (test "sacla-tests/must-condition.lisp")
       (test "sacla-tests/must-cons.lisp")
       (test "sacla-tests/must-data-and-control.lisp")
       (test "sacla-tests/must-do.lisp")
       (test "sacla-tests/must-eval.lisp")
       (test "sacla-tests/must-hash-table.lisp")
       ;; (test "sacla-tests/must-loop.lisp")
       ;; (test "sacla-tests/must-package.lisp")
       ;; (test "sacla-tests/must-printer.lisp")
       ;; (test "sacla-tests/must-reader.lisp")
       (test "sacla-tests/must-sequence.lisp")
       (test "sacla-tests/must-string.lisp")
       (test "sacla-tests/must-symbol.lisp")
       (test "sacla-tests/should-array.lisp")
       (test "sacla-tests/should-character.lisp")
       (test "sacla-tests/should-cons.lisp")
       (test "sacla-tests/should-data-and-control.lisp")
       (test "sacla-tests/should-eval.lisp")
       (test "sacla-tests/should-hash-table.lisp")
       (test "sacla-tests/should-package.lisp")
       (test "sacla-tests/should-sequence.lisp")
       (test "sacla-tests/should-string.lisp")
       (test "sacla-tests/should-symbol.lisp")
       ;; (test "sacla-tests/x-sequence.lisp")
       ))
    (print *results*)))
