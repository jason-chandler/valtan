#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defun copy-structure (x)
  (unless (*:structure-p x)
    (type-error x 'structure-object))
  (*:%copy-structure x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-structure-name-and-options (name-and-options)
    (common-lisp:multiple-value-bind (structure-name options)
        (if (common-lisp:consp name-and-options)
            (common-lisp:values (common-lisp:first name-and-options) (common-lisp:rest name-and-options))
            (common-lisp:values name-and-options nil))
      (let* ((conc-name (common-lisp:format nil #"~A-" structure-name))
             constructor-option
             included-option
             type-option
             named-option
             (constructor-name
              (common-lisp:intern (common-lisp:format nil #"MAKE-~A" structure-name)
                         (common-lisp:symbol-package structure-name)))
             (copier-name
              (common-lisp:intern (common-lisp:format nil #"COPY-~A" structure-name)
                         (common-lisp:symbol-package structure-name)))
             (predicate-name
              (common-lisp:intern (common-lisp:format nil #"~A-P" structure-name) (common-lisp:symbol-package structure-name)))
             print-function)
        (common-lisp:dolist (option options)
          (common-lisp:unless (common-lisp:consp option) (setq option (common-lisp:list option)))
          (common-lisp:ecase (common-lisp:first option)
            (:conc-name (setq conc-name (common-lisp:second option)))
            (:constructor
             (setq constructor-name (common-lisp:second option)
                   constructor-option (common-lisp:rest option)))
            (:copier (common-lisp:unless (common-lisp:null (common-lisp:rest option)) (setq copier-name (common-lisp:second option))))
            (:predicate
             (common-lisp:unless (common-lisp:null (common-lisp:rest option)) (setq predicate-name (common-lisp:second option))))
            (:include (setq included-option (cons (common-lisp:second option) (common-lisp:cddr option))))
            ((:print-object :print-function) (setq print-function (common-lisp:second option)))
            (:type (setq type-option (common-lisp:second option)))
            (:named (setq named-option t))))
        (common-lisp:list :structure-name structure-name :conc-name conc-name :constructor-option
                 constructor-option :included-option included-option :type-option type-option
                 :named-option named-option :constructor-name constructor-name :copier-name
                 copier-name :predicate-name predicate-name :print-function print-function)))))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (common-lisp:destructuring-bind
        (&key structure-name conc-name constructor-option included-option type-option named-option
              constructor-name copier-name predicate-name print-function)
      (parse-structure-name-and-options name-and-options)
    (declare (ignore included-option type-option named-option))
    (common-lisp:check-type structure-name common-lisp:symbol)
    `(progn
       ,@(common-lisp:unless (common-lisp:null copier-name) `((defun ,copier-name (x) (common-lisp:copy-structure x))))
       ,@(common-lisp:unless (common-lisp:null predicate-name)
           `((defun ,predicate-name (x) (typep x ',structure-name))))
       (defun ,constructor-name
           ,(if (common-lisp:and constructor-option (common-lisp:rest constructor-option))
                (common-lisp:second constructor-option)
                `(&key
                  ,@(common-lisp:mapcar
                     (lambda (slot-desc)
                       (if (common-lisp:consp slot-desc)
                           (common-lisp:list (common-lisp:first slot-desc) (common-lisp:second slot-desc))
                           slot-desc))
                     slot-descriptions)))
         (*:make-structure ',structure-name
                           ,@(common-lisp:mapcar
                              (lambda (slot-desc)
                                (let ((slot-name
                                        (if (common-lisp:consp slot-desc)
                                            (common-lisp:first slot-desc)
                                            slot-desc)))
                                  slot-name))
                              slot-descriptions)))
       ,@(let ((i -1))
           (common-lisp:mapcar
            (lambda (slot-desc)
              (let* ((slot-name
                       (if (common-lisp:consp slot-desc)
                           (common-lisp:first slot-desc)
                           slot-desc))
                     (accessor (common-lisp:intern (common-lisp:format nil #"~A~A" conc-name slot-name))))
                (common-lisp:incf i)
                (common-lisp:destructuring-bind
                      (&key common-lisp:type read-only)
                    (if (common-lisp:consp slot-desc)
                        (common-lisp:cddr slot-desc)
                        nil)
                  (declare (ignore common-lisp:type))
                  `(progn
                     (defun ,accessor (common-lisp:structure)
                       (common-lisp:unless (*:structure-p common-lisp:structure)
                         (type-error common-lisp:structure 'common-lisp:structure-object))
                       (*:%structure-ref common-lisp:structure ,i))
                     ,@(common-lisp:unless read-only
                         `((defun (common-lisp:setf ,accessor) (value common-lisp:structure)
                             (common-lisp:unless (*:structure-p common-lisp:structure)
                               (type-error common-lisp:structure 'common-lisp:structure-object))
                             (*:%structure-set common-lisp:structure ,i value))))))))
            slot-descriptions))
       (common-lisp:setf (common-lisp:get ',structure-name 'structure-printer)
                ,(if print-function
                     (let ((common-lisp:structure (common-lisp:gensym)) (common-lisp:stream (common-lisp:gensym)))
                       `(lambda (,common-lisp:structure ,common-lisp:stream)
                          (,print-function ,common-lisp:structure ,common-lisp:stream 0)))
                     `(lambda (common-lisp:structure common-lisp:stream)
                        (common-lisp:write-string #"#S(" common-lisp:stream)
                        (common-lisp:write-string ,(common-lisp:string structure-name) common-lisp:stream)
                        (common-lisp:write-string #" " common-lisp:stream)
                        ,@(let ((i -1))
                            (common-lisp:mapcar
                             (lambda (slot-desc)
                               `(progn
                                  ,(common-lisp:unless (common-lisp:= i -1) '(common-lisp:write-string #" " common-lisp:stream))
                                  (common-lisp:prin1
                                   ,(common-lisp:intern
                                     (common-lisp:string
                                      (if (common-lisp:consp slot-desc)
                                          (common-lisp:first slot-desc)
                                          slot-desc))
                                     :keyword)
                                   common-lisp:stream)
                                  (common-lisp:write-string #" " common-lisp:stream)
                                  (common-lisp:prin1 (*:%structure-ref common-lisp:structure ,(common-lisp:incf i))
                                            common-lisp:stream)))
                             slot-descriptions))
                        (common-lisp:write-string #")" common-lisp:stream))))
       ',structure-name)))

(defun structure-printer (structure)
  (get (*:%structure-name structure) 'structure-printer))
