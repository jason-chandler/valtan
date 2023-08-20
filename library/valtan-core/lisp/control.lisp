#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

#+valtan
(*:defmacro* lambda (lambda-list &rest body)
  `(function (lambda ,lambda-list ,@body)))

(*:defmacro* return (&optional value)
  `(return-from nil ,value))

(*:defmacro* cond (&rest clauses)
  (if (common-lisp:null clauses)
      nil
      (let ((clause (common-lisp:first clauses))
            (g-test (common-lisp:gensym)))
        `(let ((,g-test ,(common-lisp:first clause)))
           (if ,g-test
               ,(if (common-lisp:null (common-lisp:rest clause))
                    g-test
                    `(progn ,@(common-lisp:rest clause)))
               (cond ,@(common-lisp:rest clauses)))))))

(*:defmacro* or (&rest forms)
  (if (common-lisp:null forms)
      nil
      (let ((value (common-lisp:gensym)))
        `(let ((,value ,(common-lisp:first forms)))
           (if ,value
               ,value
               (or ,@(common-lisp:rest forms)))))))

(*:defmacro* and (&rest forms)
  (cond ((common-lisp:null forms))
        ((common-lisp:null (common-lisp:rest forms)) (common-lisp:first forms))
        (t `(if ,(common-lisp:first forms)
                (and ,@(common-lisp:rest forms))
                nil))))

(*:defmacro* when (test &rest forms)
  `(if ,test
       (progn ,@forms)))

(*:defmacro* unless (test &rest forms)
  `(if ,test
       nil
       (progn ,@forms)))

(defun not (x)
  (if x nil t))

(*:defmacro* do* (varlist endlist &rest body)
  (let ((g-start (common-lisp:gensym))
        (body (compiler::parse-body body nil)))
    (common-lisp:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      (let ((varlist
              (common-lisp:mapcar (lambda (var-spec)
                           (if (common-lisp:symbolp var-spec)
                               `(,var-spec nil)
                               var-spec))
                         varlist)))
        `(block nil
           (let* ,(common-lisp:mapcar (lambda (var-spec)
                               `(,(common-lisp:first var-spec)
                                 ,(common-lisp:second var-spec)))
                             varlist)
             (declare ,@declares)
             (tagbody
               ,g-start
               (if ,(common-lisp:first endlist)
                   (return (progn ,@(common-lisp:rest endlist)))
                   (progn
                     (tagbody ,@body)
                     (setq ,@(common-lisp:mapcan (common-lisp:lambda (var-spec)
                                          (if (common-lisp:cddr var-spec)
                                              `(,(common-lisp:first var-spec)
                                                ,(common-lisp:third var-spec))))
                                        varlist))
                     (go ,g-start))))))))))

(*:defmacro* psetq (&rest pairs)
  (when (common-lisp:oddp (common-lisp:length pairs))
    (common-lisp:error "Odd number of args to PSETQ."))
  (let ((gvars '())
        (vars '())
        (values '()))
    (common-lisp:do*
        ((pairs* pairs (common-lisp:cddr pairs*)))
        ((common-lisp:null pairs*))
      (let ((var (common-lisp:first pairs*))
            (value (common-lisp:second pairs*)))
        (setq vars (common-lisp:cons var vars))
        (setq values (common-lisp:cons value values))
        (setq gvars (common-lisp:cons (common-lisp:gensym) gvars))))
    (setq gvars (common-lisp:nreverse gvars))
    (setq vars (common-lisp:nreverse vars))
    (setq values (common-lisp:nreverse values))
    `(let ,(common-lisp:mapcar #'common-lisp:list gvars values)
       ,@(common-lisp:mapcar (common-lisp:lambda (var gvar) `(setq ,var ,gvar))
                    vars gvars)
       nil)))

(*:defmacro* do (varlist endlist &rest body)
  (let ((g-start (common-lisp:gensym)))
    (common-lisp:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      (let ((varlist
              (common-lisp:mapcar (lambda (var-spec)
                           (if (common-lisp:symbolp var-spec)
                               `(,var-spec nil)
                               var-spec))
                         varlist)))
        `(block nil
           (let ,(common-lisp:mapcar (lambda (var-spec)
                              `(,(common-lisp:first var-spec)
                                ,(common-lisp:second var-spec)))
                            varlist)
             (declare ,@declares)
             (tagbody
               ,g-start
               (if ,(common-lisp:first endlist)
                   (return (progn ,@(common-lisp:rest endlist)))
                   (progn
                     (tagbody ,@body)
                     (psetq ,@(common-lisp:mapcan (common-lisp:lambda (var-spec)
                                           (if (common-lisp:cddr var-spec)
                                               `(,(common-lisp:first var-spec)
                                                 ,(common-lisp:third var-spec))))
                                         varlist))
                     (go ,g-start))))))))))

(*:defmacro* dotimes (var-form &rest body)
  (let ((var (common-lisp:first var-form))
        (expr (common-lisp:second var-form))
        (result (common-lisp:third var-form))
        (g-expr (common-lisp:gensym)))
    (common-lisp:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      `(let ((,g-expr ,expr))
         (do ((,var 0 (+ ,var 1)))
             ((>= ,var ,g-expr) ,result)
           (declare ,@declares)
           ,@body)))))

(*:defmacro* dolist (var-form &rest body)
  (let* ((var (common-lisp:first var-form))
         (expr (common-lisp:second var-form))
         (result (common-lisp:third var-form))
         (g-list (common-lisp:gensym))
         (g-start (common-lisp:gensym)))
    (common-lisp:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      `(block nil
         (let ((,g-list ,expr))
           (tagbody
             ,g-start
             (unless (endp ,g-list)
               (let ((,var (car ,g-list)))
                 (declare ,@declares)
                 (setq ,g-list (cdr ,g-list))
                 (tagbody ,@body))
               (go ,g-start))))
         (let ((,var nil))
           (declare (ignorable ,var))
           ,result)))))

(*:defmacro* case (keyform &rest cases)
  (let ((var (common-lisp:gensym)))
    `(let ((,var ,keyform))
       (cond ,@(common-lisp:mapcar (common-lisp:lambda (c)
                            (common-lisp:cond ((common-lisp:eq 'otherwise (common-lisp:car c))
                                      `(t ,@(common-lisp:cdr c)))
                                     ((common-lisp:listp (common-lisp:car c))
                                      `((member ,var ',(common-lisp:car c))
                                        ,@(common-lisp:cdr c)))
                                     (t
                                      `((eql ,var ',(common-lisp:car c))
                                        ,@(common-lisp:cdr c)))))
                          cases)))))

(*:defmacro* ecase (keyform &rest cases)
  (let ((var (common-lisp:gensym)))
    `(let ((,var ,keyform))
       (cond ,@(common-lisp:mapcar (common-lisp:lambda (c)
                            (common-lisp:cond ((common-lisp:listp (common-lisp:car c))
                                      `((member ,var ',(common-lisp:car c))
                                        ,@(common-lisp:cdr c)))
                                     (t
                                      `((eql ,var ',(common-lisp:car c))
                                        ,@(common-lisp:cdr c)))))
                          cases)
             (t (error "~S fell through ECASE expression. Wanted one of ~S."
                       ,var
                       ',(common-lisp:mapcan (common-lisp:lambda (c)
                                      (common-lisp:copy-list
                                       (if (common-lisp:listp (common-lisp:car c))
                                           (common-lisp:car c)
                                           (common-lisp:list (common-lisp:car c)))))
                                    cases)))))))

(*:defmacro* multiple-value-bind (vars value-form &rest body)
  (let ((rest (common-lisp:gensym)))
    `(multiple-value-call (lambda (&optional ,@vars &rest ,rest)
                            (declare (ignore ,rest))
                            ,@body)
       ,value-form)))

(defun ensure-function (value)
  (cond ((common-lisp:functionp value)
         value)
        ((common-lisp:symbolp value)
         (common-lisp:symbol-function value))
        (t
         (type-error value 'function))))

(*:defmacro* multiple-value-call (function arg &rest args)
  (if (common-lisp:null args)
      `(*:multiple-value-call (ensure-function ,function)
         ,(if (common-lisp:atom arg)
              `(values ,arg)
              arg))
      `(*:multiple-value-call (ensure-function ,function)
         ,arg
         ,@(if (common-lisp:atom (common-lisp:car (common-lisp:last args)))
               `(,@(common-lisp:butlast args) (values ,@(common-lisp:last args)))
               args))))

(*:defmacro* multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

(*:defmacro* multiple-value-prog1 (first-form &rest forms)
  (let ((g-values (common-lisp:gensym)))
    `(let ((,g-values (multiple-value-list ,first-form)))
       ,@forms
       (apply #'values ,g-values))))

(*:defmacro* multiple-value-setq (vars form)
  (if (null vars)
      `(values ,form)
      (let ((gsyms
              (mapcar (lambda (var)
                        (declare (ignore var))
                        (gensym))
                      vars)))
        `(multiple-value-bind ,gsyms ,form
           (setq ,@(mapcan #'list
                           vars
                           gsyms))
           ,(first gsyms)))))

(defun eql (x y)
  (cond ((and (characterp x) (characterp y))
         (char= x y))
        (t
         (eq x y))))

(defun equal (x y)
  (cond ((and (consp x)
              (consp y))
         (and (equal (car x) (car y))
              (equal (cdr x) (cdr y))))
        ((and (common-lisp:stringp x)
              (common-lisp:stringp y))
         (string= x y))
        ((and (simple-bit-vector-p x)
              (simple-bit-vector-p y))
         (and (= (length x) (length y))
              (dotimes (i (length x) t)
                (unless (eql (aref x i)
                             (aref y i))
                  (return nil)))))
        (t
         (eql x y))))

(defun equalp (x y)
  (cond ((and (characterp x)
              (characterp y))
         (char-equal x y))
        ((and (numberp x)
              (numberp y))
         (= x y))
        ((and (consp x)
              (consp y))
         (and (equalp (car x) (car y))
              (equalp (cdr x) (cdr y))))
        ((and (common-lisp:stringp x)
              (common-lisp:stringp y))
         (string-equal x y))
        ((and (arrayp x)
              (arrayp y))
         (array-equalp x y))
        ((and (*:structure-p x)
              (*:structure-p y))
         (and (eq (*:%structure-name x)
                  (*:%structure-name y))
              (dotimes (i (*:%structure-slot-count x) t)
                (unless (equalp (*:%structure-ref x i)
                                (*:%structure-ref y i))
                  (return nil)))))
        ((and (hash-table-p x)
              (hash-table-p y))
         (error "trap"))
        (t
         (eql x y))))

(*:defmacro* prog1 (result &rest body)
  (let ((tmp (common-lisp:gensym)))
    `(let ((,tmp ,result))
       ,@body
       ,tmp)))

(defun identity (x) x)

(defun complement (function)
  (lambda (&rest args)
    (not (apply function args))))

(defun constantly (value)
  (lambda (&rest args)
    (declare (ignore args))
    value))
