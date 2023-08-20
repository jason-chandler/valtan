#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun !get-setf-expansion (place &optional environment)
    (flet ((gensyms (common-lisp:list)
             (common-lisp:mapcar (lambda (x) (declare (ignore x)) (common-lisp:gensym)) common-lisp:list)))
      (let ((setf-expander nil))
        (common-lisp:cond
         ((symbolp place)
          (let ((store (common-lisp:gensym)))
            (common-lisp:values nil nil (common-lisp:list store) `(setq ,place ,store) place)))
         ((common-lisp:and (common-lisp:consp place) (setq setf-expander (common-lisp:get (common-lisp:first place) 'setf-expander)))
          (common-lisp:cond
           ((symbolp setf-expander)
            (let ((vars (gensyms (common-lisp:rest place))) (store (common-lisp:gensym)))
              (common-lisp:values vars (common-lisp:rest place) (common-lisp:list store) `(,setf-expander ,@vars ,store)
                         `(,(common-lisp:first place) ,@vars))))
           ((common-lisp:consp setf-expander)
            (let ((vars (gensyms (common-lisp:rest place)))
                  (store (common-lisp:gensym))
                  (fn
                   (common-lisp:eval
                    `(lambda ,(common-lisp:first setf-expander) (lambda ,@(common-lisp:rest setf-expander))))))
              (common-lisp:values vars (common-lisp:rest place) (common-lisp:list store) (common-lisp:funcall (common-lisp:apply fn vars) store)
                         `(,(common-lisp:first place) ,@vars))))
           ((functionp setf-expander) (common-lisp:funcall setf-expander (common-lisp:rest place)))))
         (t
          (common-lisp:multiple-value-bind (expansion expanded-p)
              (common-lisp:macroexpand-1 place environment)
            (if expanded-p
                (!get-setf-expansion expansion environment)
                (let ((newvar (common-lisp:gensym))
                      (vars (gensyms (common-lisp:cdr expansion)))
                      (vals (common-lisp:cdr expansion)))
                  (common-lisp:values vars vals (common-lisp:list newvar)
                             `(common-lisp:funcall (common-lisp:fdefinition '(common-lisp:setf ,(common-lisp:car expansion))) ,newvar
                                          ,@vars)
                             `(,(common-lisp:car expansion) ,@vars)))))))))))

(defmacro setf (&rest pairs)
  (labels ((setf-expand-1 (place value)
             (common-lisp:multiple-value-bind (vars forms store common-lisp:set access)
                 (!get-setf-expansion place)
               (declare (ignore access))
               `(let* (,@(common-lisp:mapcar #'common-lisp:list (common-lisp:append vars store)
                                    (common-lisp:append forms (common-lisp:list value))))
                  ,common-lisp:set)))
           (setf-expand (pairs)
             (common-lisp:cond ((common-lisp:endp pairs) nil)
                      ((common-lisp:endp (common-lisp:cdr pairs)) (common-lisp:error "Odd number of args to SETF."))
                      (t
                       (common-lisp:cons (setf-expand-1 (common-lisp:first pairs) (common-lisp:second pairs))
                                (setf-expand (common-lisp:cddr pairs)))))))
    `(progn ,@(setf-expand pairs))))

(defmacro defsetf (access-fn &rest common-lisp:rest)
  ;; TODO: documentation文字列
  ;; TODO: restが単一のシンボルか関数ではないときの処理
  (common-lisp:check-type access-fn common-lisp:symbol)
  (common-lisp:cond
   ((common-lisp:and (common-lisp:first common-lisp:rest) (common-lisp:or (symbolp (common-lisp:first common-lisp:rest)) (functionp (common-lisp:first common-lisp:rest))))
    (common-lisp:setf (common-lisp:get access-fn 'setf-expander) (common-lisp:first common-lisp:rest))
    `(progn (*:put ',access-fn 'setf-expander ',(common-lisp:first common-lisp:rest)) ',access-fn))
   (t (common-lisp:setf (common-lisp:get access-fn 'setf-expander) common-lisp:rest)
    `(progn (*:put ',access-fn 'setf-expander ',common-lisp:rest) ',access-fn))))

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (common-lisp:unless (symbolp access-fn)
    (common-lisp:error "DEFINE-SETF-EXPANDER access-function name ~S is not a symbol." access-fn))
  (let ((g-rest (common-lisp:gensym)))
    (common-lisp:setf (common-lisp:get access-fn 'setf-expander)
               (common-lisp:eval `(lambda (,g-rest) (common-lisp:destructuring-bind ,lambda-list ,g-rest ,@body))))
    `(progn
      (*:put ',access-fn 'setf-expander
       (lambda (,g-rest) (common-lisp:destructuring-bind ,lambda-list ,g-rest ,@body)))
      ',access-fn)))

(defmacro define-modify-macro
    (name lambda-list function &optional (common-lisp:documentation nil documentation-p))
  (let ((update-form
          (common-lisp:do ((common-lisp:rest lambda-list (common-lisp:cdr common-lisp:rest))
                  (vars 'nil))
              ((common-lisp:null common-lisp:rest) `(common-lisp:list ',function access-form ,@(common-lisp:nreverse vars)))
            (common-lisp:cond ((eq '&optional (common-lisp:car common-lisp:rest)))
                     ((eq '&rest (common-lisp:car common-lisp:rest))
                      (common-lisp:return
                        `(common-lisp:list* ',function access-form ,@(common-lisp:nreverse vars) (common-lisp:cadr common-lisp:rest))))
                     ((symbolp (common-lisp:car common-lisp:rest)) (common-lisp:push (common-lisp:car common-lisp:rest) vars))
                     (t (common-lisp:push (common-lisp:caar common-lisp:rest) vars))))))
    (let ((reference (common-lisp:gensym)))
      `(defmacro ,name (,reference ,@lambda-list)
         ,@(common-lisp:when documentation-p `(,common-lisp:documentation))
         (common-lisp:multiple-value-bind (vars common-lisp:values stores set-form access-form)
             (!get-setf-expansion ,reference)
           (common-lisp:list 'let*
                    (common-lisp:mapcar #'common-lisp:list (common-lisp:append vars stores)
                               (common-lisp:append common-lisp:values (common-lisp:list ,update-form)))
                    set-form))))))

(define-modify-macro incf (&optional (n 1)) +)
(define-modify-macro decf (&optional (n 1)) -)
