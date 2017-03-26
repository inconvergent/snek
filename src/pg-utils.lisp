; below code is from from On Lisp by Paul Graham.
; http://ep.yimg.com/ty/cdn/paulgraham/onlisp.lisp

; This code is copyright 1993 by Paul Graham, but anyone who wants
; to use the code in any nonprofit activity, or distribute free
; verbatim copies (including this notice), is encouraged to do so.


(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))


(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))


(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))


(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
                 syms)
     ,@body))


(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))


(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f) ,gs)))
                     fields)
         ,@body))))

