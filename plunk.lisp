;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :trumpet)

(defmacro define-template (name (&rest parameters) &body body)
  (multiple-value-bind (required optional) (parse-template-parameters parameters)
    (let ((docstring ())
          (declarations ()))
      (when (stringp (first body))
        (push (pop body) docstring))
      (loop while (and (consp (first body)) (eql (caar body) 'declare))
           do (push (pop body) declarations))
      `(defun ,name (&key
                     ,@(loop for (p supplied-p) in required collect `(,p nil ,supplied-p))
                     ,@optional)
         ,@docstring
         ,@(nreverse declarations)
         ,@(loop for (p supplied-p) in required collect
                `(unless ,supplied-p (error "Template ~a needs a value for ~a" ',name ',p)))
         (html ,@body)))))

(defun parse-template-parameters (parameters)
  (let ((start-optional (member '&optional parameters)))
    (values
     (loop for p in (ldiff parameters start-optional) collect (list p (gensym)))
     (rest start-optional))))