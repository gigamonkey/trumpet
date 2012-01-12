;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :trumpet)

(defparameter *tag-mappings*
  '((:math             . :i)

    ;; Complex remappings
    (:email            . mailto-link)
    (:url              . url-link)
    (:section          . section-marker)
    (:book             . amazon-link)
    (:amazon           . amazon-link)
    (:amazon-image-bug . amazon-image-bug)

    ;; Divs.
    (:by               . (:div :class "by"))
    (:signature        . (:div :class "signature"))
    (:news             . (:div :class "news"))
    (:intro            . (:div :class "intro"))

    ;; Spans.
    (:n                . (:span :class "name"))
    (:tk               . (:span :class "tk"))

    ;; Remove comment tags. In comment view they are dealt with before
    ;; retaging.
    (:comment          . nil)
    ))

(defun section-marker (sexp)
  (unless (eql (car sexp) :section)
    (error "Bad sexp for section marker: ~s" sexp))
  `((:div :class "hr") ((:hr :class "section"))))

(defun make-standard-rewriter (tag-mappings)
  (compose
   (lambda (x) (footnotes :note x))
   (make-retagger tag-mappings)
   'htmlize-links
   'add-amazon-image-bugs))

(defparameter *rewriter* (make-standard-rewriter *tag-mappings*))

(defun render-inline (file)
  "Render the contents of the Markup file inline."
  (emit-html `(:progn ,@(rest (parsed-markup file)))))

(defun render-body (file)
  (emit-html `(:progn ,@(rest (rest (parsed-markup file))))))

(defun parsed-markup (file)
  (rewrite-sexps (parse-markup file)))

(defun rewrite-sexps (sexps)
  (funcall *rewriter* sexps))

(defun parse-markup (file)
  (parse-file file :parse-links-p t :subdocument-tags '(:note :comment :signature :news)))
