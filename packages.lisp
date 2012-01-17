;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :cl-user)

(defpackage :trumpet
  (:use
   :cl
   :cl-ppcre
   :com.gigamonkeys.json
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.spam
   :com.gigamonkeys.utilities
   :comments
   :monkeylib-atom
   :monkeylib-html
   :toot ;; FIXME -- probably should be able to use WHISTLE without having to also use TOOT.
   :whistle
   )

  (:export
   :blog-handler
   :define-template
   :comment-db
   :render-page
   :index-page
   :category-page
   :article-page))