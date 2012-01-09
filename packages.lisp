;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :cl-user)

(defpackage :horn
  (:use
   :cl
   :whistle
   :toot ;; FIXME -- probably should be able to use WHISTLE without having to also use TOOT.
   :comments
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.utilities
   :cl-ppcre
   :monkeylib-atom
   :monkeylib-html
   :com.gigamonkeys.markup
   :com.gigamonkeys.markup.html
   :com.gigamonkeys.markup.html.handy-tags
   :com.gigamonkeys.utilities)

  (:import-from :com.gigamonkeys.markup.html.handy-tags :amazon-link :amazon-image-bug)
  )