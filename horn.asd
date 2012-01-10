;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(defsystem :horn
  :description "A blog server to run on top of Whistle."
  :depends-on (:whistle
               :comments
               :com.gigamonkeys.json
               :com.gigamonkeys.utilities
               :com.gigamonkeys.markup
               :com.gigamonkeys.utilities
               :cl-ppcre
               :monkeylib-markup-html
               :monkeylib-html
               :monkeylib-atom)
  :components ((:file "packages")
               (:file "horn" :depends-on ("packages"))
               (:file "comments" :depends-on ("packages"))
               (:file "markup-to-html" :depends-on ("packages"))))
