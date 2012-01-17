;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(defsystem :trumpet
  :description "A blog server to run on top of Whistle."
  :depends-on (:whistle
               :comments
               :com.gigamonkeys.json
               :com.gigamonkeys.utilities
               :cl-ppcre
               :monkeylib-html
               :monkeylib-atom)
  :components ((:file "packages")
               (:file "trumpet" :depends-on ("packages"))
               (:file "plunk" :depends-on ("packages"))
               (:file "templates" :depends-on ("packages" "plunk"))
               (:file "comments" :depends-on ("packages"))))
