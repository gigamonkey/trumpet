;;; Copyright (c) 2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(defsystem :horn
  :description "A blog server to run on top of Whistle."
  :depends-on (:whistle
               :com.gigamonkeys.utilities
               :monkeylib-html
               :monkeylib-atom)
  :components ((:file "packages")
               (:file "horn" :depends-on ("packages"))))
