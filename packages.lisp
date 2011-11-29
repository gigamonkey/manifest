(in-package :cl-user)

(defpackage :manifest
  (:use :cl :toot :split-sequence :closer-mop :monkeylib-html :monkeylib-text-output)
  (:shadowing-import-from :closer-mop
                          :defmethod
                          :defgeneric
                          :standard-generic-function)

  (:export :start
           :stop))
