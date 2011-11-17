(in-package :cl-user)

(defpackage :lispdoc
  (:use :cl :toot :puri :split-sequence :closer-mop)
  (:shadowing-import-from :closer-mop
                          :defmethod
                          :defgeneric
                          :standard-generic-function))

