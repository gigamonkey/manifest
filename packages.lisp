(in-package :cl-user)

(defpackage :manifest
  (:use :closer-common-lisp
        :monkeylib-html
        :monkeylib-text-output
        :toot
        :split-sequence
        :alexandria)
  (:export :start
           :stop))
