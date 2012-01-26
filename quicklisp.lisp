(in-package :manifest)

(defun system-descriptions ()
  "Temporary hack until Xach provides a way to get this programatically from Quicklisp."
  (let ((h (make-hash-table :test #'equal)))
    (with-open-file (in "descriptions.txt")
      (loop for line = (read-line in nil nil)
         while line do
           (let* ((pos (search " - " line))
                  (name (subseq line 0 pos))
                  (description (subseq line (+ pos 3))))
             (setf (gethash name h) description))))
    h))


(defun quicklisp-page (request)
  (let ((descriptions (system-descriptions)))
    (with-response-body (s request)
      (with-html-output (s)
        (:html
          (:head
           (:title "Manifest: Quicklisp browser")
           (:link :rel "stylesheet" :type "text/css" :href "manifest.css"))
          (:body
           (:h1 "Dists")
           (loop for dist in (ql-dist:all-dists) do
                (html
                  (:h2 (:print (ql-dist:name dist)))
                  (:table
                   (:thead
                    (:th "System")
                    (:th "Description")
                    (:th "Installed?"))
                   (:tbody
                    (loop for system in (ql-dist:provided-systems dist)
                       for name = (ql-dist:name system)
                       for installedp = (ql-dist:installedp system)
                       for (description descriptionp) = (multiple-value-list
                                                         (gethash name descriptions "NO DESCRIPTION!"))
                       do
                       (html
                         (:tr :class (:format "~:[not-documented~;~]" descriptionp)
                              (:td
                               (if (and installedp (find-package (case-invert-name name)))
                                   (html (:a :href (:format "/package/~a" name) name))
                                   (html name)))
                              (:td :class "docs"
                                   description)
                              (:td
                               (if installedp
                                   (html "✓")
                                   (html (:a :href (:format "/quicklisp/install/~a" name) "Install")))))))))))))))))

#+(or)(defun foo ()
  (let ((dist (ql-dist:find-dist "quicklisp"))
        (systems (make-hash-table))
        (in (make-hash-table))
        (out (make-hash-table))
        (all (make-hash-table)))
    (flet ((record-dependency (system dep)
             (setf (gethash system all) t)
             (setf (gethash dep all) t)
             (incf (gethash system out 0))
             (incf (gethash dep in 0))))
      (loop for release in (ql-dist:provided-releases dist) do
         (loop for system in (ql-dist:provided-systems release) do
              (loop for dep in (ql-dist:required-systems system) do
                   (format t "~&~a requires ~a" (ql-dist:name system) dep)))))))

(defun quicklisp-install (request)
  (destructuring-bind (quicklisp install system &rest rest)
      (split-sequence #\/ (subseq (request-path request) 1))
    (declare (ignore rest))
    (assert (string= quicklisp "quicklisp"))
    (assert (string= install "install"))

    (ql:quickload system)
    (redirect request "/quicklisp")))
