(in-package :manifest)

(defvar *manifest-server* nil)

(defparameter *categories* '(:function :generic-function :accessor :variable :class :condition :constant))

(defun start (&key (port 0))
  "Start the manifest server and return the URL to browse. By default
picks a random unused port or you can specify a port with the :port
keyword argument."
  (cond
    ((and *manifest-server* (not (toot::shutdown-p *manifest-server*)))
     (warn "Manifest server already running."))
    (*manifest-server*
     (start-acceptor *manifest-server*))
    (t
     (setf *manifest-server* (start-server :handler (make-handler) :port port))))
  (format nil "http://localhost:~d/" (port *manifest-server*)))

(defun stop (&optional (server *manifest-server*))
  "Stop the manifest server, defaulting to *manifest-server*."
  (stop-acceptor server))

(defun make-handler (&optional (root-dir (asdf:system-relative-pathname :manifest nil)))
  (let ((static-files (make-instance 'static-file-handler :root root-dir)))
    (lambda (request)
      (let ((result (manifest request)))
        (case result
          (not-handled (handle-request static-files request))
          (t result))))))

(defun manifest (request)
  (cond
    ((string= (uri-path (request-uri request)) "/") (index-page request))
    (t (package-page request))))

(defun package-page (request)
  (destructuring-bind (package-name &rest rest)
      (split-sequence #\/ (subseq (uri-path (request-uri request)) 1))
    (declare (ignore rest))

    (let ((package (find-package (string-upcase package-name))))
      (cond
        (package
          (let ((s (send-headers request)))
            (format s "<html><head><title>Package: ~a</title><link rel='stylesheet' type='text/css' href='manifest.css'></head>" (package-name package))
            (format s "<body><h1>Package: ~a</h1>" (package-name package))

            (let ((readme (readme-text package-name)))
              (when readme
                (format s "~a" readme)))

            (loop for what in *categories* do
                 (format s "~&<h2>~:(~a~)</h2>~&<table>" (pluralization what))
                 (loop for sym in (names package what) do
                      (format s "~&<tr><td class='symbol'>~(~a~)</td><td class='docs'>~a</td></tr>" sym (docs-for sym what)))
                 (format s "~&</table>"))


            (format s "~&</dl></body></html>")))
        (t 'not-handled)))))

(defun index-page (request)
  (let ((s (send-headers request)))
    (format s "<html><head><title>Manifest: all packages</title><link rel='stylesheet' type='text/css' href='manifest.css'></head>")
    (format s "<body><h1>All Packages</h1>")
    (format s "~&<ul>")
    (loop for pkg in (sort (mapcar #'package-name (list-all-packages)) #'string<) do
         (format s "<li><a href='~a'>~a</a></li>" (string-downcase pkg) pkg))
    (format s "~&</ul>")
    (format s "~&</body></html>")))

(defun readme-text (package-name)
  (let ((dir (ignore-errors (asdf:system-relative-pathname package-name nil))))
    (when dir
      (with-open-file (in (merge-pathnames "README" dir) :if-does-not-exist nil)
        (when in
          (with-output-to-string (s)
            (loop for line = (read-line in nil nil)
               while line do (write-line line s))))))))

(defun names (package what)
  (sort (loop for sym being the external-symbols of package when (is sym what) collect sym) #'string<))

(defgeneric is (symbol what))
(defgeneric docs-for (symbol what))
(defgeneric pluralization (what))

(defmethod pluralization (what) (format nil "~as" what))

(defmacro define-category (name (symbol what) &body body)
  (let ((is-test (cdr (assoc :is body)))
        (get-docs (cdr (assoc :docs body)))
        (pluralization (cdr (assoc :pluralization body))))
    `(progn
       (defmethod is (,symbol (,what (eql ',name))) ,@is-test)
       (defmethod docs-for (,symbol (,what (eql ',name))) ,@get-docs)
       ,@(when pluralization
               `((defmethod pluralization ((,what (eql ',name)))
                   ,@pluralization))))))

(define-category :function (symbol what)
  (:is (and (fboundp symbol) (not (typep (symbol-function symbol) 'generic-function))))
  (:docs (documentation symbol 'function)))

(define-category :generic-function (symbol what)
  (:is (and (fboundp symbol)
            (typep (symbol-function symbol) 'generic-function)
            (not (is symbol :accessor))))
  (:docs (documentation symbol 'function)))

(define-category :class (symbol what)
  (:is (and (find-class symbol nil) (not (is symbol :condition))))
  (:docs (documentation (find-class symbol) t))
  (:pluralization (format nil "~aes" what)))

(define-category :condition (symbol what)
  (:is (and (find-class symbol nil) (subtypep (find-class symbol nil) 'condition)))
  (:docs (documentation (find-class symbol) t)))

(define-category :variable (symbol what)
  (:is (and (boundp symbol) (not (constantp symbol))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (boundp symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(define-category :accessor (symbol what)
  (:is (and (fboundp symbol)
            (fboundp `(setf ,symbol))
            (typep (fdefinition symbol) 'generic-function)
            (typep (fdefinition `(setf ,symbol)) 'generic-function)))
  (:docs (documentation symbol 'function)))




