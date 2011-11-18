(in-package :manifest)

(defvar *manifest-server* nil)

(defparameter *categories* '(:function :generic-function :slot-accessor :variable :class :condition :constant))

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
     (setf *manifest-server*
           (start-server :handler (make-handler) :port port :access-logger nil :message-logger nil))))
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

    (let ((package (find-package (string-upcase package-name)))
          (some-docs-p nil))
      (cond
        (package
          (let ((s (send-headers request)))
            (format s "<html><head><title>Package: ~a</title><link rel='stylesheet' type='text/css' href='manifest.css'></head>" (package-name package))
            (format s "<body><h1>Package: ~a</h1>" (package-name package))

            (let ((readme (readme-text package-name)))
              (when readme
                (setf some-docs-p t)
                (format s "<pre>~a</pre>" (escape-for-html readme))))

            (loop for what in *categories*
               for names = (names package what)
               when names do
                 (setf some-docs-p t)
                 (format s "~&<h2>~:(~a~)</h2>~&<table>" (pluralization what))
                 (loop for sym in names do
                      (format s "~&<tr><td class='symbol'>~(~a~)</td>
<td class='docs'>~a</td></tr>"
                              (escape-for-html (princ-to-string sym))
                              (escape-for-html (or (docs-for sym what) "NO DOCS!"))))
                 (format s "~&</table>"))

            (unless some-docs-p
              (format s "<p>Uh oh! No docs at all.</p>"))


            (format s "~&</body></html>")))
        (t 'not-handled)))))

(defun index-page (request)
  (let ((s (send-headers request)))
    (format s "<html><head><title>Manifest: all packages</title><link rel='stylesheet' type='text/css' href='manifest.css'></head>")
    (format s "<body><h1>All Packages</h1>")
    (format s "~&<ul>")
    (loop for pkg in (sort (mapcar #'package-name (public-packages)) #'string<) do
         (format s "<li><a href='~a'>~a</a></li>" (string-downcase pkg) pkg))
    (format s "~&</ul>")
    (format s "~&</body></html>")))

(defun public-packages ()
  (loop for p in (list-all-packages)
     when (and (has-exported-symbols-p p) (not (eql p (find-package :keyword))))
     collect p))

(defun has-exported-symbols-p (package)
  (do-external-symbols (sym package)
    (declare (ignore sym))
    (return-from has-exported-symbols-p t))
  nil)

(defun needs-documentation (package)
  (loop for what in *categories*
     for names = (names package what)
     when names nconc
       (loop for sym in names unless (docs-for sym what) collect (list sym what))))

(defun documentation-templates (package)
  (loop for (name what) in (needs-documentation package)
       collect
       (ecase what
         (:slot-accessor
          `(setf (documentation #',name t) "WRITE ME!"))
         (:class
          `(setf (documentation (find-class ',name) t) "WRITE ME!")))))



(defun readme-text (package-name)
  (let ((dir (ignore-errors (asdf:system-relative-pathname package-name nil))))
    (when dir
      (with-open-file (in (merge-pathnames "README" dir) :if-does-not-exist nil)
        (when in
          (with-output-to-string (s)
            (loop for line = (read-line in nil nil)
               while line do (write-line line s))))))))

(defun names (package what)
  (sort
   (loop for sym being the external-symbols of package
      when (is sym what) collect sym
      when (is `(setf ,sym) what) collect `(setf ,sym))
   #'name<))

(defun name< (n1 n2)
  (cond
    ((and (symbolp n1) (symbolp n2))
     (string< n1 n2))
    ((and (symbolp n1) (listp n2))
     (cond
       ((string< n1 (second n2)) t)
       ((string< (second n2) n1) nil)
       (t t)))
    ((and (listp n1) (symbolp n2))
     (cond
       ((string< (second n1) n2) t)
       ((string< n2 (second n1)) nil)
       (t nil)))
    ((and (listp n1) (listp n2))
     (string< (second n1) (second n2)))))


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

(defun function-p (name)
  (ignore-errors (fdefinition name)))

(defun generic-function-p (name)
  (and (function-p name)
       (typep (fdefinition name) 'generic-function)))

(defun variable-p (name)
  (ignore-errors (boundp name)))

(defun automatic-p (docstring)
  (member docstring '("automatically generated reader method" "automatically generated writer method") :test #'string-equal))

(defun gf-docs (name)
  (let ((simple (documentation (fdefinition name) t))
        (from-setf (and (consp name) (documentation (fdefinition (second name)) t))))

    (or
     (and simple (not (automatic-p simple)) (format nil "The ~a" simple))
     (and from-setf (not (automatic-p from-setf)) (format nil "Set the ~a" from-setf))
     (first (remove-if #'automatic-p (remove nil (mapcar
                         (lambda (m) (documentation m t))
                         (generic-function-methods (fdefinition name)))))))))


(define-category :function (symbol what)
  (:is (and (function-p symbol)
            (not (or (is symbol :generic-function) (is symbol :slot-accessor)))))
  (:docs (documentation symbol 'function)))

(define-category :generic-function (symbol what)
  (:is (and (generic-function-p symbol)
            (not (is symbol :slot-accessor))))
  (:docs (documentation symbol 'function)))

(define-category :class (symbol what)
  (:is (and (find-class symbol nil) (not (is symbol :condition))))
  (:docs (documentation (find-class symbol) t))
  (:pluralization (format nil "~aes" what)))

(define-category :condition (symbol what)
  (:is (and (find-class symbol nil) (subtypep (find-class symbol nil) 'condition)))
  (:docs (documentation (find-class symbol) t)))

(define-category :variable (symbol what)
  (:is (and (variable-p symbol) (not (is symbol :constant))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (variable-p symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(define-category :slot-accessor (symbol what)
  (:is (and (generic-function-p symbol)
            (some (lambda (m)
                    (or (eql (class-of m) (find-class 'standard-reader-method))
                        (eql (class-of m) (find-class 'standard-writer-method))))
                  (generic-function-methods (fdefinition symbol)))))
  (:docs (gf-docs symbol)))


