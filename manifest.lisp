(in-package :manifest)

(defun start (&key (port 0))
  "Start the manifest server and return the URL to browse. By default
picks a random unused port or you can specify a port with the :port
keyword argument."
  (let ((acceptor (start-server :handler (make-handler (asdf:system-relative-pathname :manifest nil)) :port port)))
    (format nil "http://localhost:~d/" (port acceptor))))

(defun make-handler (root-dir)
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

            (loop for what in '(:class :function :generic-function :accessor :variable :constant) do
                 (format s "~&<h2>~:(~a~a~)</h2>~&<table>" what (pluralization what))
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
  (let ((dir (asdf:system-relative-pathname package-name nil)))
    (when dir
      (with-open-file (in (merge-pathnames "README" dir))
        (with-output-to-string (s)
          (loop for line = (read-line in nil nil)
             while line do (write-line line s)))))))

(defun names (package what)
  (sort (loop for sym being the external-symbols of package when (is sym what) collect sym) #'string<))

(defgeneric is (symbol what))
(defgeneric docs-for (symbol what))
(defgeneric pluralization (what))

(defmethod pluralization (what) "s")

(defmethod pluralization ((what (eql :class))) "es")


(defmethod is (sym (what (eql :function)))
  (and (fboundp sym) (not (typep (symbol-function sym) 'generic-function))))

(defmethod is (sym (what (eql :generic-function)))
  (and (fboundp sym) (typep (symbol-function sym) 'generic-function)
       (not (is sym :accessor))))

(defmethod is (sym (what (eql :class)))
  (find-class sym nil))

(defmethod is (sym (what (eql :variable)))
  (and (boundp sym) (not (constantp sym))))

(defmethod is (sym (what (eql :constant)))
  (and (boundp sym) (constantp sym)))

(defmethod is (sym (what (eql :accessor)))
  (and (fboundp sym) (fboundp `(setf ,sym)) (typep (symbol-function sym) 'generic-function)))


(defmethod docs-for (symbol (what (eql :function)))
  (documentation symbol 'function))

(defmethod docs-for (symbol (what (eql :generic-function)))
  (documentation symbol 'function))

(defmethod docs-for (symbol (what (eql :accessor)))
  (documentation symbol 'function))

(defmethod docs-for (symbol (what (eql :class)))
  (documentation (find-class symbol) t))

(defmethod docs-for (symbol (what (eql :variable)))
  (documentation symbol 'variable))

(defmethod docs-for (symbol (what (eql :constant)))
  (documentation symbol 'variable))

