;;;-*- Mode: Lisp; Package: (USE-MODULE) -*-

;; Instead of a defsystem.

(in-package :cl-user)

(defpackage "USE-MODULE"
  (:nicknames "UM")
  (:export "USE-MODULE"))

(in-package :use-module)

#-lispworks
(defun file-newer-p (file1 file2)
  "Returns true if file1 is newer than file 2 or file2 doesn't exist."
  (unless (probe-file file1) (error "file1 doesn't exist" file1))
  (if (probe-file file2)
    (> (file-write-date file1)
       (file-write-date file2))
    t))

#-lispworks
(defun compile-file-if-needed (path-name)
  (let ((source-file (concatenate 'string path-name ".lisp"))
        (fasl-file (concatenate 'string path-name ".pfsl")))
    (when (file-newer-p source-file fasl-file)
      (compile-file source-file))))

(defun use-module (module-name path-name)
  (compile-file-if-needed path-name)
  (require module-name path-name))
