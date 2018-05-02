;;;;
;;;;	File:		generate-version-wxi.lisp
;;;;	Contents:	The file which generate WiX include file with build version information.
;;;;	History:	02.05.2018  Artem Boldarev  Created.
;;;;
;;;;	This file is used during MSI installer generation.
;;;;

(defun write-version-wxi (pathname)
  (with-open-file (out pathname :direction :output :external-format :ascii
            :if-exists :supersede)
        (format out "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%<Include>~%")
        (multiple-value-bind (major minor patch-level)
            (cormanlisp-version)
            (format out "  <?define MajorVersion=\"~A\" ?>~%" major)
            (format out "  <?define MinorVersion=\"~A\" ?>~%" minor)
            (format out "  <?define BuildVersion=\"~A\" ?>~%" patch-level)
        (format out "</Include>~%"))))

(write-version-wxi "installer\\Version.wxi")