;;;;
;;;;	File:		install-hyperspec.lisp
;;;;	Contents:	Hyperspec installation utility for Corman Lisp.
;;;;	History:	4/9/02  RGC  Created.
;;;;
;;;;	Load this file to install the Hyperspec.
;;;;

(in-package :win32)

#! (:export t :library "KERNEL32")

/*
 * Standard Archive Format - Standard TAR - USTAR
 */

#define  RECORDSIZE  512
#define  NAMSIZ      100
#define  TUNMLEN      32
#define  TGNMLEN      32

typedef struct _header
{
	char    name[NAMSIZ];
    char    mode[8];
    char    uid[8];
    char    gid[8];
    char    size[12];
    char    mtime[12];
    char    chksum[8];
    char    linkflag;
    char    linkname[NAMSIZ];
    char    magic[8];
    char    uname[TUNMLEN];
    char    gname[TGNMLEN];
    char    devmajor[8];
    char    devminor[8];
} header;
!#

(defun tar-extract (path)
	(multiple-value-bind (address length)
		(ccl:map-file path)
		(if (null address)
			(error "Could not find file ~S" path))
		(unwind-protect
			(do ((pos 0)
				 (file-name nil)
				 (header nil)
				 (addr (ct:foreign-ptr-to-int address))
				 (file-length 0)
				 (file-path)
				 (root (make-pathname :device (pathname-device path) :directory (pathname-directory path))))
				((>= pos length))
				(setf header (ct:int-to-foreign-ptr (+ addr pos))) 
				(setf file-name 
					(ct:c-string-to-lisp-string (ct:cref header header name)))
				(if (or (null file-name) (= (length file-name) 0))
					(return))
				(setf file-path (merge-pathnames file-name root))
				(setf file-length 
					(parse-integer (ct:c-string-to-lisp-string (ct:cref header header size))
							:junk-allowed t
							:radix 8))
				(format t "~A~20T~D~%" file-name file-length) (force-output)
				(ensure-directories-exist file-path)
				(if (pathname-name file-path)
					(with-open-file (os file-path :direction :output :element-type 'unsigned-byte)
						(dotimes (i file-length)
							(write-byte (ct:cref (:unsigned-char *) address (+ pos RECORDSIZE i)) os))))
				(incf pos (* (+ 1 (truncate (+ file-length (- RECORDSIZE 1)) RECORDSIZE)) RECORDSIZE))))
			(ccl:unmap-file address)))

(defun install-hyperspec ()
  (let ((gzpath (namestring (truename (merge-pathnames "HyperSpec-7-0.tar.gz"))))
		(tarpath (namestring (truename (merge-pathnames "HyperSpec-7-0.tar"))))
		(hyperspec-path (namestring (truename (merge-pathnames "hyperspec/")))))
	
	;; unless the Hyperspec is already installed at this location, extract
	;; it to that location
	(unless (probe-file (merge-pathnames "Front\\Contents.htm" hyperspec-path))
	  (format t "Unzipping ~A...~%" gzpath)
	  (force-output)
	  (ccl:uncompress-file gzpath tarpath)
	  (format t "Extracting files from ~A...~%" tarpath)
	  (tar-extract tarpath)
	  (delete-file tarpath))
	(setf *hyperspec-local-path* hyperspec-path)))

(install-hyperspec)



        


		
