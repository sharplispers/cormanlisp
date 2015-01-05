 ;;; -*- Mode: LISP; Package: LOGICAL-PATHNAME; Syntax: Common-lisp;  -*- 
;;; Tue Apr  9 19:17:01 1991 by Mark Kantrowitz <mkant@LION.OZ.CS.CMU.EDU>
;;; logical-pathnames.lisp

;;; ****************************************************************
;;; Logical Pathnames System ***************************************
;;; ****************************************************************
;;;
;;; Logical Pathnames provide a facility for referring to pathnames
;;; in a portable manner. Logical pathnames are mapped to physical
;;; pathnames by a set of implementation dependent and site-dependent
;;; rules. 
;;; 
;;; This system is a Common Lisp portable implementation of logical
;;; pathnames. It fulfills most of the X3J13 June 1989 specification
;;; for logical pathnames, as documented in Guy Steele's "Common Lisp:
;;; The Language" (2nd Edition), section 23.1.5 "Logical Pathnames".
;;;
;;; Written by Mark Kantrowitz, July 1990.
;;;
;;; Address: Carnegie Mellon University
;;;          School of Computer Science
;;;          Pittsburgh, PA 15213
;;;
;;; This code is in the public domain and is distributed without warranty
;;; of any kind. 
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 
;;;
;;; Please send bug reports, comments and suggestions to mkant@cs.cmu.edu. 
;;;
;;;
;;; Logical Pathnames are especially useful when coupled with a portable
;;; system construction tool, such as the Defsystem facility written
;;; by Mark Kantrowitz.
;;;

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; djc  = Daniel J. Clancy <clancy@cs.utexas.edu>
;;;
;;; 30-JUL-90  mk    Fixed logical pathnames for VAX-LISP (thanks to
;;;                  Paul Werkowski). In VAX-LISP simple strings are not
;;;                  sub-types of simple-vectors, so svref doesn't work
;;;                  on strings. These calls have been fixed to read
;;;                  #+:cmu svref #-:cmu aref.
;;; 15-NOV-90  mk    Changed convert-file-function to better handle optional 
;;;                  args. This should fix the problem of (ed) and (dribble)
;;;                  returning errors like "argument NIL must be a number"
;;;                  in parse-namestring. Note that some lisps seem to make
;;;                  a distinction between (funcall #'foo) and (foo) with
;;;                  respect to this error.
;;; 29-JAN-91  mk    Defined LISP:NTH-VALUE if not already present (it is
;;;                  a CLtL2 addition) and used it in LOAD-PHYSICAL-HOSTAB
;;;                  to avoid needing a GARBAGE variable in 
;;;                  (multiple-value-setq (garbage pos) ...) which we can
;;;                  not declare ignore and yet causes a compiler warning
;;;                  since we don't use it.
;;; 29-JAN-91  mk    lisp::file-name is particular to CMU Common Lisp
;;;                  and the #+:cmu's were accidentally left off.
;;; 29-JAN-91  mk    Added :explorer physical namestring output to 
;;;                  PHYSICAL-NAMESTRING.                
;;; 29-JAN-91  mk    Warns about name collisions between physical and logical
;;;                  host names.
;;; 30-JAN-91  mk    Added :logical-pathnames-mk to the *features* list.
;;; 25-FEB-91  mk    Added definition of LOAD-LOGICAL-PATHNAME-TRANSLATIONS.
;;; 09-APR-91  mk    Export pathname-host-type, append-logical-directories.
;;; 09-APR-91  mk    Translation rules now support :case :unchanged.
;;; 09-APR-91  djc   Fixed so that (logical-pathname "") returns a 
;;;                  logical-pathname structure.
;;; 12-JUL-91 mk    Changed wildcarding to bring it into closer conformance
;;;                 with reversible wildcard translation, and to make it
;;;                 easier for the user to customize the behavior.
;;;  8-JAN-92 mk    Upgraded packages to work in CLtL2 compatible lisps too.
;;; 20-MAY-92 mk    No longer define NTH-VALUE in the Lisp package to avoid
;;;                 name conflicts in Lucid, because the symbol is defined in
;;;                 the CLIM-LISP using package.
;;; 20-MAY-92 mk    Logical pathnames now have a default host specified by the
;;;                 variable *default-logical-pathname-host*.
;;; 07-SEP-00 cnd   Modified to work with Corman Lisp 1.41. Added :win32
;;;                 host type. The original package I used as a base was
;;;                 obtained from the CMU Artificial Intelligence Repository.

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; should really define *default-logical-pathname-defaults* to avoid
;;; problems with *default-pathname-defaults*.
;;;
;;; Support for Macintosh pathnames. Little tricky, since MACL uses a
;;; colon (:) as the delimiter.
;;;    
;;; support for tops-20/tenex, multics, its, ms-dos
;;; add host-type to pathnames
;;; merge-pathnames, with-open-file 
;;;
;;; Define generic pathname parsing/printing definition interface.
;;;
;;; Redefine with-open-file?
;;;
;;; Port to emacs-lisp for gnu-emacs?
;;;
;;; Logical pathnames needs to case both on the physical host type and on
;;; lisp type (e.g., for canonicalization). Fix this, and define lots of
;;; canonical types. Dependency on lisp type can probably be handled using
;;; #+ and #-. What about conflicts between canonicalization and the
;;; translations (e.g., "L" vs :lisp)?
;;;

;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    LOGICAL-PATHNAMES has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       Macintosh Allegro Common Lisp (1.3.2)
;;;       ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 3/30/90)
;;;       Lucid CL (Version 2.1 6-DEC-87)
;;;       Symbolics Common Lisp (8.0)
;;;       Lucid Common Lisp (3.0, 4.0)
;;;       VAXLisp (2.0, 3.1)
;;;
;;;    LOGICAL-PATHNAMES needs to be tested in the following lisps:
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp


;;; ********************************
;;; Documentation ******************
;;; ********************************
;;; 
;;; Logical pathnames allow large programs to be moved between sites
;;; by separating pathname reference from actual file location. The
;;; program will refer to files using logical pathnames. At each site,
;;; a user will specify a set of "translations" which map from the logical
;;; pathnames to the physical pathnames used on the device.
;;;
;;; Logical pathnames provide a uniform convention for filesystem access,
;;; with the following properties:
;;;  1.  Pathname Portability: The program specifies a pathname in
;;;      a conventional format (logical pathnames), which may be
;;;      mapped reasonably literally (via the translations) to
;;;      a variety of filesystems.
;;;  2.  Pathname Aliasing: The files may exist in different locations
;;;      in the various filesystems. For example, the root directory
;;;      might change. The translations allow such a change easily.
;;;  3.  Cross-host Access: The files need not all exist on the same
;;;      physical host. 
;;;
;;; This definition of logical pathnames provides support for physical
;;; pathnames for Unix, VMS/VAX, Symbolics, and TI Explorers, and is
;;; easily extended to handle additional platforms. Code which may need
;;; customization for particular Lisps and platforms has been commented
;;; with three ampersands (&&&). In addition, the user probably should
;;; define their own canonical types, translation rules, and
;;; logical-pathname-translations. Examples are provided.
;;;
;;; Logical pathnames employ the following syntax:
;;;     [host:] [;] {directory ;}* [name] [. type [. version]]
;;; host      ::= word
;;; directory ::= word | wildcard-word | wildcard-inferiors
;;; name      ::= word | wildcard-word
;;; type      ::= word | wildcard-word
;;; version   ::= word | wildcard-word
;;; word      ::= {letter | digit | -}*
;;; wildcard-word ::= [word] * {word *}* [word]
;;; wildcard-inferiors ::= **
;;;
;;; A wildcard-word of * parses as :wild; all others as strings. These
;;; definitions may be extended (e.g., "newest" parsing as :newest) by
;;; defining new canonical types.
;;;
;;; Incompatibilities with the X3J13 specification:
;;;    -  LOGICAL-PATHNAME is not defined as a subclass of PATHNAME
;;;       since we have no guarrantee about the format of PATHNAME
;;;       (i.e., is it a defstruct or a class definition, what are
;;;       its slots, etc.). Many Lisps will be able to replace the
;;;       definition of PHYSICAL-PATHNAME with their definition of
;;;       PATHNAME by doing a string-replace of "physical-pathname"
;;;       with "pathname" and deleting some definitions from this file.
;;;    -  CLtL does not specify the manner in which wildcards are
;;;       translated. We use reversible wildcard pathname translation,
;;;       similar to that used in the Symbolics logical pathnames.
;;;    -  COMPILE-FILE-PATHNAME has not been defined, since it is
;;;       highly implementation dependent.

;;; ********************************
;;; Examples ***********************
;;; ********************************
;;;
;;; The following examples of the use of logical pathnames are taken
;;; from Section 23.1.5.4 of Guy Steele CLtL 2nd Ed.

#|
(setf (lp:physical-host-type "MY-LISPM") :symbolics)
(setf (lp:logical-pathname-translations "foo")
      '(("**;*.*.*" "MY-LISPM:>library>foo>**>")))

<cl> (lp:translate-logical-pathname "foo:bar;baz;mum.quux.3" :namestring)
"MY-LISPM:>library>foo>bar>baz>mum.quux.3"

(setf (lp:physical-host-type "U") :unix)
(setf (lp:physical-host-type "V") :vms)
(setf (lp:logical-pathname-translations "prog")
      '(("RELEASED;*.*.*"    "U:/sys/bin/my-prog/")
	("RELEASED;*;*.*.*"  "U:/sys/bin/my-prog/*/")
	("EXPERIMENTAL;*.*.*" "U:/usr/Joe/development/prog/")
	("EXPERIMENTAL;DOCUMENTATION;*.*.*" "V:SYS$DISK:[JOE.DOC]")
	("EXPERIMENTAL;*;*.*.*" "U:/usr/Joe/development/prog/*/")
	("MAIL;**;*.MAIL"       "V:SYS$DISK:[JOE.MAIL.PROG...]*.MBX")))

<cl> (lp:translate-logical-pathname "prog:mail;save;ideas.mail.3" :namestring)
"V:SYS$DISK:[JOE.MAIL.PROG.SAVE]IDEAS.MBX.3"
<cl> (lp:translate-logical-pathname "prog:experimental;spreadsheet.c" :namestring)
"U:/usr/Joe/development/prog/spreadsheet.c"

(setf (lp:logical-pathname-translations "prog")
      '(("CODE;*.*.*"    "/lib/prog/")))
<cl> (lp:translate-logical-pathname "prog:code;documentation.lisp" :namestring)
"/lib/prog/documentation.lisp"

(setf (lp:logical-pathname-translations "prog")
      '(("CODE;DOCUMENTATION.*.*"    "/lib/prog/docum.*")
	("CODE;*.*.*"    "/lib/prog/")))
<cl> (lp:translate-logical-pathname "prog:code;documentation.lisp" :namestring)
"/lib/prog/docum.lisp"


(setf (lp:logical-pathname-translations "prog")
      `(("**;*.LISP.*"  ,(lp:logical-pathname "PROG:**;*.L.*"))
	("**;*.FASL.*"  ,(lp:logical-pathname "PROG:**;*.B.*"))
	("CODE;DOCUMENTATION.*.*" "/lib/prog/documentatio.*")
	("CODE;*.*.*"             "/lib/prog/")))
<cl> (lp:translate-logical-pathname "prog:code;documentation.lisp" :namestring)
"/lib/prog/documentatio.l"

|#

;;; ****************************************************************
;;; Logical Pathnames **********************************************
;;; ****************************************************************

;;; ********************************
;;; Massage CLtL2 onto *features* **
;;; ********************************
;;; Let's be smart about CLtL2 compatible Lisps:
(eval-when (compile load eval)
  #+(or (and :excl :allegro-v4.0) :mcl :cormanlisp)
  (pushnew :cltl2 *features*))


;;; ********************************
;;; Packages ***********************
;;; ********************************

;;; Putting this in a separate package doesn't prevent collisions
;;; with the LISP package, since this package :uses the LISP
;;; package.
#-:cltl2
(in-package "LOGICAL-PATHNAME" :nicknames '("LP"))
;;; For CLtL2 compatible lisps...
#+:cltl2
(defpackage "LOGICAL-PATHNAME" (:nicknames "LP") (:use "COMMON-LISP"))
#+:cltl2
(in-package "LOGICAL-PATHNAME")

(export '(logical-pathname 
	  translate-logical-pathname 
	  logical-pathname-translations
	  pathname-host-type
	  append-logical-directories
	  make-logical-pathname
	  physical-host-type
	  load-logical-pathname-translations
	  load-physical-hostab
	  define-translation-rule
	  define-canonical))

(pushnew :logical-pathnames-mk *features*)

#+cormanlisp
(defun lisp-implementation-type ()
  "CORMAN LISP")

#+cormanlisp
(require 'loop)

;;; ********************************
;;; Global Variables ***************
;;; ********************************
(defvar *null-vector* #-cormanlisp (coerce nil 'simple-vector) #+cormanlisp (vector))

(defvar *warn-about-host-type-collisions* t
  "Warn user when a logical host type definition collides with a physical 
   host type definition.")

;;; ********************************
;;; Primitives *********************
;;; ********************************
(defun parse-with-string-delimiter (delim string &key (start 0) end)
  "Returns up to three values: the string up to the delimiter DELIM
   in STRING (or NIL if the field is empty), the position of the beginning
   of the rest of the string after the delimiter, and a value which, if
   non-NIL (:delim-not-found), specifies that the delimiter was not found."
  (declare (string string))
  ;; Conceivably, if DELIM is a string consisting of a single character,
  ;; we could do this more efficiently using POSITION instead of SEARCH.
  ;; However, any good implementation of SEARCH should optimize for that
  ;; case, so nothing to worry about.
  (setq end (or end (length string)))
  (let ((delim-pos (search delim string :start2 start :end2 end))
	(dlength (length delim)))
    (cond ((null delim-pos)		
	   ;; No delimiter was found. Return the rest of the string,
	   ;; the end of the string, and :delim-not-found.
	   (values (subseq string start end) end :delim-not-found))
	  ((= delim-pos start)		
	   ;; The field was empty, so return nil and skip over the delimiter.
	   (values nil (+ start dlength)))
	  ;; The following clause is subsumed by the last cond clause,
	  ;; and hence should probably be eliminated.
;	  ((= delim-pos (- end dlength))
;	   ;; The delimiter is at the end of the string, so return the
;	   ;; field and skip to the end.
;	   (values (subseq string start delim-pos)
;		   end))
	  (t				
	   ;; The delimiter is in the middle of the string. Return the
	   ;; field and skip over the delimiter. 
	   (values (subseq string start delim-pos)
		   (+ delim-pos dlength))))))

(defun parse-with-string-delimiter* (delim string &key (start 0) end
					   include-last)
  "Breaks STRING into a list of strings, each of which was separated
   from the previous by DELIM. If INCLUDE-LAST is nil (the default),
   will not include the last string if it wasn't followed by DELIM
   (i.e., \"foo,bar,\" vs \"foo,bar\"). Otherwise includes it even if
   not terminated by DELIM. Also returns the final position in the string."
  (declare (string string))
  (setq end (or end (length string)))
  (let (result)
    (loop
     (if (< start end)
	 (multiple-value-bind (component new-start delim-not-found)
	     (parse-with-string-delimiter delim string :start start :end end)
	   (when delim-not-found 
	     (when include-last
	       (setq start new-start)
	       (push component result))
	     (return))
	   (setq start new-start)
	   (push component result))
	 (return)))
    (values (nreverse result) 
	    start)))

(defun get-host-string (string &optional (host-delimiter ":") (start 0) end)
  "Strips the host name off the front of the string."
  (setq end (or end (length string)))
  (multiple-value-bind (host pos delim-not-found)
      (parse-with-string-delimiter host-delimiter string :start start :end end)
    (if delim-not-found
	(values nil start)
	(values host pos))))

(defun parallel-substitute (alist string)
  "Makes substitutions for characters in STRING according to the ALIST. 
   In effect, PARALLEL-SUBSTITUTE can perform several SUBSTITUTE
   operations simultaneously."
  (declare (string string))
  ;; This function should be generalized to arbitrary sequences and
  ;; have an arglist (alist sequence &key from-end (test #'eql) test-not
  ;; (start 0) (count most-positive-fixnum) end key).
  (if alist
      (let* ((length (length string))
	     (result (make-string length)))
	(declare (string result))
	(dotimes (i length)
	  (let ((old-char (schar string i)))
	    (setf (schar result i)
		  (or (second (assoc old-char alist :test #'char=))
		      old-char))))
	result)
      string))

(defun name-substitution (alist string)
  "Replaces STRING by it's replacement in ALIST, if present."
  (let ((new-string (second (assoc string alist :test #'string-equal))))
    (or new-string string)))

(unless (fboundp 'lisp::nth-value)
  ;; NTH-VALUE is a CLtL2 addition, so not every lisp has it yet.
  ;; This definition conses a lot, so we shouldn't use it in time-critical
  ;; situations. It is fine for load-physical-hostab which is the only
  ;; place we use it.
  ;; We don't define it in the LISP package to avoid problems in Lucid
  ;; with name conflicts.
  (defmacro nth-value (n form)
    "Returns the nth value of the values returned by form."
    `(nth ,n (multiple-value-list ,form)))
  ; (export 'lisp::nth-value "LISP")
  )

;;; ********************************
;;; Logical Host Tables ************
;;; ********************************
(defvar *logical-pathname-translations-table* (make-hash-table :test #'equal))
(defun canonicalize-logical-hostname (host)
  (when host
	(string-upcase host)))
(defun LOGICAL-PATHNAME-TRANSLATIONS (host)
  "If HOST is the host component of a logical pathname and has been defined
   as a logical pathname host name by SETF of LOGICAL-PATHNAME-TRANSLATIONS,
   this function returns the list of translations for the specified HOST.
   Each translation is a list of at least two elements, a from-wildname
   and a to-wildname. The former is a logical pathname whose host is the
   specified HOST. (I.e., the host of the from-pathname need not be 
   explicitly specified.) The latter is any pathname. If to-wildname coerces to
   a logical pathname, TRANSLATE-LOGICAL-PATHNAME will retranslate the
   result, repeatedly if necessary. Translations are listed in
   the order listed, so more specific from-wildnames must precede more
   general ones."
  ;; would be nice to have host:: specify logical host if physical host
  ;; already exists, to distinguish from host:
  (gethash (canonicalize-logical-hostname host)
	   *logical-pathname-translations-table*))
(defun (setf logical-pathname-translations) (translations host)
  "(setf (logical-pathname-translations host) translations) sets the list
   of translations for the logical pathname host to translations. If host
   is a string that has not previously been used as a logical pathname
   host, a new logical pathname host is defined; otherwise an existing
   host's translations are replaced. Logical pathname host names are
   compared with string-equal."
  (progn
     (when (and *warn-about-host-type-collisions*
		(physical-host-type host))
       (format t "~&Warning in (SETF LOGICAL-PATHNAME-TRANSLATIONS):~
                  ~&   ~S is defined as both a physical host and a logical host."
	     host))
     (setf (gethash (canonicalize-logical-hostname host)
		    *logical-pathname-translations-table*)
	   translations)))

;;; ********************************
;;; Load Logical Translations ******
;;; ********************************
(defvar *logical-translations-directory* nil ; &&&
  "Directory where logical pathname translations are stored.")
;;; (setq *logical-translations-directory* "/usr/local/lisp/Registry/")

(defun LOAD-LOGICAL-PATHNAME-TRANSLATIONS (host)
  "Loads the logical pathname translations for host named HOST if the logical 
   pathname translations are not already defined. First checks for a file
   with the same name as the host (lowercase) and type \"translations\" in
   the current directory, then the translations directory. If it finds such
   a file it loads it and returns T, otherwise it signals an error."
  (unless (logical-pathname-translations host)
    (let* ((trans-fname (concatenate 'string (string-downcase host)
				     ".translations"))
	   (pathname (when *logical-translations-directory*
		       (merge-pathnames *logical-translations-directory*
					trans-fname))))
      (cond ((probe-file trans-fname)
	     (load trans-fname)
	     t)
	    ((and *logical-translations-directory*
		  (probe-file pathname))
	     (load pathname)
	     t)
	    (t
	     (error "Logical pathname translations for host ~A not found."
		    host))))))

;;; ********************************
;;; Physical Host Tables ***********
;;; ********************************
(defvar *physical-host-table* (make-hash-table :test #'equal)
  "Table of physical hosts and system types for those hosts. 
   Valid (implemented) types include :vms, :explorer, :symbolics, :unix :win32 :unc.")
(defun physical-host-type (host)
  (gethash host *physical-host-table*))
(defun (setf physical-host-type) (type host)
  (progn
     (when (and *warn-about-host-type-collisions*
		(logical-pathname-translations host))
       (format t "~&Warning in (SETF PHYSICAL-HOST-TYPE):~
                  ~&   ~S is defined as both a physical host and a logical host."
	       host))
     (setf (gethash host *physical-host-table*)
	   type)))

(defconstant local-host-table		; &&&
  #+:vms "chaos$root:[host.tables]nethosts.txt"
  #-:vms "nethosts.txt")

(defun load-physical-hostab (&optional (local-hostab local-host-table))
  "Loads the physical host namespace table. This is compatible with
   vms and symbolics host tables. Hostab line format should look
   something like:
      HOST NAME,CHAOS-#,STATUS,SYSTEM-TYPE,MACHINE-TYPE,NICKNAMES
   NAME and SYSTEM-TYPE are required; all others are optional (but delimiting
   commas are still required). SYSTEM-TYPE specifies the operating system
   run on the host. This information is used to figure out how to parse
   pathnames for the host. Common values are: LISP, LISPM, UNIX, MACH,
   VMS, and EXPLORER."
  ;; What about SITE, SHORT-NAME, USER-PROPERTY, ADDRESS, PRETTY-NAME, 
  ;; and other Symbolics host attributes?
  (when local-hostab
    (with-open-file (hostab local-hostab :direction :input)
      (do* ((host (read hostab nil :eof)(read hostab nil :eof))
	    ;; host should be NET or HOST.
	    (line (read-line hostab nil :eof)(read-line hostab nil :eof)))
	  ;; Exit on end of file.
	  ((or (eq host :eof)(eq line :eof)))
	;; For each line in the host table, do
	(cond ((null line)
	       (warn "Unexpected EOF in hostab ~S, exiting." local-hostab)
	       (return))
	      ((string-equal (symbol-name host) "HOST")
	       ;; Delete spaces and tabs.
	       (setq line (delete #\tab (delete #\space line)))
	       (let ((pos 0) name system machine nicknames delim-not-found)
		 ;; Snarf the machine NAME.
		 (multiple-value-setq (name pos)
		     (parse-with-string-delimiter "," line :start pos))
		 ;; Throw away chaos host numbers.
		 (setq pos
		       (nth-value 1 (parse-with-string-delimiter 
				     (if (char-equal #\( (char line pos))
					 ")," ",")
				     line :start pos)))
		 ;; Throw away status.
		 (setq pos
		       (nth-value 1 (parse-with-string-delimiter "," line
								 :start pos)))
		 ;; Snarf the system and machine types.
		 (multiple-value-setq (system pos)
		     (parse-with-string-delimiter "," line :start pos))
		 (multiple-value-setq (machine pos delim-not-found)
		     (parse-with-string-delimiter "," line :start pos))
		 (when (and (not delim-not-found)
			    (> (length line) pos))
		   ;; Snarf the nicknames.
		   (setq nicknames
			 (parse-with-string-delimiter* 
			  ","
			  (parse-with-string-delimiter "]" line 
						       :start (1+ pos)))))
		 (unless (or (equal "" system) (null system))
		   (when (equal "LISP" system) (setq system machine))
		   (setq system (intern system 'keyword))
		   (case system
		     ;; :vms, :ms-dos, etc are left alone.
		     ((:mach :unix :unix42) (setq system :unix))
		     ((:lisp :lispm) (setq system :symbolics))
		     ((:appaloosa :explorer) (setq system :explorer)))
		   (setf (physical-host-type name) system)
		   (dolist (name nicknames)
		     (setf (physical-host-type name) system))))))))))

(defun host-type (host)
  "Returns the type of the host. If HOST is a defined logical pathname
   host (i.e., it has translations), returns :logical. Otherwise checks
   the physical type of the host. If HOST is NIL, uses the type of the
   default physical host (the one lisp is running in)."
  ;; Note that logical hosts have priority over physical hosts...
  ;; This is a bad situation, since we don't have any way of 
  ;; distinguishing between host names that are both logical and physical.
  ;; CLtL2 relies on the convention of naming them differently, but
  ;; collisions are going to occur. It would be better to have some
  ;; way of distinguishing the two in a pathname's printed representation.
  (cond ((multiple-value-bind (ignore present)
	     (logical-pathname-translations host)
	   ;; Yet another use for nth-value.
	   (declare (ignore ignore))
	   present)
	 :logical)
	((physical-host-type host))))

(defun pathname-host-type (pathname)
  (cond ((typep pathname 'logical-pathname) :logical)
	((typep pathname 'physical-pathname)
	 (host-type (physical-pathname-host pathname)))
	((stringp pathname) (host-type (get-host-string pathname ":")))))

;;; Setup Default Physical Host
(eval-when (load eval)  ; &&&
(setf (physical-host-type nil)		; nil is default host
      (or #+:vms        :vms
	  #+:explorer   :explorer
	  #+:symbolics  :symbolics
	  #+:unix       :unix
	  #+:hp         :unix
	  #+:cmu        :unix
	  #+:cormanlisp :win32
	  :unix				; default. change if necessary
	  ))
(setf (physical-host-type "Default")
      (physical-host-type nil))
)

;;; ********************************
;;; Translation Rules **************
;;; ********************************
(defstruct translation-rule
  host-type
  case					; Default case of pathname
  char-mappings				; Character substitutions 
  component-mappings			; String substitutions
  version-case				; Case for version component
  type-case				; Case for type component
  name-case				; Case for name
  component-case			; Case for directory names
  )

(defvar *permanent-translation-rules* (make-hash-table :test #'equal)
  "Hash table of default translation rules for each type of host.")

(defvar *default-translation-rule* (make-translation-rule))

(defmacro define-translation-rule (host-type 
				   &key case char-mappings component-mappings
				   version-case
				   type-case
				   name-case
				   component-case)
  "Defines translation rules for hosts of type host-type.
   Case may be :unchanged, :upper, :lower, or :capitalize. This provides a
   default case translation; version-case, type-case, name-case, and
   component-case will shadow this value if non-nil.
   Char-mappings is a list of character substitutions which occur in parallel.
   Component-mappings is a list of string substitutions."
  ;; Note: Currently there is only one rule per host-type.
  `(setf (gethash ,host-type *permanent-translation-rules*)
	 (make-translation-rule :host-type ',host-type 
				:case ',case
				:char-mappings ',char-mappings
				:component-mappings ',component-mappings
				:version-case ',version-case
				:type-case ',type-case
				:name-case ',name-case
				:component-case ',component-case)))

(defun find-translation-rule (host-type)
  (or (gethash host-type *permanent-translation-rules*)
      *default-translation-rule*))

(defun choose-case (rule level)
  (or (case level
	(version (translation-rule-version-case rule))
	(type (translation-rule-type-case rule))
	(name (translation-rule-name-case rule))
	(component (translation-rule-component-case rule)))
      (translation-rule-case rule)))

(defun casify (thing case)
  (if (stringp thing)
    (case case
      (:upper (string-upcase thing))
      (:lower (string-downcase thing))
      (:capitalize (string-capitalize thing))
      (:unchanged thing)
      (otherwise thing))
    thing))

(define-translation-rule :vms
  :case :upper :char-mappings ((#\- #\_)))

(define-translation-rule :unix
  :case :unchanged ; :lower
  :type-case :lower
  )

(define-translation-rule :win32
  :case :unchanged ; :lower
  :type-case :lower
  )

(define-translation-rule :unc
  :case :unchanged ; :lower
  :type-case :lower
  )


(define-translation-rule :logical
  :case :upper
  :name-case :unchanged)

;;; ********************************
;;; Canonical Types ****************
;;; ********************************
(defvar *default-canonical-types* (make-hash-table :test #'equal)
  "Alists of canonical types and default surface types.")
(defvar *canonical-types-alist* (make-hash-table :test #'equal)
  "Alists of canonical types and surface types for various hosts.")

(defmacro define-canonical (level canonical default  &body specs)
  "Defines a new canonical type. Level specifies whether it is a 
   canonical type, version, name, or component. Default is a string
   containing the default surface type for any kind of host not
   mentioned explicitly. The body contains a list of specs that define
   the surface types that indicate the new canonical type for each host.
   For systems with more than one possible default surface form,
   the form that appears first becomes the preferred form for the type."
  `(progn
     (setf (gethash ',level *default-canonical-types*)
	   (cons (list ',canonical ',default)
		 (remove ',canonical 
			 (gethash ',level *default-canonical-types*)
			 :key #'car)))
;     (push (list ',canonical ',default) 
;	   (gethash ',level *default-canonical-types*))
     (setf (gethash ',level *canonical-types-alist*)
	   (cons (list* ',canonical ',specs)
		 (remove ',canonical 
			 (gethash ',level *canonical-types-alist*)
			 :key #'car)))
;     (push (list* ',canonical ',specs) 
;	   (gethash ',level *canonical-types-alist*))
     ))

(defun member-or-eq (x list-or-atom)
  (cond ((listp list-or-atom) (member x list-or-atom))
	(t (eq x list-or-atom))))

(defun surface-form (canonical host-type &optional (level 'type))
  "Given the canonical form of some canonical type, replaces it with
   the appropriate surface form."
  (let ((case (choose-case (find-translation-rule host-type) level)))
    (casify (or (second (assoc host-type
			       (cdr (assoc canonical
					   (gethash level 
						    *canonical-types-alist*)
					   :test #'equal))
			       :test #'member-or-eq))
		(second (assoc canonical
			       (gethash level *default-canonical-types*)
			       :test #'equal))
		canonical)
	    case)))

(defun canonicalize (surface-form host-type &optional (level 'type))
  "Given the surface form of some canonical type, replaces it with
   the appropriate canonical type."
  (cond ((stringp surface-form)
	 (or (first (find surface-form (gethash level *canonical-types-alist*)
			  :key #'cdr
			  :test #'(lambda (surf alist)
				    (member surf
					    (cdr (assoc host-type alist 
							:test #'member-or-eq))
					    :test #'string-equal))))
	     (first (find surface-form 
			  (gethash level *default-canonical-types*) 
			  :key #'second :test #'string-equal))
	     (coerce surface-form 'string)))
	(t surface-form)))


;;; *** Some Sample Types ***

(define-canonical host :default ""
  (:unix #+:CMU "Mach" "" "Default"))

(define-canonical host "Default" ""
  (:unix nil "" "Default"))

(define-canonical device :unspecific "")

(define-canonical component :absolute ""
  (:unix "/")
  (:win32 "/" "\\")
  (:symbolics ">")
  (:logical   "")
  (:vms       ""))
(define-canonical component :relative ""
  (:unix "")
  (:win32 "")
  (:unc "")
  (:symbolics "")
  (:logical   ";")
  (:vms       "."))
(define-canonical component :wild "*")
(define-canonical component :wild-inferiors "**"
  (:vms ".."))

(define-canonical name :wild "*")

(define-canonical type :unspecific "") ;; null type
(define-canonical type :wild "*")      ;; wild type

;; uncommented the "L" causes the last Steele example to break, of course.
(define-canonical type :lisp "LISP" 
  (:unix-ucb "LISP")
  (:unix #+(and :sun :kcl :unix) "lsp" 
	 "lisp" ; "L" #+:excl "cl"
)
  (:win32 "lisp" "cl" "lsp")
  (:unc "lisp" "cl" "lsp")
  (:vms "LSP" "LISP")
  ;; (:vms4 "LSP" "LISP")
  ((:tops-20 :tenex) "LISP" "LSP"))

(define-canonical type :text "TEXT" 
  (:unix "text" "txt" "tx")
  (:win32 "txt" "text")
  (:vms "TXT")
  ((:tops-20 :tenex) "TXT"))

(define-canonical type :fasl "FASL" 
  (:unix #+:hp "b"
	 #+(and :sun :kcl :unix) "o"
	 #+:cmu "fasl"
	 "fasl" "bin" "BN")
  (:win32 "fasl")
  (:vms "FAS" "BIN")
  (:explorer "XLD")
  (:symbolics "BIN")
  ((:tops-20 :tenex) "BIN"))

(define-canonical version :wild "*")
(define-canonical version :newest "newest")

#|
;;; Examples:
<cl> (lp::canonicalize "*" :unix)
:WILD
<cl> (lp::surface-form :fasl :unix)
"fasl"
<cl> (lp::surface-form :fasl :vms)
"FAS"
|#


;;; ********************************
;;; Pathname Defstruct *************
;;; ********************************
;;;
;;; We define a generic physical pathname (physical-pathname defstruct) because
;;; we have absolutely NO guarrantees about the structure of pathnames.
;;; Pathnames may be defstructs or classes, and the slots may have arbitrary
;;; types, especially with respect to the directory slot. Depending on the
;;; lisp, the directory slot may be a list, vector, simple-vector,
;;; string, keyword, or nil. If a list or vector, the items in the list
;;; may be strings, keywords (for canonical types), or nil. The first item
;;; in the list may or may not be a special keyword (e.g., :relative and
;;; :absolute). 
;;;
;;; The lack of a common interface to pathnames means that any implementation
;;; of logical pathnames must parse and generate the pathname (namestring)
;;; formats for a variety of file-servers. We can't simply rely on the
;;; lisp's implementation of the PATHNAME defstruct, because that does not
;;; necessarily handle the formats of file-servers of a different type 
;;; (translations may be in the format of the target file server). Also,
;;; inconsistency in the implementation of the PATHNAME type means that we
;;; would have to special case most of the code for each and every lisp.
;;;
;;; Instead, we parse the pathnames into a common format (the physical-pathname
;;; defstruct), from which we generate a namestring in a format acceptable
;;; to the underlying lisp. The namestring (which is a string in *all* the
;;; lisps) serves as the interface to the lisp's implementation of pathnames.
;;;
;;; As it currently stands, X3J13's spec for logical pathnames tries to 
;;; accomplish two distinct goals:
;;;    (1) isolate pathname reference from actual file location (logical
;;;        as opposed to physical pathnames) 
;;;    (2) provide a common format for namestring syntax and 
;;;        pathname structure
;;; This is trying to accomplish too much within a single framework. Instead,
;;; the second goal should be decoupled from logical pathnames and made a 
;;; requirement for pathnames in general. 
;;;
;;; In other words, let there be a standard namestring syntax and a fully
;;; specified structure for physical pathnames (not just logical pathnames).
;;; This standard should subsume the requirements of all current lisps, and
;;; the individual lisp implementation should worry about interfacing with
;;; the file system. There is no good reason why a programmer should have
;;; to know the peculiarities of a filesystem when writing software. The X3J13
;;; spec just shoves it under the rug, forcing the programmer to deal with
;;; it when writing the translations file. 
;;;
;;; Because there is no standard for pathnames, we're forced into a situation
;;; where different lisps running on the same physical host may have
;;; different namestring syntaxes, so knowing the physical host type is not
;;; a guarrantee of the pathname syntax. 
;;;
(defstruct (physical-pathname
	    (:conc-name %physical-pathname-)
	    (:print-function %print-physical-pathname)
	    (:constructor %make-physical-pathname 
	     (host device directory name type version))
	    (:predicate physical-pathnamep))	     
  "Physical-Pathname is the underlying structure for a pathname."
  (host nil :type (or null keyword string))
  (device nil :type (or null keyword string))
  (directory nil :type (or null simple-vector))
  (name nil :type (or null keyword string))
  (type nil :type (or null keyword string))
  version)

(defun %print-physical-pathname (pname stream depth)
  (declare (ignore depth))
  (format stream "#.(physical-pathname ~S)" (physical-namestring pname)))

(defun make-physical-pathname (&key host device directory name type version)
  (let ((host-type (host-type host)))
    (when (stringp directory)
      (setq directory 
	    (%physical-pathname-directory (parse-generic-namestring directory
								    host))))
    (%make-physical-pathname 
     (canonicalize host host-type 'host)
     (canonicalize device host-type 'device)
     directory
     (canonicalize name host-type 'name)
     (canonicalize type host-type 'type)
     (canonicalize version host-type 'version)
     )))

(defun ensure-physical-pathname (thing)
  (if (physical-pathnamep thing) thing (physical-pathname thing)))

;;; The following cannot be done by the accessors because the pathname
;;; arg may be a string.

(defun physical-pathname-host (pathname)
  "Returns the host of PATHNAME, which may be a string or pathname."
  (%physical-pathname-host (ensure-physical-pathname pathname)))

(defun physical-pathname-device (pathname)
  "Returns the device of PATHNAME, which may be a string or pathname."
  (%physical-pathname-device (ensure-physical-pathname pathname)))

(defun physical-pathname-directory (pathname)
  "Returns the directory of PATHNAME, which may be a string or pathname."
  (%physical-pathname-directory (ensure-physical-pathname pathname)))

(defun physical-pathname-name (pathname)
  "Returns the name of PATHNAME, which may be a string or pathname."
  (%physical-pathname-name (ensure-physical-pathname pathname)))

(defun physical-pathname-type (pathname)
  "Returns the type of PATHNAME, which may be a string or pathname."
  (%physical-pathname-type (ensure-physical-pathname pathname)))

(defun physical-pathname-version (pathname)
  "Returns the version of PATHNAME, which may be a string or pathname."
  (%physical-pathname-version (ensure-physical-pathname pathname)))

;;; ********************************
;;; Logical Pathname Defstruct *****
;;; ********************************
(defstruct (logical-pathname
	    (:include physical-pathname)
	    (:conc-name %logical-pathname-)
	    (:print-function %print-logical-pathname)
	    (:constructor %make-logical-pathname 
			  (host device directory name type version))
	    (:predicate logical-pathnamep))	     
  "Logical-pathname is the underlying structure for a logical pathname.")

(defun %print-logical-pathname (pname stream depth)
  (declare (ignore depth))
  (format stream "#.(logical-pathname ~S)" (logical-namestring pname)))

(defun make-logical-pathname (&key host directory name type version)
  (let ((host-type (host-type host)))
    (when (stringp directory)
      (setq directory 
	    (%logical-pathname-directory (parse-generic-namestring directory
								   host))))
    (%make-logical-pathname 
     (canonicalize host host-type 'host)
     :unspecific
     directory
     (canonicalize name host-type 'name)
     (canonicalize type host-type 'type)
     (canonicalize version host-type 'version)
     )))

(defun ensure-logical-pathname (thing)
  (if (logical-pathnamep thing) thing (logical-pathname thing)))

;;; The following cannot be done by the accessors because the pathname
;;; arg may be a string.

(defun logical-pathname-host (logical-pathname)
  "Returns the logical-pathname-host of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-host (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-directory (logical-pathname)
  "Returns the logical-pathname-directory of LOGICAL-PATHNAME.
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-directory (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-name (logical-pathname)
  "Returns the logical-pathname-name of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-name (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-type (logical-pathname)
  "Returns the logical-pathname-type of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-type (ensure-logical-pathname logical-pathname)))

(defun logical-pathname-version (logical-pathname)
  "Returns the logical-pathname-type of LOGICAL-PATHNAME. 
   LOGICAL-PATHNAME may be a string or logical pathname."
  (%logical-pathname-version (ensure-logical-pathname logical-pathname)))


;;; ********************************
;;; Pathname Namestring Functions **
;;; ********************************
(defun logical-namestring (logical-pathname)
  "Returns the full form of LOGICAL-PATHNAME as a string."
  (setq logical-pathname (logical-pathname logical-pathname))
  (let ((host      (%logical-pathname-host logical-pathname))
	(directory (%logical-pathname-directory logical-pathname))
	(name      (%logical-pathname-name logical-pathname))
	(type      (%logical-pathname-type logical-pathname))
	(version   (%logical-pathname-version logical-pathname))
	result)
    (declare (string result))
    ;; FORMAT would have been easier, but this is faster.
    (when host
      (setq result 
	    (concatenate 'string 
			 (surface-form host :logical 'host) ":")))
    (when directory
      (setq result
	    (concatenate 'string
			 result
			 (the string (%directory-string directory)))))
    (when name
      (setq result
	    (concatenate 'string
			 result
			 (the string (surface-form name :logical 'name)))))
    (when type
      (setq result
	    (concatenate 'string
			 result "."
			 (the string (surface-form type :logical 'type)))))
    (when version
      (setq result
	    (concatenate 'string
			 result "."
			 (the string
			      (%version-to-string version)))))
    result))

(defun %directory-string (dirlist &optional (host-type :logical)
				  (dir-delim #\;))
  "Converts a vector of the form #(\"foo\" \"bar\" ... \"baz\") into
   a string of the form \"foo;bar;...;baz;\""
  (declare (simple-vector dirlist))
  (let* ((numdirs (length dirlist))
	 (length numdirs))
    (declare (fixnum numdirs length))
    (dotimes (i numdirs)
      (let ((component (#+:cmu svref #-:cmu aref dirlist i)))
	(case component
	  ;; Do we have to worry about Lucid's :root here???
	  ((:relative :absolute) 
	   (incf length
		 (the fixnum
		      (1- (length (surface-form component
						host-type 'component))))))
	  (otherwise (incf length
			   (the fixnum
				(length (surface-form component host-type
						      'component))))))))
    (do ((result (make-string length))
	 (index 0 (1+ index))
	 (position 0))
	((= index numdirs) result)
      (declare (string result))
      (let* ((component (#+:cmu svref #-:cmu aref dirlist index))
	     (string (surface-form component host-type 'component))
	     (len (length string))
	     (end (+ position len)))
	(declare (string string)
		 (fixnum len end))
	(replace result string :start1 position :end1 end :end2 len)
	(unless (or (eq component :absolute)(eq component :relative))
	  (setf (schar result end) dir-delim)
	  (setq position (+ end 1)))))))

(defun %version-to-string (version &optional (host-type :logical))
  (cond ((surface-form version host-type 'version))
	((zerop version) "0")
	((eql version 1) "1")
	(t
	 ;; Using FORMAT would have been easier, but this is faster.
	 (do* ((len (1+ (truncate (log version 10)))) ; base 10 num digits
	       (res (make-string len))
	       (i (1- len) (1- i))
	       (q version) ; quotient
	       (r)) ; residue
	     ((zerop q) ; nothing left
	      res)
	   (declare (string res)
		    (fixnum len i r))
	   (multiple-value-setq (q r) (truncate q 10))
	   (setf (schar res i) (schar "0123456789" r))))))

(defun physical-namestring (pathname)
  ;; needs to get appropriate surface forms
  (setq pathname (physical-pathname pathname))
  (let* ((host (%physical-pathname-host pathname))
	 (host-type (host-type host))
	(device    (%physical-pathname-device pathname))
	(directory (coerce (%physical-pathname-directory pathname) 'list))
	(name      (%physical-pathname-name pathname))
	(type      (%physical-pathname-type pathname))
	(version   (%physical-pathname-version pathname))
	(ptype (pathname-host-type pathname)))
    (setq host (surface-form host host-type 'host)
	  name (surface-form name host-type 'name)
	  type (surface-form type host-type 'type)
	  version (surface-form version host-type 'version))
    ;; Does directory need to be mapcar'ed into surface-form?
    ;; Yes, but we can probably ignore it for now, since the only
    ;; canonical types defined so far are :wild and :wild-inferiors,
    ;; which we don't have to support. Probably wouldn't hurt to
    ;; uncomment this code.
    ;;(setq directory 
    ;;      (cons (car directory)
    ;;            (mapcar #'(lambda (comp)
    ;;			    (surface-form comp host-type 'component)) 
    ;;			(cdr directory))))
    (case ptype
      (:logical
       (logical-namestring pathname))
      (:unix
       (format nil "~@[~A:~]~A~{~A/~}~@[~A~@[.~A~@[.~A~]~]~]"
	       host (case (car directory)
		      (:absolute "/")
		      (otherwise ""))
	       (cdr directory)
	       name type version))
	  (:win32
       (format nil "~@[~A:~]~A~{~A\\~}~@[~A~@[.~A~@[.~A~]~]~]"
	       host (case (car directory)
		      (:absolute "\\")
		      (otherwise ""))
	       (cdr directory)
	       name type version))
      (:vms       
       ;; was "~@[~A:~]~@[~A:~][~A~{~A.~}]~@[~A~@[.~A~@[.~A~]~]~]"
       ;; which was adding an extra "." to path
       ;; such as [a.b] => [a.b.]
       (format nil 
	       "~@[~A:~]~@[~A:~][~A~{~A~^.~}]~@[~A~@[.~A~@[.~A~]~]~]"
	       host device (case (car directory)
			     (:relative ".")
			     (otherwise ""))
	       (cdr directory)
	       name type version))
      (:explorer
       (format nil "~@[~A:~]~A~{~A~^.~};~@[~A~@[.~A~@[#~A~]~]~]"
	       host (case (car directory)
		      (:relative ".")
		      (otherwise ""))
	       (cdr directory)
	       name type version))
      (:symbolics
       (format nil "~@[~A:~]~A~{~A>~}~@[~A~@[.~A~@[.~A~]~]~]"
	       host (case (car directory)
		      (:absolute ">")
		      (otherwise ""))
	       (cdr directory)
	       name type version))
      (otherwise
       ;; Use UNIX as default.
       (format nil "~@[~A:~]~A~{~A/~}~@[~A~@[.~A~@[.~A~]~]~]"
	       host (case (car directory)
		      (:absolute "/")
		      (otherwise ""))
	       (cdr directory)
	       name type version))
      )))

;;; ********************************
;;; Pathname Parsing Functions *****
;;; ********************************
(defvar *default-logical-pathname-host* nil ;;"SYS"
  "Default logical pathname host, so that the logical pathname
   foo;bar means SYS:foo;bar.")

(defun logical-pathname (thing 
			 &optional (host *default-logical-pathname-host*))
  "Converts THING to a logical pathname and returns it. THING may be
   a logical pathname, a logical pathname namestring containing a 
   host component, or a stream for which the pathname function returns
   a logical pathname."
  (etypecase thing
    (string
     (values (parse-generic-namestring thing host
				       *default-pathname-defaults* 
				       :force-logical t)))
    (physical-pathname thing)
    (logical-pathname thing)
	#+cormanlisp (lisp::pathname (physical-pathname (lisp::namestring thing) host))
    #+:CMU(stream           (logical-pathname (lisp::file-name thing) host))))

(defun physical-pathname (thing &optional host)
  "Converts THING to a physical-pathname and returns it. THING may be
   a pathname, a pathname namestring containing a 
   host component, or a stream for which the file-name function returns
   a pathname."
  (typecase thing
    (string           (values (parse-generic-namestring thing host)))
    (logical-pathname thing)
    (physical-pathname thing)
    #+:CMU(stream           (physical-pathname (lisp::file-name thing) host))))

(defun parse-generic-namestring (thing &optional host
				       (defaults *default-pathname-defaults*)
				       &key (start 0) end junk-allowed
				       force-logical)
  "Convert namestring into a pathname."
  (declare (ignore junk-allowed))
  (unless end (setf end (length thing)))
  (let ((host-string (get-host-string thing ":"))
	host-type)
    (unless host-string (setq host-string host))
    (when (and host host-string (not (string-equal host host-string)))
      (cerror "Ignore it."
	      "Host mismatch in ~S: ~S isn't ~S"
	      'parse-generic-namestring
	      host-string
	      host))
    (if force-logical
	(setq host-type :logical)
      (setq host-type (host-type host-string)))
    (if host-type
	(multiple-value-bind (parsed-host device directory name type version)
	    (do-generic-pathname-parse thing host-type start end)
	  (let ((defaults-p (and (typep defaults 'physical-pathname)
				 (equal host-type
					(pathname-host-type defaults)))))
	    (values
	     (case host-type
	       (:logical
		(make-logical-pathname 
		 :host	(or parsed-host host
			    (and defaults-p (logical-pathname-host defaults))
;			    (when directory "Default")
			    )
		 :directory (or directory
				(and defaults-p
				     (logical-pathname-directory defaults)))
		 :name      (or name
				(and defaults-p
				     (logical-pathname-name defaults)))
		 :type      (or type
				(and defaults-p
				     (logical-pathname-type defaults)))
		 :version   (or version
				(and defaults-p
				     (logical-pathname-version defaults)))))
	       (otherwise 
		(make-physical-pathname 
		 :host	(or parsed-host host
			    (and defaults-p (physical-pathname-host defaults))
;			    (when directory "Default")
			    )
		 :device    (or device
				(and defaults-p
				     (physical-pathname-device defaults)))
		 :directory (or directory
				(and defaults-p
				     (physical-pathname-directory defaults)))
		 :name      (or name
				(and defaults-p
				     (physical-pathname-name defaults)))
		 :type      (or type
				(and defaults-p
				     (physical-pathname-type defaults)))
		 :version   (or version
				(and defaults-p
				     (physical-pathname-version defaults))))))
	     end)))
	;; Unknown host type, wing it with parse-namestring.
      (when thing
	(lisp:parse-namestring thing host defaults 
			       :start start :end end)))))

;;; ********************************
;;; Parse Physical Pathnames *******
;;; ********************************
(defun do-generic-pathname-parse (string host-type &optional (start 0) end)
  "Splits string into a logical host, a vector of directories, a file name,
   a file type, and a file version."
  (declare (string string))
  (case host-type
    (:logical
     ;; Parses Logical Pathnames of the following format:
     ;;     host:dir1;dir2;name.type.version
     (parse-generic-pathname string start end ":" nil ";" "." "." "."))
    (:unix
     ;; Parses Unix pathnames of the following format:
     ;;     host:/dir1/dir2/*/name.type.version 
     (parse-generic-pathname string start end ":" t "/" "." "." "."))
	(:win32
     ;; Parses Win32 pathnames of the following format:
     ;;     host:\\dir1\\dir2\\*\\name.type.version 
     (parse-generic-pathname string start end ":" t "\\" "." "." "."))
    (:symbolics 
     ;; Parses Symbolics Pathnames of the following format:
     ;;     host:>dir1>dir2>**>name.type.version
     (parse-generic-pathname string start end ":" t ">" "." "." "."))
    (:vms       (parse-vms-pathname string start end))
    (:explorer  (parse-explorer-pathname string start end))
    (otherwise  (warn "~&PARSE-~A-PATHNAME not yet implemented.~%" host-type)
		nil)))

(defun parse-generic-pathname (string &optional (start 0) end
				      (host-delim ":")(lead-is-abs t)
				      (dir-delim "/")
				      (name-delim ".")(type-delim ".")
				      (version-delim "."))
  "Splits string into a host, vector of directories, a file name, type,
   and version. Parses generic pathnames."
  (declare (string string))
  (setq end (or end (length string)))
  (let (host a-vs-r directories name type version host-type)
    (multiple-value-setq (host start)
	(get-host-string string host-delim start end))
    (setq host-type (host-type host))
    ;; Absolute vs. Relative
    (cond ((and (not (string-equal string "" :start1 start))
		(char= (char dir-delim 0) (char string start))) 
	   (setq a-vs-r (if lead-is-abs :absolute :relative))
	   (incf start))
	  (t (setq a-vs-r (if lead-is-abs :relative :absolute))))
    ;; Split off the components
    (multiple-value-bind (dirs new-start)
	(parse-with-string-delimiter* dir-delim string :start start :end end)
      (setq directories
	    (cons a-vs-r 
		  (mapcar #'(lambda (dir)
			      (canonicalize dir host-type 'component))
			  dirs))
	    start new-start))
    ;; Split off the name, type, and version
    (when (< start end)
      (multiple-value-setq (name start)
	  (parse-with-string-delimiter name-delim string
				       :start start :end end))
      (when (< start end)
	(multiple-value-setq (type start)
	    (parse-with-string-delimiter type-delim string
					 :start start :end end))
	(when (< start end)
	  (multiple-value-setq (version start)
	      (parse-with-string-delimiter version-delim string
					   :start start :end end)))))
    ;; Return the values
    (values host
	    :unspecific
	    (when (or host directories)
	      (coerce directories 'vector))
	    name
	    type
	    version
	    ;; This last is the remaining cruft. Should be nil.
	    (when (< start end) (subseq string start end)))))

(defun parse-vms-pathname (string &optional (start 0) end)
  "Splits string into a host, vector of directories, a file name, type,
   and version. Parses VMS pathnames of the following formats:
       host::device:[dir1.dir2...]name.type;version
       host::device:<dir1.dir2...>name.type.version
       host:device:<dir1.dir2...>name.type.version &c
   .. = :wild-inferiors"
  (declare (string string))
  (setq end (or end (length string)))
  (let (host device a-vs-r (directories "") name type version)
    (multiple-value-bind (new-host new-start)
	(get-host-string string "::" start end)
      (if new-host
	  (setq host new-host start new-start)
	  (multiple-value-setq (host start) (get-host-string string ":" start end))))
    (multiple-value-setq (device start)	(get-host-string string ":" start end))
    (when (plusp (length string))
      (case (char string start)
	(#\[  (multiple-value-setq (directories start)
		  (parse-with-string-delimiter "]" string 
					       :start (1+ start) :end end)))
	(#\<  (multiple-value-setq (directories start)
		  (parse-with-string-delimiter ">" string 
					       :start (1+ start) :end end)))))
    ;; Absolute vs. Relative
    (cond ((and (not (zerop (length directories)))
		(char= #\. (char directories 0)))
	   (setq a-vs-r :relative))
	  (t (setq a-vs-r :absolute)))
    ;; Split off the components
    (multiple-value-bind (dirs)
	(parse-with-string-delimiter* "." directories 
				      :start (if (eq a-vs-r :relative) 1 0)
				      :include-last t) ; <<< fix
      (let ((last2 (when (> (length dirs) 1)
		     (nthcdr (- (length dirs) 2) dirs))))
	(when (equal last2 '(nil nil))
	  (rplaca last2 "..")
	  (rplacd last2 nil)))
      (setq directories
	    (cons a-vs-r 
		  (mapcar #'(lambda (dir) (canonicalize dir :vms 'component))
			  dirs))))
    ;; Split off the name, type, and version
    (when (< start end)
      (multiple-value-setq (name start)
	  (parse-with-string-delimiter "." string :start start :end end))
      (when (< start end)
	(multiple-value-bind (new-type new-start delim-not-found)
	    (parse-with-string-delimiter ";" string :start start :end end)
	  (cond (delim-not-found
		 (multiple-value-setq (type start)
		     (parse-with-string-delimiter "." string
						  :start start :end end)))
		(t
		 (setq type new-type start new-start))))
	(when (< start end)
	  (multiple-value-setq (version start)
	      (parse-with-string-delimiter "." string :start start :end end)))))
    ;; Return the values
    (values host
	    device
	    (when (or host directories)
	      (coerce directories 'vector))
	    name
	    type
	    version
	    ;; This last is the remaining cruft. Should be nil.
	    (when (< start end) (subseq string start end)))))

(defun parse-explorer-pathname (string &optional (start 0) end)
  "Splits string into a host, vector of directories, a file name, type,
   and version. Parses TI Explorer pathnames of the following format:
       host:dir1.dir2...;name.type#version"
  (declare (string string))
  (setq end (or end (length string)))
  (let (host a-vs-r (directories "") name type version)
    (multiple-value-setq (host start)
	(get-host-string string ":" start end))
    (multiple-value-setq (directories start)
	(parse-with-string-delimiter ";" string 
				     :start start :end end))
    ;; Absolute vs. Relative
    (cond ((and (not (zerop (length directories)))
		(char= #\. (char directories 0)))
	   (setq a-vs-r :relative))
	  (t (setq a-vs-r :absolute)))
    ;; Split off the components
    (multiple-value-bind (dirs)
	(parse-with-string-delimiter* "." directories 
				      :start (if (eq a-vs-r :relative) 1 0)
				      :end nil :include-last t)

      (setq directories
	    (cons a-vs-r 
		  (mapcar #'(lambda (dir)
			      (canonicalize dir :explorer 'component))
			  dirs))))
    ;; Split off the name, type, and version
    (when (< start end)
      (multiple-value-bind (new-name new-start delim-not-found)
	  (parse-with-string-delimiter "." string :start start :end end)
	(when (not delim-not-found)
	  (setq name new-name start new-start)))
      (when (< start end)
	(multiple-value-setq (type start)
	    (parse-with-string-delimiter "#" string :start start :end end))
	(when (< start end)
	  (multiple-value-setq (version start)
	      (parse-with-string-delimiter "." string :start start :end end)))))
    ;; Return the values
    (values host
	    :unspecific
	    (when (or host directories)
	      (coerce directories 'vector))
	    name
	    type
	    version
	    ;; This last is the remaining cruft. Should be nil.
	    (when (< start end) (subseq string start end)))))


;;; ********************************
;;; Convert Generic Pathnames ******
;;; ********************************
;;; Converts a generic pathname to a format for standard lisp functions.

(defvar *translation-output* :namestring
  "Specifies whether the output of translate-logical-pathname
   should be a :namestring or a :pathname made with lisp:make-pathname,
   or :as-is.")

(defconstant directory-structure-type ; &&&
  #+:CMU 'simple-vector
  #+:lispm 'list
  #+:kcl   'list
  #+:hp    'list
  #+cormanlisp 'list
  #-(or :cmu :lispm :kcl :hp :cormanlisp)
  (cond ((string-equal (lisp-implementation-type) "VAX LISP") 'list)
	(t 'list)))

(defun convert-generic-pathname (pathname 
				 &optional (output-type *translation-output*))
  (when pathname
    (case output-type 
      (:namestring        (physical-namestring pathname))
      (:pathname
       (let ((host       (%physical-pathname-host pathname))
	     (device     (%physical-pathname-device pathname))
	     (directory  (coerce (%physical-pathname-directory pathname)
				 'list))
	     (name       (%physical-pathname-name pathname))
	     (type       (%physical-pathname-type pathname))
	     (version    (%physical-pathname-version pathname))
	     (target-host-type  (host-type nil))
	     a-vs-r)
	 ;; Handle :absolute/:relative crap.
	 (setq a-vs-r (pop directory))
	 (case a-vs-r
	   (:absolute
	    #+:cmu (setf device :absolute)
	    #+(and :sun :kcl :unix) (setq a-vs-r :root))
	   (:relative
	    #+:cmu (setf device "Default")))
	 ;; Reverse canonicalizations
	 (setq host (surface-form host target-host-type 'host)
	       directory (mapcar #'(lambda (dir)
				     (surface-form dir target-host-type
						   'component))
				 directory)
	       name (surface-form name target-host-type 'name)
	       type (surface-form type target-host-type 'type)
	       version (surface-form version target-host-type 'version))
	 ;; Fixup Host
	 #+:cmu (setf host "Mach")
	 ;; Fixup Directory
	 #-:cmu (push a-vs-r directory)
	 (setq directory (coerce directory directory-structure-type))
       
	 (when (string-equal (lisp-implementation-type) "VAX LISP")
	   (setq directory
		 (cond ((stringp directory) directory)
		       ((eq (car directory) :absolute)
			(format nil "[~{~A~^.~}]" (cdr directory)))
		       ((eq (car directory) :relative)
			(format nil "[.~{~A~^.~}]" (cdr directory)))
		       (t (format nil "[~{~A~^.~}]" directory)))))

	 ;; Return the new pathname
	 (make-pathname :host host :device device :directory directory
			:name name :type type :version version)
	 ))
      (otherwise pathname)))) 


;;; ********************************
;;; Translate Logical Pathnames ****
;;; ********************************
(defvar *circularity-check-table* (make-hash-table :test #'equal)
  "This table is used to prevent infinite circular loops in the logical
   pathname resolution. If a pathname's entry in this table is set
   to T, it has already been \"seen\". Seeing such a pathname twice
   is an error.")

(defun translate-logical-pathname (logical-pathname
				   &optional
				   (output-format *translation-output*))
  "Translates a logical pathname to the corresponding physical pathname.
   The pathname argument is first coerced to a logical pathname [this 
   should really be pathname, but for that we'd have to redefine
   make-pathname and friends to check whether the host is a logical host].
   If the coerced argument is a logical pathname, the first matching
   translation (according to LOGICAL-PATHNAME-MATCH-P) of the logical pathname
   host is applied, as if by calling TRANSLATE-LOGICAL-PATHNAME-AUX.
   If the result is a logical pathname, this process is repeated. 
   When the result is finally a physical pathname, it is returned. If no 
   translation matches a logical pathname, or the resolution process loops,
   an error is signaled.

   TRANSLATE-LOGICAL-PATHNAME may perform additional translations,  
   to provide translation of file types to local naming conventions, to
   accommodate physical file systems with names of limited length, or to
   deal with special character requirements such as translating hyphens
   to underscores or uppercase letters to lowercase."

  ;; Ensure that it is a logical pathname
  (setq logical-pathname (logical-pathname logical-pathname))
  (when (typep logical-pathname 'logical-pathname)
    ;; To prevent circular loops...
    (let ((namestring (logical-namestring logical-pathname)))
      (setf (gethash namestring *circularity-check-table*) T))
    (unwind-protect
	(resolve-logical-pathname logical-pathname output-format)
      (clrhash *circularity-check-table*))))

(defun resolve-logical-pathname (logical-pathname 
				 &optional
				 (output-format *translation-output*))
  "Resolve the logical pathname into a physical pathname using the
   translations table."
  (let ((logical-host (logical-pathname-host logical-pathname)))
    (if logical-host
	(let ((translated-pathname 
	       (map-logical-pathname logical-pathname logical-host
				     output-format)))
	  (if translated-pathname
	      (or (when (eq (pathname-host-type translated-pathname) :logical)
		    ;; If the translation is itself a logical pathname,
		    ;; repeat the process until a physical pathname is reached.
		    (check-logical-pathname translated-pathname)
		    (resolve-logical-pathname translated-pathname 
					      output-format))
		  translated-pathname)
	      (error "No translation mapping for ~S." logical-pathname)))
	(error "No such logical host in ~S." logical-pathname))))

(defun check-logical-pathname (pathname)
  "Ensure that there are no cycles in the translations."
  (let ((namestring (logical-namestring pathname)))
    (if (gethash namestring *circularity-check-table*) 
	(error "Circularity in translations for ~S." namestring)
	(setf (gethash namestring *circularity-check-table*) T))))

(defun map-logical-pathname (logical-pathname 
			     host
			     &optional (output-format *translation-output*))
  "Find and execute the first matching translation."
  (dolist (translation (logical-pathname-translations host))
    (let ((from-pathname (logical-pathname (car translation) host))
	  (to-pathname   (cadr translation)))
      (when (logical-pathname-match-p logical-pathname from-pathname)
	(return (translate-logical-pathname-aux logical-pathname
						from-pathname
						to-pathname
						output-format))))))

(defun logical-pathname-match-p (logical-pathname from-pathname)
  "Return T if the logical pathname matches the test pathname."
  (setq logical-pathname (logical-pathname logical-pathname)
	from-pathname (logical-pathname from-pathname))
  ;; ignore host. Match directories. Match name. Match type. Match version.
  (and (match-directories (logical-pathname-directory from-pathname)
			  (logical-pathname-directory logical-pathname))
       (match-wildcard-word (logical-pathname-name from-pathname)
			    (logical-pathname-name logical-pathname))
       (match-wildcard-word (logical-pathname-type from-pathname)
			    (logical-pathname-type logical-pathname))
       (match-wildcard-word (logical-pathname-version from-pathname)
			    (logical-pathname-version logical-pathname))))  

(defun translate-logical-pathname-aux (logical-pathname 
				       from-pathname to-pathname
				       &optional
				       (output-format *translation-output*))
  "Translates the logical pathname using the substitution specified by
   a particular translation."
  (let* ((host (physical-pathname-host to-pathname))
	 (host-type (host-type host))
	 (translation-rule (find-translation-rule host-type))
	 (char-map (translation-rule-char-mappings translation-rule))
	 (string-map (translation-rule-component-mappings translation-rule)))
    (let ((device (physical-pathname-device to-pathname))
	  (directories (map-directories
			(physical-pathname-directory logical-pathname)
			(physical-pathname-directory from-pathname)
			(physical-pathname-directory to-pathname)
			*null-vector* 0 0 0
			(choose-case translation-rule 'component)
			char-map string-map))
	  (name (map-wildcard-word (physical-pathname-name logical-pathname)
				   (physical-pathname-name from-pathname)
				   (physical-pathname-name to-pathname)
				   (choose-case translation-rule 'name)
				   char-map string-map))
	  (type (map-wildcard-word (physical-pathname-type logical-pathname)
				   (physical-pathname-type from-pathname)
				   (physical-pathname-type to-pathname)
				   (choose-case translation-rule 'type)
				   char-map string-map))
	  (version (map-wildcard-word (physical-pathname-version logical-pathname)
				      (physical-pathname-version from-pathname)
				      (physical-pathname-version to-pathname)
				      (choose-case translation-rule 'version)
				      char-map string-map)))
      (cond ((eq (pathname-host-type to-pathname) :logical)
	     (make-logical-pathname :host host 
				    :directory directories
				    :name name
				    :type type
				    :version version))
	    (t 
	     (convert-generic-pathname 
	      (make-physical-pathname :host host 
				      :device device
				      :directory directories
				      :name name
				      :type type
				      :version version)
	      output-format))))))

;;; ********************************
;;; Match and Map Wildcards ********
;;; ********************************
(defun wildcard-wordp (string)
  (find #\* string))

(defun must-match (thing)
  (or (eq thing :wild)
      (and (stringp thing)
	   (wildcard-wordp thing))))

(defun match-wildcard-word (template string)
  ;; "*" standalone (:wild) is treated differently from "*" within
  ;; a word.
  (or (eq template :wild)
      (null template)
      (and (stringp string) (stringp template)
	   (match-strings template string))
      ;; e.g., :absolute :absolute
      (eq template string)))

(defun match-strings (template string &optional (t-start 0) (s-start 0))
  (let* ((t-length (length template))
	 (s-length (length string))
	 (t-at-end (= t-length t-start))
	 (s-at-end (= s-length s-start)))
    (cond ((or t-at-end s-at-end) 	; if at end of template or string
	   (and t-at-end s-at-end))     ; both must be at the end.
	  ((char= #\* (char template t-start))
	   (or (match-strings template string (1+ t-start) s-start)
	       (match-strings template string t-start (1+ s-start))
	       (match-strings template string (1+ t-start) (1+ s-start))))
	  ((char-equal (char template t-start)
		  (char string s-start)) ; includes * against *
	   (match-strings template string (1+ t-start) (1+ s-start))))))

(defun match-directories (template dirs &optional (t-start 0) (d-start 0))
  (let* ((t-length (length template))
	 (d-length (length dirs))
	 (t-at-end (= t-length t-start))
	 (d-at-end (= d-length d-start)))
    (cond ((or t-at-end d-at-end)
	   (and t-at-end d-at-end))
	  ((eq (#+:cmu svref #-:cmu aref template t-start) :wild-inferiors)
	   ;; :wild-inferiors matches any number of components, including
	   ;; zero. First try skipping over the :wild-inferiors. If that fails,
	   ;; try matching against one component without skipping over the
	   ;; :wild-inferiors. Finally, try matching against one component
	   ;; while skipping over the :wild-inferiors (the latter really
	   ;; isn't necessary, since the first 2 cases include it).
	   (or (match-directories template dirs (1+ t-start) d-start)
	       (match-directories template dirs t-start (1+ d-start))
	       (match-directories template dirs (1+ t-start) (1+ d-start))))
	  ((match-wildcard-word (#+:cmu svref #-:cmu aref template t-start)
				(#+:cmu svref #-:cmu aref dirs d-start))
	   (match-directories template dirs (1+ t-start) (1+ d-start))))))

(defun map-wildcard-word (string source target 
				 &optional case char-mappings string-mappings)
  (let ((result
	 (cond ((and (stringp target)
		     (not (wildcard-wordp target)))
		;; If the target pattern does not contain *, copy the target
		;; pattern component literally to the target instance.
		target)
	       ((or (eq target :wild) (null target))
		;; If the target pattern is :wild, copy the source string
		;; component to the target string literally with no further
		;; analysis. This holds even for the type, which is 
		;; represented internally in terms of canonical types,
		;; and is "translated" when realized for the new host.
		string)
	       ((not (stringp target))
		target)
	       ((eq source :wild)
		(map-strings string string target))
	       (t (map-strings string source target)))))
    (when (stringp result)
      (setq result 
	    (casify (parallel-substitute char-mappings
					 (name-substitution string-mappings 
							    result))
		    case)))
    result))

(defun map-strings (string source target
			   &optional (result "")
			   (s-start 0) (st-start 0) (tt-start 0))
  (let* ((s-length (length string))
	 (st-length (length source))
	 (tt-length (length target))
	 (s-at-end (= s-length s-start))
	 (st-at-end (= st-length st-start))
	 (tt-at-end (= tt-length tt-start)))
    (cond ((and s-at-end (not (or st-at-end tt-at-end))
		(char= #\* (char target tt-start))
		(char= #\* (char source st-start)))
	   (map-strings string source target result 
			s-start (1+ st-start) (1+ tt-start)))
	  ((or s-at-end st-at-end)
	   ;; When not enough matching values are available due to too few
	   ;; * in the source pattern, use the null string as the matching
	   ;; value for any * remaining in the target.
	   (when (and s-at-end st-at-end)
	     (concatenate 'string 
			  result
			  (delete #\* (subseq target tt-start)))))
	  (tt-at-end
	   ;; When the source pattern has too many *, ignore the first
	   ;; extra * and everything following it.
	   result)
	  ((char= #\* (char target tt-start))
	   ;; Replace * in target pattern with the contents of the source
	   ;; string specified by the next * in the source pattern.
	   (cond ((char= #\* (char source st-start))
		  (or (map-strings string source target result 
				   s-start (1+ st-start) (1+ tt-start))
		      (map-strings string source target 
				   (concatenate 'string result
						(subseq string s-start 
							(1+ s-start)))
				   (1+ s-start) st-start tt-start)))
		 ((char-equal (char source st-start) ; was char=
			      (char string s-start))
		  (map-strings string source target result 
			       (1+ s-start) (1+ st-start) tt-start))))
	  (t;; copy literal strings as is from the target
	   (let ((next-* (position #\* target :start tt-start)))
	     (if next-*
		 (map-strings string source target
			      (concatenate 'string result
					   (subseq target tt-start next-*))
			      s-start st-start next-*)
		 (when (match-strings source string st-start s-start)
		   (concatenate 'string
				result (subseq target tt-start)))))))))

(defun map-directories (dirs source target 
			     &optional (result *null-vector*)
			     (d-start 0) (s-start 0) (t-start 0)
			     case char-map string-map)
  (let* ((d-length (length dirs))
	 (s-length (length source))
	 (t-length (length target))
	 (d-at-end (= d-length d-start))
	 (s-at-end (= s-length s-start))
	 (t-at-end (= t-length t-start)))
    (cond ((and d-at-end (not (or s-at-end t-at-end)) ; added 7/12/91 --mk
		(must-match (#+:cmu svref #-:cmu aref target t-start))
		(must-match (#+:cmu svref #-:cmu aref source s-start)))
	   (map-directories dirs source target result
			    d-start (1+ s-start) t-start
			    case char-map string-map))
	  ((or d-at-end s-at-end)
	   (when (and d-at-end s-at-end)
	     (concatenate 'simple-vector result
			  (map 'simple-vector 
			       #'(lambda (x) 
				   (map-wildcard-word 
				    "" "" x
				    case char-map string-map))
			       (delete :wild-inferiors 
				       (subseq target t-start))))))
	  (t-at-end
	   (when (match-directories source dirs s-start d-start)
	     result))
	  ((eq :wild-inferiors (#+:cmu svref #-:cmu aref target t-start))
	   (cond ((eq :wild-inferiors (#+:cmu svref
				       #-:cmu aref source s-start))
		  (or (map-directories dirs source target result
				       d-start (1+ s-start) (1+ t-start)
				       case char-map string-map)
		      (map-directories dirs source target 
				       (concatenate 'simple-vector result
						    (list (map-wildcard-word
							   (#+:cmu svref 
							    #-:cmu aref
							    dirs d-start)
							   :wild :wild
							   case char-map
							   string-map)))
				       (1+ d-start) s-start t-start
				       case char-map string-map)
		      (map-directories dirs source target 
				       (concatenate 'simple-vector result
						    (list (map-wildcard-word
							   (#+:cmu svref 
							    #-:cmu aref
							    dirs d-start)
							   :wild :wild
							   case char-map
							   string-map)))
				       (1+ d-start) (1+ s-start) (1+ t-start)
				       case char-map string-map)))
		 ((string-equal (#+:cmu svref #-:cmu aref dirs d-start)
				(#+:cmu svref #-:cmu aref source s-start))
		  (map-directories dirs source target result
				   (1+ d-start) (1+ s-start) t-start
				   case char-map string-map))))
	  ((must-match (#+:cmu svref #-:cmu aref target t-start))
	   (cond ((must-match (#+:cmu svref #-:cmu aref source s-start))
		  (map-directories dirs source target
				   (concatenate 'simple-vector result
						(list (map-wildcard-word 
						 (#+:cmu svref 
						  #-:cmu aref dirs d-start)
						 (#+:cmu svref
						  #-:cmu aref source s-start)
						 (#+:cmu svref
						  #-:cmu aref target t-start)
						 case char-map string-map)))
				   (1+ d-start) (1+ s-start) (1+ t-start)
				   case char-map string-map))
		 ((string-equal (#+:cmu svref #-:cmu aref dirs d-start)
				(#+:cmu svref #-:cmu aref source s-start))
		  (map-directories dirs source target result
				   (1+ d-start) (1+ s-start) t-start
				   case char-map string-map))))
	  (t
	   (map-directories dirs source target
			    (concatenate 'simple-vector result
					 (list
					  (map-wildcard-word
					   (#+:cmu svref 
						   #-:cmu aref target t-start)
					   :wild :wild
					   case char-map
					   string-map)))
			    d-start s-start (1+ t-start)
			    case char-map string-map)))))


;;; ********************************
;;; Common Lisp Redefinitions ******
;;; ********************************
;;; Not doing merge-pathnames or with-open-file. Parse-namestring not
;;; really done well.

;;; append-directories
(defun append-logical-directories (absolute-dir relative-dir)
  (when (or absolute-dir relative-dir)
    (setq absolute-dir (logical-pathname (or absolute-dir ""))
	  relative-dir (logical-pathname (or relative-dir "")))
    (logical-namestring 
     (make-logical-pathname
      :host (or (logical-pathname-host absolute-dir)
		(logical-pathname-host relative-dir))
      :directory (concatenate 'simple-vector
			      (logical-pathname-directory absolute-dir)
			      (cdr (coerce (logical-pathname-directory
					    relative-dir)
					   'list)))
      :name (or (logical-pathname-name absolute-dir)
		(logical-pathname-name relative-dir))
      :type (or (logical-pathname-type absolute-dir)
		(logical-pathname-type relative-dir))
      :version (or (logical-pathname-version absolute-dir)
		   (logical-pathname-version relative-dir))))))
	  
(eval-when (compile load eval)
(defun real-filename (filename)
  (if (and filename
	   (eq (pathname-host-type filename) :logical))
      (translate-logical-pathname filename :namestring)
      filename))

#|
(defmacro convert-file-function (name &optional optionalp)
  (let ((old-name (intern (concatenate 'string "OLD-" (string name)))))
    `(unless (fboundp ',old-name)
	     (setf (symbol-function ',old-name)(symbol-function ',name))
	     (setf (symbol-function ',name)
		   #'(lambda ,(if optionalp 
				  '(&optional filename &rest args)
				'(filename &rest args))
		       (apply #',old-name (real-filename filename) args))))))
|#

(defmacro convert-file-function (name &optional optionalp)
  (let ((old-name (intern (concatenate 'string "OLD-" (string name)))))
    `(unless (fboundp ',old-name)
       ;; Yes, some lisps will give compiler warnings about OLD-name
       ;; not being declared or defined as a function. But what can
       ;; we do, with most lisps not yet recognizing CLtL2's ftype
       ;; declaration?
       (setf (symbol-function ',old-name)(symbol-function ',name))
       (setf (symbol-function ',name)
	     #'(lambda ,(if optionalp 
			    '(&optional filename &rest args)
			  '(filename &rest args))
		 ,(if optionalp
		      `(if filename
			   (apply #',old-name (real-filename filename) args)
			 (,old-name)) ; instead of (funcall #',old-name)
		    `(apply #',old-name (real-filename filename) args)))))))

(defmacro convert-file-function-2-args (name)
  (let ((old-name (intern (concatenate 'string "OLD-" (string name)))))
    `(unless (fboundp ',old-name)
       (setf (symbol-function ',old-name)(symbol-function ',name))
       (setf (symbol-function ',name)
	     #'(lambda (filename1 filename2 &rest args)
		 (apply #',old-name
			(real-filename filename1)(real-filename filename2)
			args))))))
)

(convert-file-function lisp::load)
(convert-file-function lisp::open)
(convert-file-function lisp::probe-file)
(convert-file-function lisp::delete-file)
#-cormanlisp (convert-file-function lisp::truename)
(convert-file-function lisp::directory)
#-cormanlisp (convert-file-function lisp::dribble t)
#-cormanlisp (convert-file-function lisp::ed t)
#-cormanlisp (convert-file-function lisp::file-author)
(convert-file-function lisp::file-write-date)

#-cormanlisp (convert-file-function-2-args lisp::rename-file)
;; should take care of :output-file as well
(convert-file-function lisp::compile-file)			  

(unless (fboundp 'old-parse-namestring)
  (setf (symbol-function 'old-parse-namestring)
	(symbol-function 'lisp::parse-namestring))
  (defun lisp::parse-namestring (thing &optional host
				       (defaults *default-pathname-defaults*)
				       &key (start 0) end junk-allowed)
    "Convert THING (string, symbol, pathname, or stream) into a pathname."
    (declare (ignore junk-allowed))
    (cond ((null thing) nil) ; try to fix bug with (ed). probably not here.
	  ((or (eq (pathname-host-type thing) :logical)
	       (eq (pathname-host-type defaults) :logical)
	       (eq (host-type host) :logical))
	   ;; Tis a logical pathname
	   (parse-generic-namestring thing host defaults 
				     :start start :end end))
	  (t (if end
		 (funcall 'old-parse-namestring thing host defaults 
			  :start start :end end)
		 (funcall 'old-parse-namestring thing host defaults 
			  :start start))))))

#+cormanlisp
(provide 'lp)

#|
Corman Lisp Example:

(setf (physical-host-type "c") :win32)
(setf (logical-pathname-translations "ccl")
	  '(("*.*.*" "c:\\program files\\ccl\\")	
		("**;*.*.*" "c:\\program files\\ccl\\**\\")
		(";**;*.*.*" "c:\\program files\\ccl\\**\\")))
(translate-logical-pathname "ccl:documentation;credits.txt")

(probe-file "ccl:documentation;credits.txt")
(with-open-file (fs "ccl:test.txt" :direction :output)
   (write-line "blah" fs))

(with-open-file (fs "c:\\program files\\ccl\\test2.txt" :direction :output)
   (write-line "blah" fs))

(with-open-file (fs "ccl:documentation;credits.txt")
    (let ((x (make-string (file-length fs))))
       (read-sequence x fs)
       x))
|#

;;; *EOF*
