;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; publish.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;
;;
;; $Id: publish.cl,v 1.34 2000/08/10 00:57:32 jkf Exp $

;; Description:
;;   publishing urls

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-


(in-package :net.aserve)


(defclass entity ()
  ;; an object to be published
  ;; host and port may be nil, meaning "don't care", or a list of
  ;; items or just an item
  ((host 
    :initarg :host
	 :initform nil
	 :reader host)
   (port :initarg :port
	 :initform nil
	 :reader port)
   (path :initarg :path
	 :reader path)
   (location :initarg :location
	     :reader location)
   (prefix :initarg :prefix
	   :initform nil
	   :reader prefix)
   (last-modified :initarg :last-modified
		  :accessor last-modified
		  :initform nil ; means always considered new
		  )
   (format :initarg :format  ;; :text or :binary
	   :initform :text
	   :reader  entity-format)
   
   (content-type :initarg :content-type
		 :reader content-type
		 :initform nil)
   
   (authorizer  :initarg :authorizer  ; authorizer object, if any
		:accessor entity-authorizer
		:initform nil)
   )
  )


(defclass file-entity (entity)
  ;; a file to be published
  (
   (file  :initarg :file :reader file)
   (contents :initarg :contents :reader contents
	     :initform nil)
   (cache-p 
    ;; true if the contents should be cached when accessed
    :initarg :cache-p
    :initform nil
    :accessor cache-p)
     
   (ssi
    ;; true if we should look for server side includes when
    ;; accessing this file
    :initarg :ssi
    :initform nil
    :accessor ssi)
     
   (dependencies 
    ;; list of (filename . lastmodifiedtime) 
    ;; for each of the files that this file includes
    :initarg :dependencies
    :initform nil
    :accessor dependencies)
     
   )) 


(defclass computed-entity (entity)
  ;; entity computed each time it's called
  ((function :initarg :function :reader entity-function)))



(defclass directory-entity (entity)
  ;; entity that displays the contents of a directory
  ((directory :initarg :directory ; directory to display
	      :reader entity-directory)
   (prefix    :initarg :prefix   ; url prefix pointing to ths dir
	      :reader prefix
	      :initform "")
   (recurse   :initarg :recurse	   ; t to descend to sub directories
	      :initform nil
	      :reader recurse)
   
   (cache-p 
    ;; settting for file entities created:
    ;; true if the contents should be cached when accessed
    :initarg :cache-p
    :initform nil
    :accessor cache-p)   
    
   (ssi
    ;; settting for file entities created:
    ;; true if we should look for server side includes when
    ;; accessing this file
    :initarg :ssi
    :initform nil
    :accessor ssi)
   )
  )




;;-------- locators - objects which find the entity to return

(defclass locator ()
  ((name :initform :unnamed
	 :initarg :name
	 :reader  locator-name)

   ; info is where the locator will likely store data related
   ; to mapping
   (info :initform nil
	 :initarg :info
	 :accessor locator-info)
   ))


#-cormanlisp
(defclass locator-exact (locator)
  ;; used to map specific uri paths to entities
  ;; the table slot holds the hash table that's used
  ()
  (:default-initargs :info (make-hash-table :test #'equal)))

#+cormanlisp
(defclass locator-exact (locator)
  ;; used to map specific uri paths to entities
  ;; the table slot holds the hash table that's used
  ())

#+cormanlisp
(defmethod initialize-instance ((l locator-exact) &key info name)
	(call-next-method)
	(if info
	  (setf (locator-info l) info)
	  (setf (locator-info l) (make-hash-table :test #'equal)))
	(if name
	  (setf (slot-value l 'name) name)
	  (setf (slot-value l 'name) :unnamed)))


(defclass locator-prefix (locator)
  ;; use to map prefixes to entities
  ()
  )


;; the info slot of a locator-prefix class is a list of
;; prefix-handler objects, sorted by the length of the path
;; (from longest to smallest).
(defstruct (prefix-handler (:type list))
  path           ;; string which must be the prefix of the url part to match
  host-handlers  ;; list of host-handlers
  )

(defstruct (host-handler  (:type list))
  host	  ;; list of host names to match.  nil means match anything
  entity  ;; entity object to handle this request
  )







  









; we can specify either an exact url or one that handles all
; urls with a common prefix.
;
(defparameter *file-type-to-mime-type*
    ;; this list constructed by generate-mime-table in parse.cl
    '(("application/EDI-Consent") ("application/EDI-X12") ("application/EDIFACT")
      ("application/activemessage") ("application/andrew-inset" "ez")
      ("application/applefile") ("application/atomicmail")
      ("application/cals-1840") ("application/commonground")
      ("application/cybercash") ("application/dca-rft") ("application/dec-dx")
      ("application/eshop") ("application/hyperstudio") ("application/iges")
      ("application/mac-binhex40" "hqx") ("application/mac-compactpro" "cpt")
      ("application/macwriteii") ("application/marc") ("application/mathematica")
      ("application/msword" "doc") ("application/news-message-id")
      ("application/news-transmission")
      ("application/octet-stream" "bin" "dms" "lha" "lzh" "exe" "class")
      ("application/oda" "oda") ("application/pdf" "pdf")
      ("application/pgp-encrypted") ("application/pgp-keys")
      ("application/pgp-signature") ("application/pkcs10")
      ("application/pkcs7-mime") ("application/pkcs7-signature")
      ("application/postscript" "ai" "eps" "ps")
      ("application/prs.alvestrand.titrax-sheet") ("application/prs.cww")
      ("application/prs.nprend") ("application/remote-printing")
      ("application/riscos") ("application/rtf" "rtf") ("application/set-payment")
      ("application/set-payment-initiation") ("application/set-registration")
      ("application/set-registration-initiation") ("application/sgml")
      ("application/sgml-open-catalog") ("application/slate")
      ("application/smil" "smi" "smil") ("application/vemmi")
      ("application/vnd.3M.Post-it-Notes") ("application/vnd.FloGraphIt")
      ("application/vnd.acucobol")
      ("application/vnd.anser-web-certificate-issue-initiation")
      ("application/vnd.anser-web-funds-transfer-initiation")
      ("application/vnd.audiograph") ("application/vnd.businessobjects")
      ("application/vnd.claymore") ("application/vnd.comsocaller")
      ("application/vnd.dna") ("application/vnd.dxr")
      ("application/vnd.ecdis-update") ("application/vnd.ecowin.chart")
      ("application/vnd.ecowin.filerequest") ("application/vnd.ecowin.fileupdate")
      ("application/vnd.ecowin.series") ("application/vnd.ecowin.seriesrequest")
      ("application/vnd.ecowin.seriesupdate") ("application/vnd.enliven")
      ("application/vnd.epson.salt") ("application/vnd.fdf")
      ("application/vnd.ffsns") ("application/vnd.framemaker")
      ("application/vnd.fujitsu.oasys") ("application/vnd.fujitsu.oasys2")
      ("application/vnd.fujitsu.oasys3") ("application/vnd.fujitsu.oasysgp")
      ("application/vnd.fujitsu.oasysprs") ("application/vnd.fujixerox.docuworks")
      ("application/vnd.hp-HPGL") ("application/vnd.hp-PCL")
      ("application/vnd.hp-PCLXL") ("application/vnd.hp-hps")
      ("application/vnd.ibm.MiniPay") ("application/vnd.ibm.modcap")
      ("application/vnd.intercon.formnet") ("application/vnd.intertrust.digibox")
      ("application/vnd.intertrust.nncp") ("application/vnd.is-xpr")
      ("application/vnd.japannet-directory-service")
      ("application/vnd.japannet-jpnstore-wakeup")
      ("application/vnd.japannet-payment-wakeup")
      ("application/vnd.japannet-registration")
      ("application/vnd.japannet-registration-wakeup")
      ("application/vnd.japannet-setstore-wakeup")
      ("application/vnd.japannet-verification")
      ("application/vnd.japannet-verification-wakeup") ("application/vnd.koan")
      ("application/vnd.lotus-1-2-3") ("application/vnd.lotus-approach")
      ("application/vnd.lotus-freelance") ("application/vnd.lotus-organizer")
      ("application/vnd.lotus-screencam") ("application/vnd.lotus-wordpro")
      ("application/vnd.meridian-slingshot") ("application/vnd.mif" "mif")
      ("application/vnd.minisoft-hp3000-save")
      ("application/vnd.mitsubishi.misty-guard.trustweb")
      ("application/vnd.ms-artgalry") ("application/vnd.ms-asf")
      ("application/vnd.ms-excel") ("application/vnd.ms-powerpoint" "ppt")
      ("application/vnd.ms-project") ("application/vnd.ms-tnef")
      ("application/vnd.ms-works") ("application/vnd.music-niff")
      ("application/vnd.musician") ("application/vnd.netfpx")
      ("application/vnd.noblenet-directory") ("application/vnd.noblenet-sealer")
      ("application/vnd.noblenet-web") ("application/vnd.novadigm.EDM")
      ("application/vnd.novadigm.EDX") ("application/vnd.novadigm.EXT")
      ("application/vnd.osa.netdeploy") ("application/vnd.powerbuilder6")
      ("application/vnd.powerbuilder6-s") ("application/vnd.rapid")
      ("application/vnd.seemail") ("application/vnd.shana.informed.formtemplate")
      ("application/vnd.shana.informed.interchange")
      ("application/vnd.shana.informed.package") ("application/vnd.street-stream")
      ("application/vnd.svd") ("application/vnd.swiftview-ics")
      ("application/vnd.truedoc") ("application/vnd.visio")
      ("application/vnd.webturbo") ("application/vnd.wrq-hp3000-labelled")
      ("application/vnd.wt.stf") ("application/vnd.xara")
      ("application/vnd.yellowriver-custom-menu") ("application/wita")
      ("application/wordperfect5.1") ("application/x-bcpio" "bcpio")
      ("application/x-cdlink" "vcd") ("application/x-chess-pgn" "pgn")
      ("application/x-compress") ("application/x-cpio" "cpio")
      ("application/x-csh" "csh") ("application/x-director" "dcr" "dir" "dxr")
      ("application/x-dvi" "dvi") ("application/x-futuresplash" "spl")
      ("application/x-gtar" "gtar") ("application/x-gzip")
      ("application/x-hdf" "hdf") ("application/x-javascript" "js")
      ("application/x-koan" "skp" "skd" "skt" "skm")
      ("application/x-latex" "latex") ("application/x-netcdf" "nc" "cdf")
      ("application/x-rpm" "rpm") ("application/x-sh" "sh")
      ("application/x-shar" "shar") ("application/x-shockwave-flash" "swf")
      ("application/x-stuffit" "sit") ("application/x-sv4cpio" "sv4cpio")
      ("application/x-sv4crc" "sv4crc") ("application/x-tar" "tar")
      ("application/x-tcl" "tcl") ("application/x-tex" "tex")
      ("application/x-texinfo" "texinfo" "texi")
      ("application/x-troff" "t" "tr" "roff") ("application/x-troff-man" "man")
      ("application/x-troff-me" "me") ("application/x-troff-ms" "ms")
      ("application/x-ustar" "ustar") ("application/x-wais-source" "src")
      ("application/x400-bp") ("application/xml") ("application/zip" "zip")
      ("audio/32kadpcm") ("audio/basic" "au" "snd")
      ("audio/midi" "mid" "midi" "kar") ("audio/mpeg" "mpga" "mp2" "mp3")
      ("audio/vnd.qcelp") ("audio/x-aiff" "aif" "aiff" "aifc")
      ("audio/x-pn-realaudio" "ram" "rm") ("audio/x-realaudio" "ra")
      ("audio/x-wav" "wav") ("chemical/x-pdb" "pdb" "xyz") ("image/cgm")
      ("image/g3fax") ("image/gif" "gif") ("image/ief" "ief")
      ("image/jpeg" "jpeg" "jpg" "jpe") ("image/naplps") ("image/png" "png")
      ("image/prs.btif") ("image/tiff" "tiff" "tif") ("image/vnd.dwg")
      ("image/vnd.dxf") ("image/vnd.fpx") ("image/vnd.net-fpx") ("image/vnd.svf")
      ("image/vnd.xiff") ("image/x-cmu-raster" "ras")
      ("image/x-portable-anymap" "pnm") ("image/x-portable-bitmap" "pbm")
      ("image/x-portable-graymap" "pgm") ("image/x-portable-pixmap" "ppm")
      ("image/x-rgb" "rgb") ("image/x-xbitmap" "xbm") ("image/x-xpixmap" "xpm")
      ("image/x-xwindowdump" "xwd") ("message/delivery-status")
      ("message/disposition-notification") ("message/external-body")
      ("message/http") ("message/news") ("message/partial") ("message/rfc822")
      ("model/iges" "igs" "iges") ("model/mesh" "msh" "mesh" "silo")
      ("model/vnd.dwf") ("model/vrml" "wrl" "vrml") ("multipart/alternative")
      ("multipart/appledouble") ("multipart/byteranges") ("multipart/digest")
      ("multipart/encrypted") ("multipart/form-data") ("multipart/header-set")
      ("multipart/mixed") ("multipart/parallel") ("multipart/related")
      ("multipart/report") ("multipart/signed") ("multipart/voice-message")
      ("text/css" "css") ("text/directory") ("text/enriched")
      ("text/plain" "asc" "txt") ("text/prs.lines.tag") ("text/rfc822-headers")
      ("text/richtext" "rtx") ("text/rtf" "rtf") ("text/sgml" "sgml" "sgm")
      ("text/tab-separated-values" "tsv") ("text/uri-list") ("text/vnd.abc")
      ("text/vnd.flatland.3dml") ("text/vnd.fmi.flexstor") ("text/vnd.in3d.3dml")
      ("text/vnd.in3d.spot") ("text/vnd.latex-z") ("text/x-setext" "etx")
      ("text/xml" "xml") ("video/mpeg" "mpeg" "mpg" "mpe")
      ("video/quicktime" "qt" "mov") ("video/vnd.motorola.video")
      ("video/vnd.motorola.videop") ("video/vnd.vivo") ("video/x-msvideo" "avi")
      ("video/x-sgi-movie" "movie") ("x-conference/x-cooltalk" "ice")
      ("text/html" "html" "htm")))

(defvar *mime-types* nil)

(defun build-mime-types-table ()
  (if* (null *mime-types*)
     then (setf *mime-types* (make-hash-table :test #'equalp))
	  (dolist (ent *file-type-to-mime-type*)
	    (dolist (type (cdr ent))
	      (setf (gethash type *mime-types*) (car ent))))))
  

(build-mime-types-table)  ;; build the table now


(defun unpublish (&key all)
  (if* all
     then (dolist (locator (wserver-locators *wserver*))
	    (unpublish-locator locator))
     else (error "not done yet")))

  
;; methods on entity objects

;-- content-length -- how long is the body of the response, if we know

(defmethod content-length ((ent entity))
  ;; by default we don't know, and that's what nil means
  nil)

(defmethod content-length ((ent file-entity))
  (let ((contents (contents ent)))
    (if* contents
       then (length contents)
       else ; may be a file on the disk, we could
	    ; compute it.. this is
	    ;** to be done
	    nil)))



;- transfer-mode - will the body be sent in :text or :binary mode.
;  use :binary if you're not sure

(defmethod transfer-mode ((ent entity))
  (or (entity-format ent) :binary)
  )








  


;; url exporting








(defun publish (&key (host nil host-p) port path function class format
		     content-type
		     (server *wserver*)
		     locator
		     remove
		     authorizer
		     )
  ;; publish the given url
  ;; if file is given then it specifies a file to return
  ;; 
  (let (hval)
    (if* (null locator) 
       then (setq locator (find-locator :exact server)))

    (if* remove
       then ; eliminate the entity if it exists
	    (unpublish-entity locator path host host-p)
       else
	     
	    (let ((ent (make-instance (or class 'computed-entity)
			 :host (setq hval (if* host
					     then (if* (atom host)
						     then (list host)
						     else host)))
			 :port port
			 :path path
			 :function function
			 :format format
			 :content-type content-type
			 :authorizer authorizer)))
	      (publish-entity ent locator path hval)))))

	     

(defun publish-file (&key (server *wserver*)
			  locator
			  (host nil host-p) 
			  port path
			  file content-type class preload
			  cache-p ssi 
			  remove
			  authorizer)
  ;; return the given file as the value of the url
  ;; for the given host.
  ;; If host is nil then return for any host
  
  (if* (null locator) 
     then (setq locator (find-locator :exact server)))

  (if* remove
     then (unpublish-entity locator path
			    host
			    host-p)
	  (return-from publish-file nil))
  
  
  (let (ent got hval
	(c-type (or content-type
		    (gethash (pathname-type (pathname file))
			     *mime-types*)
		    "application/octet-stream")))
    (if* preload
       then ; keep the content in core for fast display
	    (with-open-file (p file :element-type #-cormanlisp '(unsigned-byte 8) #+cormanlisp 'unsigned-byte)
	      (let ((size (excl::filesys-size (stream-input-fn p)))
		    (lastmod (excl::filesys-write-date (stream-input-fn p)))
		    (guts))
		(setq guts (make-array size :element-type #-cormanlisp '(unsigned-byte 8) #+cormanlisp 'unsigned-byte))
	      
		(if* (not (eql size (setq got (read-sequence guts p))))
		   then (error "~s should have been ~d bytes but was ~d"
			       file
			       size
			       got))
		(setq ent (make-instance (or class 'file-entity)
			    :host (setq hval (if* host 
						then (if* (atom host)
							then (list host)
							else host)))
			    :port port
			    :path path
			    :file file
			    :content-type c-type
			    
			    :contents  guts
			    :last-modified lastmod
			    :cache-p cache-p
			    :ssi     ssi
			    :authorizer authorizer
			    ))))
       else (setq ent (make-instance (or class 'file-entity)
			:host (setq hval (if* host 
					    then (if* (atom host)
						    then (list host)
						    else host)))
			:port port
			:path path
			:file file
			:content-type c-type
			:cache-p cache-p
			:ssi ssi
			:authorizer authorizer
			)))

    (publish-entity ent locator path hval)))







(defun publish-directory (&key prefix 
			       (host nil host-p)
			       port
			       destination
			       (server *wserver*)
			       locator
			       remove
			       authorizer
			       )
  
  ;; make a whole directory available
  
  (if* (null locator) 
     then (setq locator (find-locator :prefix server)))

  (if* (and host (atom host))
     then (setq host (list host)))
  
  (let ((ent (make-instance 'directory-entity 
		       :directory destination
		       :prefix prefix
		       :host host
		       :port port
		       :authorizer authorizer
		       )))
    
  (dolist (entpair (locator-info locator))
    (if* (equal (prefix-handler-path entpair) prefix)
       then ; match, prefix
	    (if* (and remove (not host-p))
	       then ; remove all entries for all hosts
		    (setf (locator-info locator)
		      (remove entpair (locator-info locator)))
		    (return-from publish-directory nil))
	    
	    ; scan for particular host
	    (dolist (hostpair (prefix-handler-host-handlers entpair))
	      (if* (null (set-exclusive-or (host-handler-host hostpair) 
					   host
					   :test #'equalp))
		 then ; match existing one
		      (if* remove
			 then ; make it go away
			      (setf (prefix-handler-host-handlers entpair)
				(remove hostpair 
					(prefix-handler-host-handlers 
					 entpair)))
			 else (setf (host-handler-entity hostpair) ent))
		      (return-from publish-directory ent)))
	    
	    ; no match, must add it
	    (if* remove 
	       then ; no work to do
		    (return-from publish-directory nil))
		    
	    (if* (null host)
	       then ; add at end
		    (setf (prefix-handler-host-handlers entpair)
		      (append (prefix-handler-host-handlers entpair) 
			      (list (make-host-handler :host host 
						       :entity ent))))
	       else ; add at beginning
		    (setf (prefix-handler-host-handlers entpair)
		      (cons (make-host-handler :host host :entity ent) 
			    (prefix-handler-host-handlers entpair))))
	    (return-from publish-directory ent)))

  ; prefix not present, must add.
  ; keep prefixes in order, with max length first, so we match
  ; more specific before less specific
  
  (if* remove 
     then ; no work to do
	  (return-from publish-directory nil))
  
  (let ((len (length prefix))
	(list (locator-info locator))
	(new-ent (make-prefix-handler
		     :path prefix
		     :host-handlers (list (make-host-handler :host host 
					      :entity ent)))))
    (if* (null list)
       then ; this is the first
	    (setf (locator-info locator) (list new-ent))
     elseif (>= len
		(length (caar list)))
       then ; this one should preceed all other ones
	    (setf (locator-info locator) (cons new-ent list))
       else ; must fit somewhere in the list
	    (do* ((back list (cdr back))
		  (cur  (cdr back) (cdr cur)))
		((null cur)
		 ; put at end
		 (setf (cdr back) `((,prefix ((,host ,ent))))))
	      (if* (>= len (length (caar cur)))
		 then (setf (cdr back)
			`(,new-ent ,@cur))
		      (return)))))
  
  ent
  ))
		 


(defmethod publish-entity ((ent entity) 
			   (locator locator-exact)
			   path
			   host)
  ;; handle the tricky case of putting an entity in hash
  ;; table of a locator-exact.
  ;; We have to store a list of entities for each path due
  ;; to virtual hosts.  We always store the no host specified
  ;; case last (if there is one).
  ;;
  (let ((ents (gethash path (locator-info locator))))
    ;; must replace entry with matching host parameter
    (if* (null ents)
       then ; nothing for this path yet, just store
	    ; this one
	    (setq ents (list ent))
       else (if* (null host)
	       then ; no host specified, if there's a matching
		    ; entity, it will be the last
		    (let ((lents (last ents)))
		      (if* (null (host (car lents)))
			 then ; is a null one, just blast it
			      (setf (car lents) ent)
			 else ; not null, so add to end
			      (nconc ents (list ent))))
	       else ; entity specifies a host, must look for
		    ; a match
		    (do ((xents ents (cdr xents)))
			((null xents)
			 ; no match, add to the front
			 (setq ents (cons ent ents)))
		      (if* (null (set-exclusive-or 
				  host
				  (host (car xents))
				  :test #'equalp))
			 then ; match
			      (setf (car xents) ent)
			      (return)))))
		      
    (setf (gethash path (locator-info locator)) ents))
  
  ent)


(defmethod unpublish-entity ((locator locator-exact)
			     path
			     host
			     host-p)
  ;; remove any entities matching the host and path.
  ;; if host-p is nil then remove all entities, don't match the host
  (let ((ents (gethash path (locator-info locator))))
    (if* ents
       then (if* host-p
	       then ; must patch the hosts
		    
		    ; ensure that host is a list
		    (if* (and host (atom host))
		       then (setq host (list host)))
		    
		    (let (res)
		      (dolist (ent ents)
			(if* (set-exclusive-or host
					       (host ent)
					       :test #'equalp)
			   then ; no match
				(push ent res)))
		      (setf (gethash path (locator-info locator))
			(nreverse res)))
	       else ; throw away everything
		    (remhash path (locator-info locator))))))

				

(defmethod handle-request ((req http-request))
  (dolist (locator (wserver-locators *wserver*))
    (let ((ent (standard-locator req locator)))
      (if* ent
	 then ; check if it is authorized
	      (let ((authorizer (entity-authorizer ent)))
		(if* authorizer
		   then (let ((result (authorize authorizer req ent)))
			  (if* (eq result t)
			     then (if* (process-entity req ent)
				     then (return-from handle-request))
			   elseif (eq result :done)
			     then ; already responsed
				  (return-from handle-request nil)
			   elseif (eq result :deny)
			     then ; indicate denied request
				  (denied-request req)
				  (return-from handle-request nil))
			  ; the nil case falls through and will return
			  ; failed request
			  )
		   else ; no authorizer, let anyone access
			(if* (process-entity req ent)
			   then (return-from handle-request))
			)))))
  
  ; no handler
  (failed-request req)
		       
  )

(defmethod failed-request ((req http-request))
  ;; generate a response to a request that we can't handle
  (let ((entity (wserver-invalid-request *wserver*)))
    (if* (null entity)
       then (setq entity (make-instance 'computed-entity
			   :function #'(lambda (req ent)
					 (with-http-response 
					     (req ent
						  :response *response-not-found*)
					   (with-http-body (req ent)
					     (html 
					      (:head (:title "404 - NotFound")
						     (:body
						      (:h1 "Not Found")
						      "The request for "
						      (:princ-safe 
						       (render-uri 
							(request-uri req)
							nil
							))
						      " was not found on this server."))))))
			   :content-type "text/html"))
	    (setf (wserver-invalid-request *wserver*) entity))
    (process-entity req entity)))

(defmethod denied-request ((req http-request))
  ;; generate a response to a request that we can't handle
  (let ((entity (wserver-denied-request *wserver*)))
    (if* (null entity)
       then (setq entity 
	      (make-instance 'computed-entity
		:function #'(lambda (req ent)
			      (with-http-response 
				  (req ent
				       :response *response-not-found*)
				(with-http-body (req ent)
				  (html 
				   (:head (:title "404 - NotFound")
					  (:body
					   (:h1 "Not Found")
					   "The request for "
					   (:princ-safe 
					    (render-uri 
					     (request-uri req)
					     nil
					     ))
					   " was denied."))))))
		:content-type "text/html"))
	    (setf (wserver-denied-request *wserver*) entity))
    (process-entity req entity)))


(defmethod standard-locator ((req http-request)
			     (locator locator-exact))
  ;; standard function for finding an entity in an exact locator
  ;; return the entity if one is found, else return nil
  (let ((entities (gethash (uri-path (request-uri req))
			   (locator-info locator))))
    (dolist (entity entities)
      (let ((entity-host (host entity)))
	(if* entity-host
	   then ; must do a host match
		(let ((req-host (header-slot-value req "host")))
		  (if* req-host 
		     then ; name may be foo.com:8000
			  ; need to just use the foo.com part:
			  (setq req-host (car (split-on-character 
					       req-host
					       #\:)))
				
			  (if* (member req-host entity-host
				       :test #'equalp)
			     then (return entity))
		     else ; no host given, don't do it
			  nil))
	   else ; no host specified in entity, so do it
		(return entity))))))

(defmethod standard-locator ((req http-request)
			     (locator locator-prefix))
  ;; standard function for finding an entity in an exact locator
  ;; return the entity if one is found, else return nil
  (let* ((url (uri-path (request-uri req)))
	 (len-url (length url))
	 (req-host (header-slot-value req "host")))
    
    (setq req-host (car (split-on-character req-host #\:)))
	     
    (dolist (entpair (locator-info locator))
      (if* (and (>= len-url (length (prefix-handler-path entpair)))
		(buffer-match url 0 (prefix-handler-path entpair)))
	 then ; we may already be a wiener
	      (dolist (host-h (prefix-handler-host-handlers entpair))
		(let ((host (host-handler-host host-h)))
		  (if* host
		     then ; host specified, must match it
			  (if* req-host
			     then ; host passed in 
				  (if* (member req-host host
					       :test #'equalp)
				     then (return-from standard-locator
					    (host-handler-entity
					     host-h)))
			     else ; no host passed in, can't work
				  nil)
		     else ; no host specified, it always wins
			  (return-from standard-locator
			    (host-handler-entity host-h)))))))))
					  
  

(defun find-locator (name wserver)
  ;; give the locator with the given name
  (dolist (locator (wserver-locators wserver)
	    (error "no such locator as ~s" name))
    (if* (eq name (locator-name locator))
       then (return locator))))


(defmethod unpublish-locator ((locator locator-exact))
  (clrhash (locator-info locator)))

(defmethod unpublish-locator ((locator locator-prefix))
  (setf (locator-info locator) nil))





	  
				



(defmethod process-entity ((req http-request) (entity computed-entity))
  ;; 
  (let ((fcn (entity-function entity)))
    (funcall fcn req entity)
    t	; processed
    ))




       
  
(defmethod process-entity ((req http-request) (ent file-entity))
    
  (let ((contents (contents ent)))
    (if* contents
       then ;(preloaded)
	    ; set the response code and 
	    ; and header fields then dump the value
	      
	    ; * should check for range here
	    ; for now we'll send it all
	    (with-http-response (req ent
				     :content-type (content-type ent))
	      (setf (request-reply-content-length req) (length contents))
	      (push (cons "Last-Modified"
			  (universal-time-to-date 
			   (min (request-reply-date req) 
				(last-modified ent))))
		    (request-reply-headers req))
	      
	      (with-http-body (req ent :format :binary)
		;; at this point the header are out and we have a stream
		;; to write to 
		#-cormanlisp (write-sequence contents (request-reply-stream req))
	    #+cormanlisp (progn 
	(let ((stream (request-reply-stream req)))
	                    (dotimes (index (length contents))
				          (write-char (ccl:int-char (elt contents index)) stream))
	                    ))
		))
       else ; the non-preloaded case
	    (let (p)
	      
	      (setf (last-modified ent) nil) ; forget previous cached value
	      
	      (if* (null (errorset 
			  (setq p (open (file ent) 
					:direction :input
					:element-type #-cormanlisp '(unsigned-byte 8) #+cormanlisp 'unsigned-byte))))
		 then ; file not readable
		      
		      (return-from process-entity nil))
	      
	      (unwind-protect 
		  (progn
		    (let ((size (excl::filesys-size (stream-input-fn p)))
			  (lastmod (excl::filesys-write-date 
				    (stream-input-fn p)))
			  (buffer (make-array 1024 
					      :element-type #+cormanlisp 'unsigned-byte #-cormanlisp '(unsigned-byte 8))))
		      (declare (dynamic-extent buffer))
		      
		      (setf (last-modified ent) lastmod)
		      
		      (with-http-response (req ent)

			;; control will not reach here if the request
			;; included an if-modified-since line and if
			;; the lastmod value we just calculated shows
			;; that the file hasn't changed since the browser
			;; last grabbed it.
			
			(setf (request-reply-content-length req) size)
			(push (cons "Last-Modified"
				    (universal-time-to-date 
				     (min (request-reply-date req) lastmod)))
			      (request-reply-headers req))
			
			
			(with-http-body (req ent :format :binary)
			  (loop
			    (if* (<= size 0) then (return))
			    (let ((got (read-sequence buffer 
						      p :end 
						      (min size 1024))))
			      (if* (<= got 0) then (return))
		#-cormanlisp (write-sequence buffer (request-reply-stream req)
					      :end got)
	    #+cormanlisp (progn 
	(let ((stream (request-reply-stream req)))
	                    (dotimes (index got)
				          (write-char (ccl:int-char (elt buffer index)) stream))
	                    ))
											
			      
			      (decf size got)))))))
		      
		      
		
		(close p))))
    
    t	; we've handled it
    ))

	      
		
(defmethod process-entity ((req http-request) (ent directory-entity))
  ;; search for a file in the directory and then create a file
  ;; entity for it so we can track last modified and stu
  
  ; remove the prefix and tack and append to the given directory
  
  (let* ((postfix nil)
	 (realname (concatenate 'string
		     (entity-directory ent)
		     (setq postfix (subseq (uri-path (request-uri req))
					   (length (prefix ent))))))
	 (newname))
    (debug-format :info "directory request for ~s~%" realname)
    
    ; we can't allow the brower to specify a url with 
    ; any ..'s in it as that would allow the browser to 
    ; search outside the tree that's been published
    (if* (match-regexp "\\.\\.[\\/]" postfix)
       then ; contains ../ or ..\  
	    ; ok, it could be valid, like foo../, but that's unlikely
	    (return-from process-entity nil))
    
    
    (let ((type (excl::filesys-type realname)))
      (if* (null type)
	 then ; not present
	      (return-from process-entity nil)
       elseif (eq :directory type)
	 then ; we have to try index.html and index.htm
	      (if* (not (eq #\/ (schar realname (1- (length realname)))))
		 then (setq realname (concatenate 'string realname "/")))
	      
	      (if* (eq :file (excl::filesys-type
			      (setq newname
				(concatenate 'string realname "index.html"))))
		 then (setq realname newname)
	       elseif (eq :file (excl::filesys-type
				 (setq newname
				   (concatenate 'string realname "index.htm"))))
		 then (setq realname newname)
		 else ; failure
		      (return-from process-entity nil))
       elseif (not (eq :file type))
	 then  ; bizarre object
	      (return-from process-entity nil)))
    
    ;; ok realname is a file.
    ;; create an entity object for it, publish it, and dispatch on it
    
      
    (process-entity req (publish-file :path (uri-path 
					     (request-uri req))
				      :file realname
				      :authorizer (entity-authorizer ent)
				      ))
      
    t))
    
     
      
		      
		      
	      
	      
		    
  
  

		
(defun up-to-date-check (doit req ent)
  ;; if doit is true and the request req has an
  ;; if-modified-since or if-unmodified-since then
  ;; check if it applies and this resuits in a response
  ;; we can return right away then do it and 
  ;; throw to abort the rest of the body being run
  
  ; to be done
  
  (if* (not doit)
     then ; we dont' even care
	  (return-from up-to-date-check nil))
  
  (let ((if-modified-since (header-slot-value req "if-modified-since")))
    (if* if-modified-since
       then (setq if-modified-since
	      (date-to-universal-time if-modified-since)))
    
    (if* if-modified-since
       then ; valid date, do the check
	    (if* (and (last-modified ent) 
		      (<= (last-modified ent) if-modified-since))
	       then ; send back a message that it is already
		    ; up to date
		    (debug-format :info "entity is up to date~%")
		    (setf (request-reply-code req) *response-not-modified*)
		    (with-http-body (req ent)
		      ;; force out the header
		      )
		    (throw 'with-http-response nil) ; and quick exit
		    ))))

    


(defmethod compute-strategy ((req http-request) (ent entity))
  ;; determine how we'll respond to this request
  
  (let ((strategy nil)
	(keep-alive-possible
	 (and (wserver-enable-keep-alive *wserver*)
		      (>= (wserver-free-workers *wserver*) 2)
		      (header-value-member "keep-alive" 
			      (header-slot-value req "connection" )))))
    (if* (eq (request-method req) :head)
       then ; head commands are particularly easy to reply to
	    (setq strategy '(:use-socket-stream
			     :omit-body))
	    
	    (if* keep-alive-possible
	       then (push :keep-alive strategy))
	    
     elseif (and  ;; assert: get command
	     (wserver-enable-chunking *wserver*)
	     (eq (request-protocol req) :http/1.1)
	     (null (content-length ent)))
       then ;; http/1.1 so we can chunk
	    (if* keep-alive-possible
	       then (setq strategy '(:keep-alive :chunked :use-socket-stream))
	       else (setq strategy '(:chunked :use-socket-stream)))
       else ; can't chunk, let's see if keep alive is requested
	    (if* keep-alive-possible
	       then ; a keep alive is requested..
		    ; we may want reject this if we are running
		    ; short of processes to handle requests.
		    ; For now we'll accept it if we can.
		    
		    (if* (eq (transfer-mode ent) :binary)
		       then ; can't create binary stream string
			    ; must not keep alive
			    (setq strategy
			      '(:use-socket-stream
				; no keep alive
				))
		       else ; can build string stream
			    (setq strategy
			      '(:string-output-stream
				:keep-alive
				:post-headers)))
	       else ; keep alive not requested
		    (setq strategy '(:use-socket-stream
				     ))))
    
    ;;  save it

    (debug-format :info "strategy is ~s~%" strategy)
    (setf (request-reply-strategy req) strategy)
    
    ))
			     
		    
(defmethod compute-strategy ((req http-request) (ent file-entity))
  ;; for files we can always use the socket stream and keep alive
  ;; since we konw the file length ahead of time
  
  (let ((keep-alive (and (wserver-enable-keep-alive *wserver*)
			 (>= (wserver-free-workers *wserver*) 2)
			 (equalp "keep-alive" 
				 (header-slot-value req "connection"))))
	(strategy))
    
    (if*  (eq (request-method req) :get)
       then (setq strategy (if* keep-alive
			      then '(:use-socket-stream :keep-alive)
			      else '(:use-socket-stream)))
       else (setq strategy (call-next-method)))
    
    (debug-format :info "file strategy is ~s~%" strategy)
    (setf (request-reply-strategy req) strategy)))

	    
	    
  
		    
		    
    
    
	    

(defmethod send-response-headers ((req http-request) (ent entity) time)
  ;;
  ;; called twice (from with-http-body) in the generation of a response 
  ;; to an http request
  ;; 1. before the body forms are run.  in this case time eq :pre
  ;; 2. after the body forms are run.  in this case  time eq :post
  ;;
  ;; we send the headers out at the time appropriate to the 
  ;; strategy.  We also deal with a body written to a
  ;; string output stream
  ;;
    
  (mp:with-timeout (60 (logmess "timeout during header send")
		       ;;(setf (request-reply-keep-alive req) nil)
		       (throw 'with-http-response nil))
    (let* ((sock (request-socket req))
	   (strategy (request-reply-strategy req))
	   (post-headers (member :post-headers strategy :test #'eq))
	   (content)
	   (chunked-p (member :chunked strategy :test #'eq))
	   (code (request-reply-code req))
	   (send-headers
	    (if* post-headers
	       then (eq time :post)
	       else (eq time :pre))
	    )
	   (*print-pretty* nil))
      
      
      
      (if* send-headers
	 then (format-dif :xmit sock "~a ~d  ~a~a"
			  (request-reply-protocol-string req)
			  (response-number code)
			  (response-desc   code)
			  *crlf*))
      
      (if* (and post-headers
		(eq time :post)
		(member :string-output-stream strategy :test #'eq))
	 then ; must get data to send from the string output stream
	      (setq content (get-output-stream-string 
			     (request-reply-stream req)))
	      (setf (request-reply-content-length req) (length content)))
      	
      (if* (and send-headers
		(not (eq (request-protocol req) :http/0.9)))
	 then ; can put out headers
	      (format-dif :xmit sock "Date: ~a~a" 
			  (maybe-universal-time-to-date (request-reply-date req))
			  *crlf*)

	      (if* (member :keep-alive strategy :test #'eq)
		 then (format-dif :xmit
				  sock "Connection: Keep-Alive~aKeep-Alive: timeout=~d~a"
				  *crlf*
				  *read-request-timeout*
				  *crlf*)
		 else (format-dif :xmit sock "Connection: Close~a" *crlf*))
      
	      (format-dif :xmit sock "Server: AllegroServe/~a~a" 
			  *aserve-version-string*
			  *crlf*)
      
	      (if* (request-reply-content-type req)
		 then (format-dif :xmit
				  sock "Content-Type: ~a~a" 
				  (request-reply-content-type req)
				  *crlf*))

	      (if* chunked-p
		 then (format-dif :xmit
				  sock "Transfer-Encoding: chunked~a"
				  *crlf*))
	      
	      (if* (and (not chunked-p)
			(request-reply-content-length req))
		 then (format-dif :xmit sock "Content-Length: ~d~a"
				  (request-reply-content-length req)      
				  *crlf*)
		      (debug-format :info
				    "~d ~s - ~d bytes~%" 
				    (response-number code)
				    (response-desc   code)
				    (request-reply-content-length req))
	       elseif chunked-p
		 then (debug-format :info "~d ~s - chunked~%" 
				    (response-number code)
				    (response-desc   code)
				    )
		 else (debug-format :info
				    "~d ~s - unknown length~%" 
				    (response-number code)
				    (response-desc   code)
				    ))
	      
	      (dolist (head (request-reply-headers req))
		(format-dif :xmit sock "~a: ~a~a"
			    (car head)
			    (cdr head)
			    *crlf*))
	      (format-dif :xmit sock "~a" *crlf*))
      
      (if* (and send-headers chunked-p (eq time :pre))
	 then (force-output sock)
	      (socket:socket-control sock :output-chunking t))
      
      
      ; if we did post-headers then there's a string input
      ; stream to dump out.
      (if* content
	 then (write-sequence content sock))
      
      ;; if we're chunking then shut that off
      (if* (and chunked-p (eq time :post))
	 then (socket:socket-control sock :output-chunking-eof t)
	      (write-sequence *crlf* sock))
      )))

      	
      
(defmethod compute-response-stream ((req http-request) (ent file-entity))
  ;; send directly to the socket since we already know the length
  ;;
  (setf (request-reply-stream req) (request-socket req)))

(defmethod compute-response-stream ((req http-request) (ent computed-entity))
  ;; may have to build a string-output-stream
  (if* (member :string-output-stream (request-reply-strategy req) :test #'eq)
     then (setf (request-reply-stream req) (make-string-output-stream))
     else (setf (request-reply-stream req) (request-socket req))))



(defvar *far-in-the-future*
    (encode-universal-time 12 32 12 11 8 2020 0))

(defmethod set-cookie-header ((req http-request)
			      &key name value expires domain 
				   (path "/")
				   secure)
  ;; put a set cookie header in the list of header to be sent as
  ;; a response to this request.
  ;; name and value are required, they should be strings
  ;; name and value will be urlencoded.
  ;; If expires is nil (the default) then this cookie will expire
  ;;	when the browser exits.
  ;; If expires is :never then we'll sent a date so far into the future
  ;;  that this software is irrelevant
  ;; domain and path if given should be strings.
  ;; domain must have at least two periods (i.e. us  ".franz.com" rather
  ;; than "franz.com".... as netscape why this is important
  ;; secure is either true or false
  ;;
  (let ((res (concatenate 'string 
	       (uriencode-string (string name))
	       "="
	       (uriencode-string (string value)))))
    (if* expires
       then (if* (eq expires :never)
	       then (setq expires *far-in-the-future*))
	    (if* (integerp expires)
	       then (setq res (concatenate 'string
				res
				"; expires="
				(universal-time-to-date expires)))
	       else (error "bad expiration date: ~s" expires)))
    
    (if* domain
       then (setq res (concatenate 'string
			res
			"; domain="
			(string domain))))
    
    (if* path
       then (setq res (concatenate 'string
			res
			"; path="
			(string path))))
    
    (if* secure
       then (setq res (concatenate 'string
			res
			"; secure")))
    
    (push `("Set-Cookie" . ,res) (request-reply-headers req))
    res))


(defun get-cookie-values (req)
  ;; return the set of cookie name value pairs from the current
  ;; request as conses  (name . value) 
  ;;
  (let ((cookie-string (header-slot-value req "cookie")))
    (if* cookie-string
       then ; form is  cookie: name=val; name2=val2; name2=val3
	    ; which is not exactly the format we want to see it in
	    ; to parse it.  we want   
	    ;     cookie: foo; name=val; name=val
	    ; we we'll dummy up something that we want to see. 
	    ; maybe later we'll have a parser for this form too
	    ;
	    (let ((res (parse-header-value
			(concatenate 'string "foo; " cookie-string))))
	      ; res should be: ((:param "foo" ("baz" . "bof")))
	      (if* (and (consp res)
			(consp (car res))
			(eq :param (caar res)))
		 then ; the correct format, must decode pieces
		      (mapcar #'(lambda (ent)
				  (cons 
				   (uridecode-string (car ent))
				   (uridecode-string (cdr ent))))
			      (cddr (car res))))))))
			

;-----------

(defmethod timedout-response ((req http-request) (ent entity))
  ;; return a response to a web server indicating that it is taking
  ;; too long for us to respond
  ;;
  (setf (request-reply-code req) *response-internal-server-error*)
  (with-http-body (req ent)
    (html (:title "Internal Server Error")
	  (:body "500 - The server has taken too long to respond to the request"))))

  
  
  

;;;;;;;;;;;;;;; setup things

(if* (not (boundp '*wserver*))
   then ; create initial wserver object
	(setq *wserver* (make-instance 'wserver)))




  

	  
      



		    
		     
		     
		    
		  
    
