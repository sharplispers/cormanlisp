;;;; AllegroServe loader for Corman Lisp - Version 1.0
;;;;
;;;; Copyright (C) 2000 Christopher Double. All Rights Reserved.
;;;; 
;;;; License
;;;; =======
;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the author be held liable for any damages
;;;; arising from the use of this software.
;;;;
;;;; Permission is granted to anyone to use this software for any purpose,
;;;; including commercial applications, and to alter it and redistribute
;;;; it freely, subject to the following restrictions:
;;;;
;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product documentation
;;;;    would be appreciated but is not required.
;;;;
;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.
;;;;
;;;; 3. This notice may not be removed or altered from any source 
;;;;    distribution.
;;;;
;;;; More recent versions of this software may be available at:
;;;;   http://www.double.nz/cl
;;;;
;;;; Comments, suggestions and bug reports to the author, 
;;;; Christopher Double, at: chris@double.nz
;;;;
;;;; 03/03/2000 - 1.0 
;;;;              Initial release.
;;;;              Change the *as-source-directory* constant to
;;;;              point to the installation directory of 
;;;;              the AllegroServe install files.
;;;;              
;;;;
(defconstant *as-source-directory* 
    (concatenate 'string *cormanlisp-directory* "\\modules\\as\\"))

(require 'acl-excl)
(require 'uri)
(require 'mp)
(require 'acl-socket)

(in-package :cl-user)

(defconstant *as-files* 
	(list 
		"htmlgen\\htmlgen.cl"
		"macs.cl"
		"main.cl"
		"parse.cl"
		"decode.cl"
		"publish.cl"
		"authorize.cl"
		"log.cl"
		"client.cl"))

(loop for file in *as-files* do (load (concatenate 'string *as-source-directory* file)))

(provide 'allegroserve)
