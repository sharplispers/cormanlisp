;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RDNZL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/rdnzl/examples/excel.lisp,v 1.8 2010/05/18 10:55:37 edi Exp $

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This example is an adapted version of the code found at
;;; <http://www.c-sharpcorner.com/winforms/ExcelReadMG.asp>.
;;; It was tested with Microsoft Office 2003.

;;; Note: LOAD this file, before you COMPILE-FILE it.

(in-package :rdnzl-user)

(enable-rdnzl-syntax)

(import-types "System.Windows.Forms" "DialogResult" "OpenFileDialog")
(import-types "Microsoft.Office.Interop.Excel" "ApplicationClass" "WorkbookClass" "Worksheet")

(use-namespace "Microsoft.Office.Interop.Excel")
(use-namespace "System.Windows.Forms")

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The pathname of the file \(`test.lisp') where this variable was
defined.")

(defconstant +missing+ [$System.Reflection.Missing.Value]
  "Represents missing arguments.")

(defconstant +dialog-ok+ [$DialogResult.OK]
  "Returned by `OpenFileDialog' if the user confirmed the dialog.")

(defconstant +initial-directory+
  (load-time-value 
   (namestring (make-pathname :name nil :type nil
                              :defaults *this-file*))))

(defconstant +initial-filename+
  (load-time-value 
   (namestring (make-pathname :name "example" :type "xls"
                              :defaults *this-file*))))

(defun prompt-for-file (title)
  (let ((dialog (new "OpenFileDialog")))
    (setf [%InitialDirectory dialog] +initial-directory+
          [%Filter dialog]
          "Microsoft Excel files (*.xls)|*.xls|All files (*.*)|*.*"
          [%FileName dialog]
          +initial-filename+
          [%Title dialog] title)
    (and [Equals [ShowDialog dialog] +dialog-ok+]
         [%FileName dialog])))

(defun get-excel-range (file-name range)
  (let* ((app (new "ApplicationClass"))
         (workbooks [%Workbooks app])
         (workbook (cast [Open workbooks file-name
                               +missing+ nil +missing+
                               +missing+ +missing+ +missing+
                               +missing+ +missing+ +missing+
                               +missing+ +missing+ +missing+
                               +missing+ +missing+]
                         "WorkbookClass"))
         (worksheets [%Worksheets workbook])
         (sheet (cast [get_Item worksheets 1] "Worksheet"))
         (range [get_Range sheet range +missing+]))
    (prog1 (cast [%Value2 [%Cells range]] "System.Array")
      [Quit app])))

(defun convert-range-array-to-lists (range-array)
  (loop for row from 1 to [GetLength range-array 0]
        collect (loop for col from 1 to [GetLength range-array 1]
                      collect [ToString (aref* range-array row col)])))

(defun range-contents (&key (file-name (prompt-for-file "Select an Excel file"))
                            (range "A1:C4"))
  (convert-range-array-to-lists
   (get-excel-range file-name range)))

(disable-rdnzl-syntax)