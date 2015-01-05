;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		hash-table.lisp
;;;;	Contents:	Hash-table related functions.
;;;;	History:	11/6/96  RGC  Created.
;;;;				2/17/00  RGC  Implemented WITH-HASH-TABLE-ITERATOR.
;;;;                10/3/03  RGC  REMHASH and CLRHASH now reset the HASH-TABLE-COUNT correctly.
;;;;                              REMHASH is rewritten to be more efficient.
;;;;                7/21/08  RGC  SXHASH modifed to handle symbols correctly
;;;;

(defconstant default-initial-hash-table-size 100)

;; all hash-table sizes must be prime integers
(defvar *hash-table-sizes* '(211 307 401 503 613 701 809 907 1009 1201 1301 1511 2003 3001	4001 5003 
					6007 7001 8009 9001 10007
					11003 12007 13001 14009 15013 16001 17011 18013 19001 20011 25013
					30011 35023 40009 50021 60013 70001 80021 90001 100003 150001
                    200003 250007 300007 400009 500009 700001 1000003
                    1400017 1600033 2000003 2500009 3000017 4000037 5000011))

(defconstant hash-table-test-types '(eq eql equal equalp))

(defun calc-hash-table-size (requested-size)
	(dolist (x *hash-table-sizes*)
		(if (>= x requested-size)
			(return-from calc-hash-table-size x)))
	nil)

;; dummy definitions to avoid compiler warning, redefine below
(defun hash-table-grow (hash-table resize-amount) 
	(declare (ignore resize-amount))
	hash-table)
(defun hash-table-test (hash-table) (declare (ignore hash-table)))
(defun rehash-eq-hash-table (hash-table) (declare (ignore hash-table)))
(defun rehash-eql-hash-table (hash-table) (declare (ignore hash-table)))
(defun rehash-equal-hash-table (hash-table) (declare (ignore hash-table)))
(defun rehash-equalp-hash-table (hash-table) (declare (ignore hash-table)))
(defun rehash-hash-table (hash-table) (declare (ignore hash-table)))
    
;; need to override warning here for HASH-TABLE-COUNT,
;; HASH_TABLE-SIZE not defined yet
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* nil)
(defun print-hash-table (hash-table stream level)
	(declare (ignore level))
	(format stream "#<HASHTABLE ~A/~A>" 
		(hash-table-count hash-table)
		(hash-table-size hash-table)))
(setq *COMPILER-WARN-ON-UNDEFINED-FUNCTION* t)

(in-package :x86)
(defasm common-lisp::which-heap (obj)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [esi]
        mov     edx, [ebp + ARGS_OFFSET]
        mov     ecx, edx
        and     ecx, 7
        cmp     ecx, 4
        jl      short :done
        cmp     ecx, 5
        jg      short :done
        symval  ecx, cl::heap_0_start
        cmp     edx, [ecx + (uvector-offset foreign-heap-ptr-offset)]
        jb      short :done     ;; not in a heap
        symval  ecx, cl::heap_0_end
        cmp     edx, [ecx + (uvector-offset foreign-heap-ptr-offset)]
        jae     short :next1
        mov     eax, 0                ;; heap 0
        jmp     short :done
    :next1
        symval  ecx, cl::heap_1_start
        cmp     edx, [ecx + (uvector-offset foreign-heap-ptr-offset)]
        jb      short :done           ;; not in a heap
        symval  ecx, cl::heap_1_end
        cmp     edx, [ecx + (uvector-offset foreign-heap-ptr-offset)]     
        jae     short :next2
        mov     eax, (* 8 1)          ;; heap 1
        jmp     short :done 
    :next2
        symval  ecx, cl::heap_2_start
        cmp     edx, [ecx + (uvector-offset foreign-heap-ptr-offset)]
        jb      short :done           ;; not in a heap
        symval  ecx, cl::heap_2_end
        cmp     edx, [ecx + (uvector-offset foreign-heap-ptr-offset)]     
        jae     short :done
        mov     eax, (* 8 2)          ;; heap 2
    :done
        mov     ecx, 1
        pop     ebp
        ret
    })

(in-package :common-lisp)
(defvar heap_0_gc_id heap_0_gc_id)
(defvar heap_1_gc_id heap_1_gc_id)
(defvar heap_2_gc_id heap_2_gc_id)

(defstruct (hash-table 
			(:constructor make-skeleton-hash-table)
			(:print-function print-hash-table))
	(rehash-needed nil)		;; must be first field for garbage collector to mark it
	(size (calc-hash-table-size default-initial-hash-table-size))
	(count 0)
	(rehash-size 2.0)
	(rehash-threshold 0.5)
	(table nil)
	(hash-function 'hash-eql-function)
	(test 'eql)
	(test-function 'eql)
	(sync nil)
	(rehash-id (get-gc-id))
    (newest-generation 3)
    (gc-0-level 0)
    (gc-1-level 0)
    (gc-2-level 0))

;; Returns true if the current garbage collection ID is greater than that
;; stored in the rehash-id field of the hash-table. This will be true if 
;; a collection has occurred since the hash-table was last accessed.
(defun hash-table-need-rehash (ht)
    (or (hash-table-rehash-needed ht)
        (let ((gen (hash-table-newest-generation ht)))
            (cond 
                ((= gen 0)(< (hash-table-gc-0-level ht) heap_0_gc_id))
                ((= gen 1)(< (hash-table-gc-1-level ht) heap_1_gc_id))
                ((= gen 2)(< (hash-table-gc-2-level ht) heap_2_gc_id))
                (t nil)))))

;;;;
;;;;	Common Lisp MAKE-HASH-TABLE function
;;;;
(defun make-hash-table (&key (test 'eql) 
							 (size default-initial-hash-table-size) 
							 (rehash-size 2.0)	;; double in size when growing
							 (rehash-threshold 0.5) ;; grow when half full
							 (synchronized t))
	(cond
		((eq test #'eql) (setq test 'eql))
		((eq test #'eq)  (setq test 'eq))
		((eq test #'equal) (setq test 'equal))
		((eq test #'equalp) (setq test 'equalp)))
	(unless (member test hash-table-test-types)
		(error "Invalid test type for hash-table: ~A" test))
	(let ((hash-func nil))
		(cond 
			((eq test 'eq) (setq hash-func 'hash-eq-function))
			((eq test 'eql) (setq hash-func 'hash-eql-function))
			((eq test 'equal) (setq hash-func 'hash-equal-function))
			((eq test 'equalp) (setq hash-func 'hash-equalp-function)))

		(let* ((ht-size (calc-hash-table-size size))
			   (hash-table (make-skeleton-hash-table 
							:size ht-size
							:rehash-size rehash-size
							:rehash-threshold rehash-threshold
							:table (make-array ht-size :initial-element nil)
							:hash-function hash-func
							:test test
							:test-function test
							:sync (if synchronized (cl::allocate-critical-section)))))
			hash-table)))

(defun hash-table-entry-list (hash-table index)
	(let ((table (hash-table-table hash-table)))
		(elt table index)))

;;
;; If the key is a cons, then the actual key is the car, and 
;; the cdr is an integer representation of the address (in case
;; the hash table is using it as a hash id)
;;
(defun hash-table-entry-list-push (hash-table index key value)
	(let ((table (hash-table-table hash-table)))
		(setf (elt table index) (cons key (cons value (elt table index))))))

;;;;
;;;;	HASH-TABLE-KEY-INDEX
;;;;	Returns the index of the passed key.
;;;;
(defun hash-table-key-index (hash-table key)
 	(let* ((table-size (hash-table-size hash-table)))
		(mod (funcall (hash-table-hash-function hash-table) key) table-size)))

(defun update-newest-heap-key (hash-table key)
    ;; track newest heap generation used as a key
    (let ((key-generation (which-heap key)))
        (if (and
                (fixnump key-generation)
                (< key-generation (hash-table-newest-generation hash-table)))
            (setf (hash-table-newest-generation hash-table) key-generation))))

;;;;
;;;;	HASH-TABLE-ADD-ENTRY
;;;;	Adds a new entry and returns the added value.
;;;;
(defun hash-table-add-entry (hash-table key value)
	(let* ((table-size (hash-table-size hash-table))
		   (count (hash-table-count hash-table))
		   (test-func (hash-table-test-function hash-table))
		   index
		   list)

		;; grow if necessary
		(if (> count (/ table-size 2))
			(let ((resize-amount (hash-table-rehash-size hash-table)))
				(if (floatp resize-amount)
					(setq resize-amount (truncate (* (- resize-amount 1.0) table-size))))
                (if (<= resize-amount 0)
                    (setf resize-amount table-size))
				(hash-table-grow hash-table resize-amount)		
				(setq table-size (hash-table-size hash-table))
				(setq count (hash-table-count hash-table))))
        
        ;; track newest heap generation used as a key
        (update-newest-heap-key hash-table key)
                
   		(setq index (hash-table-key-index hash-table key))
		(setq list (hash-table-entry-list hash-table index))

		(do* ()
			((null list))
            (let ((hkey (car list)))
                (if (consp hkey)
                    (setf hkey (car hkey)))
    			(if (funcall test-func key hkey)
    				(return))
			(setq list (cddr list))))

		(if (null list)
			(progn
				(incf (hash-table-count hash-table))
				(hash-table-entry-list-push 
                    hash-table 
                    index 
                    ;; if a heap object push a cons with the car=key, cdr=address
                    (if (or (consp key) (uvectorp key)) (cons key (cl::%uvector-address key)) key)
                    value))
			(setf (car (cdr list)) value)) 
		value))

;;; Use a different rehash method depending on what type of 
;;; test function the hash-table uses.
(defun rehash-hash-table (hash-table)
	;(format t "Rehashing hash-table~%")
    
    (let ((test (hash-table-test hash-table)))
        (cond ((eq test 'eq)(rehash-eq-hash-table hash-table))
              ((eq test 'eql)(rehash-eql-hash-table hash-table))
              ((eq test 'equal)(rehash-equal-hash-table hash-table))
              ((eq test 'equalp)(rehash-equalp-hash-table hash-table)))
        (setf (uref hash-table 2) nil)
        (setf (hash-table-rehash-id hash-table)(get-gc-id))
        
        ;; set all ids to current gc level (0 is always the most recent)
        (setf (hash-table-gc-0-level hash-table) heap_0_gc_id)
        (setf (hash-table-gc-1-level hash-table) heap_0_gc_id)
        (setf (hash-table-gc-2-level hash-table) heap_0_gc_id)))

(defun rehash-hash-list (hash-table rehash-list)
    ;; rehash all the key-value pairs in the rehash-list
    (do* ((x rehash-list (cddr x))
          (key (car x) (car x))
          (value (cadr x) (cadr x)))
        ((null x))
        (if (consp key)
            (setf key (car key)))
         ;(format t "rehashing key ~A, value ~A~%" key value)       ;; debugging
         (hash-table-add-entry hash-table key value)))

;; If the key is a heap object (which we identify as a cons cell in 
;; the key slot) and the address of the key (car of cons) is not
;; the same as the cdr (stored address) then need to rehash.
(defun needs-rehashing-eq (key)
    (and (consp key) 
        (/= (cl::%uvector-address (car key)) (cdr key))))

;; If the key is a heap object and not a number,
;; and the address of the key (car of cons) is not
;; the same as the cdr (stored address) then need to rehash.
(defun needs-rehashing-eql (key)
    (and (consp key)
        (not (numberp (car key))) 
        (/= (cl::%uvector-address (car key)) (cdr key))))

;; If the key is a heap object and not a number or a string,
;; and the address of the key (car of cons) is not
;; the same as the cdr (stored address) then need to rehash.
(defun needs-rehashing-equal (key)
    (and (consp key)
        (let ((k (car key)))
            (and (not (numberp k))
                 (not (simple-string-p k)) 
                 (/= (cl::%uvector-address k) (cdr key))))))

;; If the key is a heap object and not a number or a string,
;; and the address of the key (car of cons) is not
;; the same as the cdr (stored address) then need to rehash.
(defun needs-rehashing-equalp (key)
    (and (consp key)
        (let ((k (car key)))
            (and (not (numberp k))
                 (not (simple-string-p k)) 
                 (/= (cl::%uvector-address k) (cdr key))))))

(defun do-rehash (hash-table needs-rehash-func)
	(let* ((table (hash-table-table hash-table))
	       (size (hash-table-size hash-table))
           (rehash-list '()))
 
        (setf (hash-table-newest-generation hash-table) 3)  ;; this gets recomputed during rehash
        (setf (hash-table-rehash-needed hash-table) nil)
              
        ;; Go through the table. If any bucket contains a key which
        ;; could potentially need rehashing (any heap object) just
        ;; add the whole bucket to the rehash list and remove it from
        ;; the table (empty the bucket).
        (dotimes (i size)
            (let ((list (elt table i)))
                (do* ((x list (cddr x))
                      (key (car x) (car x)))
                    ((null x))
                    (if (consp key)
                        (update-newest-heap-key hash-table (car key)))
                    
                    ;; when these items get rehashed then they will update the newest key
                    (when (funcall needs-rehash-func key) 
                        ;; For convenience, just rehash the whole bucket (it will usually
                        ;; only contain one entry anyway, if our hashing algorithm is good).                       
                        (setf rehash-list (nconc list rehash-list))
                        (setf (elt table i) nil)
                        (return)))))    ;; return from do*
        
        ;; now rehash all the key-value pairs in the rehash-list
        (rehash-hash-list hash-table rehash-list)
        hash-table))

;;; Need to rehash any key which is a heap object
(defun rehash-eq-hash-table (hash-table)
    (do-rehash hash-table 'needs-rehashing-eq))

;;; Need to rehash any key which is a heap object and not a number
(defun rehash-eql-hash-table (hash-table)
    (do-rehash hash-table 'needs-rehashing-eql))

(defun rehash-equal-hash-table (hash-table)
    (do-rehash hash-table 'needs-rehashing-equal))

(defun rehash-equalp-hash-table (hash-table)
    (do-rehash hash-table 'needs-rehashing-equalp))
					
;;;;
;;;;	Common Lisp GETHASH function
;;;;
(declare-synchronized-function (hash-table-sync hash-table)
	GETHASH (key hash-table &optional default)
		(if (hash-table-need-rehash hash-table)
			(rehash-hash-table hash-table))
		(let* ((index (hash-table-key-index hash-table key))
			   (entry-list (hash-table-entry-list hash-table index)))
			(do* ((list entry-list (cddr list))
				  (test-func (hash-table-test-function hash-table))
                  (hkey (car list) (car list)))
				((null list) (values default nil))
                (if (consp hkey)
                    (setf hkey (car hkey)))
				(if (funcall test-func key hkey)
					(return (values (cadr list) t))))))

;;;;
;;;;	Common Lisp (SETF GETHASH) function
;;;;
(declare-synchronized-function (hash-table-sync hash-table)
	(SETF GETHASH) (value key hash-table &optional default)
		(declare (ignore default))
		(if (hash-table-need-rehash hash-table)
			(rehash-hash-table hash-table))
		(prog1
			(hash-table-add-entry hash-table key value)
			(if (hash-table-need-rehash hash-table)
				(rehash-hash-table hash-table))))

;;;;
;;;;	Common Lisp REMHASH function
;;;;
(declare-synchronized-function (hash-table-sync hash-table)
	REMHASH (key hash-table)
	(if (hash-table-need-rehash hash-table)
		(rehash-hash-table hash-table))
    (let* ((index (hash-table-key-index hash-table key))
           (test-func (hash-table-test-function hash-table))
           (list (hash-table-entry-list	hash-table index))
           (hkey (car list)))
        (if (consp hkey)
            (setf hkey (car hkey)))
        
        ;; catch most common case quickly, when the entry we are looking for 
        ;; is at the beginning of the list (usually it will be the only one)
        (when (funcall test-func key hkey)
            (decf (hash-table-count hash-table))
            (setf (elt (hash-table-table hash-table) index) (cddr list))
            (return-from remhash t))
        
        ;; not found, look through any remaining items
    	(do* ((x list (cddr x))
              (y (cddr list) (cddr y))
    		  (hkey (car y) (car y)))
            ((null list) nil)
            (if (consp hkey)
                (setq hkey (car hkey)))
            (when (funcall test-func key hkey)
                (rplacd (cdr x) (cddr y))
                (decf (hash-table-count hash-table))
                (return t)))))

;;;;
;;;;	Common Lisp MAPHASH function
;;;;
(declare-synchronized-function (hash-table-sync hash-table)
	MAPHASH (function hash-table)
	(if (hash-table-need-rehash hash-table)
		(rehash-hash-table hash-table))
	(let ((size (hash-table-size hash-table)))
		(dotimes (i size)
			(do* ((list (hash-table-entry-list	hash-table i) (cddr list))
                  (hkey (car list) (car list))
                  (value (cadr list) (cadr list)))
				((null list))
                (if (consp hkey)
                    (setq hkey (car hkey)))
				(funcall function hkey value)))))
		
;;;;
;;;;	Common Lisp WITH-HASH-TABLE-ITERATOR macro
;;;;
(defmacro with-hash-table-iterator ((name hash-table) &rest forms)
	(let ((ht-sym (gensym))
		  (index-sym (gensym))
		  (ht-size-sym (gensym))
		  (ht-list-sym (gensym))
		  (ret-sym (gensym))
		  (ret-val (gensym))
		  (func-name (gensym)))
		`(let* ((,ht-sym ,hash-table)
			    (,index-sym 0)
			    (,ht-size-sym (hash-table-size ,ht-sym))
			    (,ht-list-sym nil)
			    (,ret-sym nil)
			    (,ret-val nil))
			(flet 
				((,func-name ()
						(do ()
							((or ,ht-list-sym (= ,index-sym ,ht-size-sym)))
							(setf ,ht-list-sym (hash-table-entry-list ,ht-sym ,index-sym))
							(incf ,index-sym))
						(when (and (null ,ht-list-sym)(= ,index-sym ,ht-size-sym))
							(return-from ,func-name nil))
						(setf ,ret-sym (car ,ht-list-sym) ,ret-val (cadr ,ht-list-sym))
                        (if (consp ,ret-sym)
                            (setf ,ret-sym (car ,ret-sym)))
						(setf ,ht-list-sym (cddr ,ht-list-sym))
						(return-from ,func-name (values t ,ret-sym ,ret-val))))
				(macrolet ((,name () '(,func-name)))
					,@forms)))))

;;;;
;;;;	Common Lisp CLRHASH function
;;;;
(declare-synchronized-function (hash-table-sync hash-table)
	CLRHASH (hash-table)
	(let ((table (hash-table-table hash-table))
	      (count (hash-table-size hash-table)))
		(dotimes (i count)
			(setf (elt table i) nil))
        (setf (hash-table-count hash-table) 0)
        hash-table))

;;;;
;;;;	Common Lisp SXHASH function
;;;;    Modified 7/21/08 to handle symbols correctly.
;;;
(defun sxhash (object) 
    (if (symbolp object)
        (hash-equalp-function (symbol-name object))
        (hash-equalp-function object)))

;; double the package capacity
(defun hash-table-grow (hash-table resize-amount)
	(let* ((current-table (hash-table-table hash-table))
	       (current-size (hash-table-size hash-table))
		   (new-size (calc-hash-table-size (+ current-size resize-amount))))
		(unless new-size (error "Could not grow the hash-table: ~A" hash-table))
		(setf (hash-table-table hash-table) 
				(make-array new-size :initial-element nil))
		(setf (hash-table-size hash-table) new-size)
		(setf (hash-table-count hash-table) 0)

		(dotimes (i current-size)
			(do* ((list (elt current-table i) (cddr list))
                  (hkey (car list) (car list)))
				((null list))
                (if (consp hkey)
                    (setq hkey (car hkey)))
				(hash-table-add-entry hash-table hkey (cadr list)))))
	hash-table)

(defun hash-obj-id (obj)
	(hash-eq-function obj))

(defun hash-list (obj)
	(let ((hash-val 0))
		(dolist (x obj)
			(setq hash-val (logxor (hash-equal-function x) hash-val)))
		hash-val))

(defun hash-uvector (x) (declare (ignore x)))    ;; forward declaration

;;#define hashUlong(n) ((n)^((n)<<5)^((n)<<10)^((n)<<15)^((n)<<20)^((n) <<25))
(x86:defasm cl::hash-eq-function (n)
    {
        push    ebp
        mov     ebp, esp
        mov     eax, [ebp + ARGS_OFFSET]
        mov     edx, eax
    begin-atomic
        shl     edx, 5
        xor     eax, edx
        shl     edx, 5
        xor     eax, edx
        shl     edx, 5
        xor     eax, edx 
        shl     edx, 5
        xor     eax, edx
        shl     edx, 5
        xor     eax, edx
        and     al, #xf8
    end-atomic
        pop     ebp
        ret
    })

(defun hash-eql-function (obj)
	(cond 
		((double-float-p obj)(hash-uvector obj))
		((single-float-p obj)(hash-uvector obj))
		((bignump obj) (hash-uvector obj))
		((ratiop obj) (hash-uvector obj))
		((complexp obj)(hash-uvector obj))
		(t (hash-obj-id obj))))

;; forward declarations
(defun hash-string (obj) (declare (ignore obj)) nil)
(defun hash-bit-vector (obj) (declare (ignore obj)) nil)

(defun hash-equal-function (obj)
	(cond 
		((consp obj)(hash-list obj))
		((stringp obj) (hash-string obj))
		((bit-vector-p obj) (hash-bit-vector obj))
		((pathnamep obj)(hash-uvector obj))
        ((foreignp obj)(hash-uvector obj))
        ((foreign-heap-p obj)(hash-uvector obj))
  		(t (hash-eql-function obj))))

(defun hash-equalp-function (obj)
    (cond ((stringp obj)(hash-string (string-upcase obj)))
          (t (hash-equal-function obj))))

(pl::defasm hash-obj-id (x)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + ARGS_OFFSET]
		mov		edx, eax
    begin-atomic
		rol		eax, 9
		xor		edx, eax
		rol		eax, 9
		xor		edx, eax
		rol		eax, 9
		xor		edx, eax
		mov		eax, edx
		and		al,	 #xf8
        shl     eax, 1
        shr     eax, 1      ;; clear high bit to ensure positive numbers
        xor     edx, edx
    end-atomic
		pop		ebp
		ret
	})

;; This function does not do any type checking.
(pl::defasm hash-cell (uvector index)
	{
		push	ebp
		mov		ebp, esp
		mov		edx, [ebp + (+ ARGS_OFFSET 4)]	;; edx = uvector
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = index
	begin-atomic
		shr		ecx, 1							;; untagged integer
		mov		eax, [edx + ecx - uvector-tag]  ;; eax is untagged
		mov		ecx, 1
   		mov		edx, eax
		rol		eax, 9
		xor		edx, eax
		rol		eax, 9
		xor		edx, eax
		rol		eax, 9
		xor		edx, eax
		mov		eax, edx
		and		al,	 #xf8
        shl     eax, 1
        shr     eax, 1      ;; clear high bit to ensure positive numbers
        xor     edx, edx
	end-atomic
   		pop		ebp
		ret
	})

(pl::defasm hash-list (obj)
	{
		push	ebp
		mov		ebp, esp
		push	0				;; local var at [ebp - 4]
	:loop
		mov		eax, [ebp + ARGS_OFFSET]	;; eax = obj
		mov		edx, eax
		and		edx, 7
		cmp		edx, cons-tag
		jne		:end-loop
		mov		edx, [eax]
		mov		[ebp + ARGS_OFFSET], edx ;; obj = (cdr obj)
		mov		eax, [eax - cons-tag]	 ;; eax = (car eax)
		push	eax
		callf	cl::hash-equal-function
		add		esp, 4
		xor		eax, [ebp - 4]
		mov		[ebp - 4], eax
		jmp 	:loop
	:end-loop
		mov		ecx, 1
		pop		eax
		pop		ebp
		ret
	})

(pl::defasm fixnum-mod (x y)
	{
		push	ebp
		mov		ebp, esp
		mov		eax, [ebp + (+ ARGS_OFFSET 4)]	;; eax = arg1
		mov		ecx, [ebp + ARGS_OFFSET]		;; ecx = arg2
		xor		edx, edx
		idiv	ecx					;; edx contains mod
		mov		eax, edx
		mov		ecx, 1
		pop		ebp
		ret
	})

(defun hash-uvector (obj)
	(let ((len (uvector-num-slots obj))
		  (hash-val 0))
		(dotimes (i len)
			(setq hash-val 
				(logxor (hash-cell obj i) (hash-obj-id hash-val))))
		hash-val))

(defun hash-table-key-index (hash-table key)
 	(let* ((table-size (hash-table-size hash-table)))
		(fixnum-mod (funcall (hash-table-hash-function hash-table) key) table-size)))

(defun hash-string (obj)
    (let ((len (length obj))
          (hash-val 0))
        (dotimes (i len hash-val)
            (setq hash-val (logxor (hash-obj-id (char obj i)) (hash-obj-id hash-val))))))

(defun hash-bit-vector (obj)
    (let ((len (length obj))
          (hash-val 0))
        (dotimes (i len hash-val)
            (setq hash-val (logxor (hash-obj-id (aref obj i)) (hash-obj-id hash-val))))))

;;;
;;; do some SETF patching now that we have hash-tables
;;;
(setf *setf-registry* (make-hash-table :test #'equal))
(defvar *setf-expander-registry* (make-hash-table :test #'equal))

;; dump all the setf functions loaded so far into the hash-table
(do ((x (symbol-plist '*setf-registry*)(cddr x)))
	((null x))
	(setf (gethash (car x) *setf-registry*) (cadr x)))

;; redefine these to use the hash table
(defun register-setf-function (name setf-func-name &optional (value-last nil))
	(remhash name *setf-expander-registry*)
	(setf (gethash name *setf-registry*) (list setf-func-name value-last)))

(defun register-setf-expander-function (name setf-func-name)
	(remhash name *setf-registry*)
	(setf (gethash name *setf-expander-registry*) setf-func-name))

(defun get-setf-function (name)
	(car (gethash name *setf-registry*)))

(defun setf-function-value-last-p (name)
	(cadr (gethash name *setf-registry*)))

(defun get-setf-expander-function (name)
	(gethash name *setf-expander-registry*))

;; now clear the property list
(setf (symbol-plist '*setf-registry*) nil)

;;; done SETF patching

;;;
;;; do some DOCUMENTATION patching now that we have hash-tables
;;;
(setf *documentation-registry* (make-hash-table))

;; dump all the documentation entries loaded so far into the hash-table
(do ((x (symbol-plist '*documentation-registry*)(cddr x)))
	((null x))
	(setf (gethash (car x) *documentation-registry*) (cadr x)))

;; redefine these to use the hash table
(defun documentation (symbol doc-type)
	(getf (gethash symbol *documentation-registry*) doc-type))

(defun |(SETF DOCUMENTATION)| (doc-string symbol doc-type)
	(setf (getf (gethash symbol *documentation-registry*) doc-type) doc-string)
	doc-string)

;; now clear the property list
(setf (symbol-plist '*documentation-registry*) nil)

;;; done DOCUMENTATION patching

