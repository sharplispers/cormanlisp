;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;  Portions Copyright (c) 2000 Vassili Bykov

;;;;  SEQUENCES.LISP -- Most of the functions listed in the "Sequences
;;;;  Dictionary" section of the HyperSpec.  Some of them are defined at the
;;;;  earlier system build stage; those are in the files SEQUENCE.LISP.

;;;;  CONTENTS: POSITION POSITION-IF POSITION-IF-NOT FIND FIND-IF FIND-IF-NOT
;;;;  COUNT COUNT-IF COUNT-IF-NOT FILL REPLACE MISMATCH SEARCH REMOVE
;;;;  REMOVE-IF REMOVE-IF-NOT DELETE DELETE-IF DELETE-IF-NOT COPY-SEQ
;;;;  REMOVE-DUPLICATES DELETE-DUPLICATES MERGE SORT STABLE-SORT NREVERSE-LIST
;;;;  NREVERSE REVERSE SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT NSUBSTITUTE
;;;;  NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT REDUCE

;;;;  TO DO: proper exception signaling

;;;;  HISTORY:
;;;;  9/27/1996 RGC Created.
;;;;  1/03/1999 VB  Rewrote most functions for better performance, space
;;;;                economy, and standard compliance.  Added MERGE,
;;;;                STABLE-SORT, NSUBSTITUTE* group.
;;;;  1/06/1999 RGC Incorporated SEARCH fix by Vassili.
;;;;  9/13/1999 RGC Incorporated REDUCE fix by Vassili
;;;;  7/14/2000 RGC Modified POSITION, COUNT to pass arguments to TEST
;;;;				functions in the correct order (item, sequence-element)


(in-package :common-lisp)

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  Bounding Index Validation
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; As clarified in the SUBSEQ-OUT-OF-BOUNDS HyperSpec writeup.

(defun validate-bounding-indices (sequence start end)
  ;; => (VALUES length actual-end)
  (let ((length (length sequence)))
    (unless end (setq end length))
    (unless (and (integerp start) (integerp end) (<= 0 start end length))
      (error "Invalid arguments: START = ~S, END = ~S" start end))
    (values length end)))

(defun validate-2-bounding-indices (sequence1 sequence2
				    start1 end1 start2 end2)
  ;; => (VALUES length1 length2 actual-end1 actual-end2)
  (let ((length1 (length sequence1))
	(length2 (length sequence2)))
    (unless end1 (setq end1 length1))
    (unless end2 (setq end2 length2))
    (unless (and (integerp start1) (integerp end1)
		 (integerp start2) (integerp end2)
		 (<= 0 start1 end1 length1)
		 (<= 0 start2 end2 length2))
      (error "Invalid arguments: START1 = ~S, END1 = ~S, START2 = ~S, END2 = ~S"
	     start1 end1 start2 end2))
    (values length1 length2 end1 end2)))

(defun %make-vector-from-list (list &optional element-type length)
  (unless length (setq length (length list)))
  (make-array length
	      :element-type element-type
	      :initial-contents list))

(defun %make-vector-from-reversed-list (list &optional element-type length)
  (unless length (setq length (length list)))
  (let ((v (make-array length :element-type element-type)))
    (do ((i (1- length) (1- i))
	 (cell list (cdr cell)))
	((null cell))
      (setf (aref v i) (car cell)))
    v))



;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  %FUNCALL-WITH-ELEMENTS-AND-INDICES
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;  VB 12/27/1998 -- POSITION*, FIND*, and COUNT* are completely rewritten,
;;;  featuring linear execution time on lists and iteration algorithm factored
;;;  out for easy tweaking.
;;;
;;; %FUNCALL-WITH-ELEMENTS-AND-INDICES is the workhorse function implementing
;;; proper iteration over sequences of different kinds.  Nine other sequence
;;; functions: POSITION*, FIND*, and COUNT*, dispatch to it.  This involves
;;; some slowdown compared to the direct implementation approach, but the
;;; slowdown is moderate enough (3-5%) to trade it off for simplicity.  The
;;; FUNCTION argument is a function of 3 arguments: (ELEMENT ORIGINAL-ELEMENT
;;; and INDEX).  It gets called for each element of the sequence.  The
;;; element, after being filtered through the KEY function, becomes the
;;; ELEMENT argument.  The original (unkeyed) value is the ORIGINAL-ELEMENT
;;; argument.

(defun %funcall-with-elements-and-indices (function sequence
					   from-end start end key)
  (let (length)
    (multiple-value-setq (length end)
      (validate-bounding-indices sequence start end))
    (let ((element-count (- end start))
	  (step (if from-end -1 1))
	  elt)
      (cond ((listp sequence)
	     ;; ELT is expensive, should CDR down the list
	     (when from-end (setq sequence (reverse sequence)))
	     (do ((s (nthcdr (if from-end (- length end) start) sequence)
		     (cdr s))
		  (i (if from-end (1- end) start) (+ i step))
		  (rep 0 (1+ rep)))
		 ((= rep element-count) nil)
	       (setq elt (car s))
	       (funcall function
			(if key (funcall key elt) elt)
			elt
			i)))
	    (t
	     ;; INDEXABLE SEQUENCE
	     (do ((i (if from-end (1- end) start) (+ i step))
		  (rep 0 (1+ rep)))
		 ((= rep element-count) nil)
	       (setq elt (aref sequence i))
	       (funcall function
			(if key (funcall key elt) elt)
			elt
			i)))))))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  POSITION
;;;  POSITION-IF
;;;  POSITION-IF-NOT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun position (item sequence 
		      &key from-end (test #'eql) test-not (start 0) end key)
	(%funcall-with-elements-and-indices
		(if test-not
			#'(lambda (elt orig-elt index)
				(declare (ignore orig-elt))
				(unless (funcall test-not item elt)
					(return-from position index)))
			#'(lambda (elt orig-elt index)
				(declare (ignore orig-elt))
				(when (funcall test item elt)
					(return-from position index))))
		sequence
		from-end start end key))

(defun position-if (test sequence 
			 &key from-end (start 0) end key)
  (%funcall-with-elements-and-indices
   #'(lambda (elt orig-elt index)
			(declare (ignore orig-elt))
       (when (funcall test elt)
	 (return-from position-if index)))
   sequence
   from-end start end key))

(defun position-if-not (test sequence
			     &key from-end (start 0) end key)
  (position-if #'(lambda (elt) (not (funcall test elt)))
	       sequence
	       :from-end from-end
	       :start start
	       :end end
	       :key key))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  FIND
;;;  FIND-IF
;;;  FIND-IF-NOT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun find (item sequence
		&key from-end (test #'eql) test-not (start 0) end key)
	(%funcall-with-elements-and-indices
		(if test-not
			#'(lambda (elt orig-elt index)
				(declare (ignore index))
				(unless (funcall test-not item elt)
					(return-from find orig-elt)))
			#'(lambda (elt orig-elt index)
				(declare (ignore index))
				(when (funcall test item elt)
					(return-from find orig-elt))))
		sequence
		from-end start end key))

(defun find-if (test sequence 
		&key from-end (start 0) end key)
  (%funcall-with-elements-and-indices
   #'(lambda (elt orig-elt index)
			(declare (ignore index))
       (when (funcall test elt)
	 (return-from find-if orig-elt)))
   sequence
   from-end start end key))

(defun find-if-not (test sequence 
		&key from-end (start 0) end key)
  (%funcall-with-elements-and-indices
   #'(lambda (elt orig-elt index)
			(declare (ignore index))
       (unless (funcall test elt)
	 (return-from find-if-not orig-elt)))
   sequence
   from-end start end key))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  COUNT
;;;  COUNT-IF
;;;  COUNT-IF-NOT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun count (item sequence 
		   &key from-end (test #'eql) test-not (start 0) end key)
  (let ((count 0))
    (%funcall-with-elements-and-indices
     (if test-not
	 #'(lambda (elt orig-elt index)
					(declare (ignore orig-elt index))
	     (unless (funcall test-not item elt)
	       (incf count)))
	 #'(lambda (elt orig-elt index)
					(declare (ignore orig-elt index))
	     (when (funcall test item elt)
	       (incf count))))
     sequence
     from-end start end key)
    count))

(defun count-if (test sequence 
		&key from-end (start 0) end key)
  (let ((count 0))
    (%funcall-with-elements-and-indices
     #'(lambda (elt orig-elt index)
					(declare (ignore orig-elt index))
	 (when (funcall test elt)
	   (incf count)))
     sequence
     from-end start end key)
    count))

(defun count-if-not (test sequence 
		&key from-end (start 0) end key)
  (let ((count 0))
    (%funcall-with-elements-and-indices
     #'(lambda (elt orig-elt index)
				(declare (ignore orig-elt index))
	 (unless (funcall test elt)
	   (incf count)))
     sequence
     from-end start end key)
    count))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  FILL
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun fill (sequence item &key (start 0) end)
  (unless end
    (setq end (length sequence)))
  (cond ((listp sequence)
	 (do ((s (nthcdr start sequence) (cdr s))
	      (i start (1+ i)))
	     ((or (>= i end) (null s)))
	   (rplaca s item)))
	(t
	 (dotimes (i (- end start))
	   (setf (elt sequence (+ i start)) item))))
  sequence)

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  REPLACE
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; VB 12/27/1998 -- also fixed non-HyperSpec behaviour when (eq
;;; sequence1 sequence2).

(defun replace (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
  (let (length1
	length2)
		(declare (ignore length1 length2))
    (multiple-value-setq (length1 length2 end1 end2)
      (validate-2-bounding-indices sequence1 sequence2 start1 end1 start2 end2))
    ;; Care should be taken to properly copy between the same
    ;; sequence.  For now, let's assume this is a rare operation and
    ;; it is OK to create some garbage to properly do that.
    (when (eq sequence1 sequence2)
      (setq sequence2 (subseq sequence2 start2 end2))
      (setq start2 0
	    end2 (length sequence2)))
    (let ((setter (%make-setter-iterator sequence1 start1))
	  (getter (%make-getter-iterator sequence2 start2)))
      (dotimes (i (min (- end1 start1) (- end2 start2)))
	(funcall setter (funcall getter)))))
  sequence1)


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  MISMATCH
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;  VB 12/27/1998 -- also fixed non-HyperSpec behaviour when
;;;  :from-end is true.

(defun mismatch (sequence1 sequence2 
			   &key from-end
			   (test #'eql) test-not
			   key
			   (start1 0) (start2 0)
			   end1 end2)
  (when test-not
    (setq test #'(lambda (x y) (not (funcall test-not x y)))))
  (let (length1
	length2
	(pred (if (null key)
		  test
		  #'(lambda (a b) (funcall test
					   (funcall key a)
					   (funcall key b))))))
    (multiple-value-setq (length1 length2 end1 end2)
      (validate-2-bounding-indices sequence1 sequence2 start1 end1 start2 end2))
    (if from-end
	;; Right-to-left comparison starting at the right end.
	(let ((iter1 (%make-reverse-getter-iterator sequence1 (- length1 end1)))
	      (iter2 (%make-reverse-getter-iterator sequence2 (- length2 end2)))
	      (matchlen (min (- end1 start1) (- end2 start2))))
	  (dotimes (rep matchlen)
	    (unless (funcall pred (funcall iter1) (funcall iter2))
	      (return-from mismatch (- end1 rep))))
	  (if (= (- end1 start1) (- end2 start2))
	      nil
	      (- end1 matchlen)))
	;; Normal left-to-right comparison starting at the left end.
	(let ((iter1 (%make-getter-iterator sequence1 start1))
	      (iter2 (%make-getter-iterator sequence2 start2))
	      (matchlen (min (- end1 start1) (- end2 start2))))
	  (dotimes (rep matchlen)
	    (unless (funcall pred (funcall iter1) (funcall iter2))
	      (return-from mismatch (+ rep start1))))
	  (if (= (- end1 start1) (- end2 start2))
	      nil
	      (+ start1 matchlen))))))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  SEARCH
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; Here goes the same OOP-from-chewing-gum approach as used in MAP: since the
;;; core algorithm of search is the same for all sequences (it is naive
;;; search--cannot do KMP because we deal with a more general case) but the
;;; rules of efficient element retrieval and position advancing are different,
;;; we package up the sequence-type specific operations behind a bunch of
;;; closures and call closures to perform the operations without knowing their
;;; implementation.  Since some closures have to share the captured state, we
;;; manufacture them in groups returned as multiple values.

(defun make-getter-and-resetter (sequence length start end from-end)
  ;; Return two closures; the first enumerates the elements of the
  ;; sequence, starting either from START or from the END, depending
  ;; on FROM-END value.  The second "resets" the first to the original
  ;; position.  LENGTH is passed by the caller to avoid doing it twice
  ;; on lists.
  (if (listp sequence)
      (let* ((tail (if from-end
		       (nthcdr (- length end) (reverse sequence))
		       (nthcdr start sequence)))
	     (s tail))
	(values #'(lambda () (pop s))
		#'(lambda () (setq s tail))))
      (let* ((base (if from-end end (1- start)))
	     (i base)
	     (step (if from-end -1 1)))
	(values #'(lambda () (aref sequence (setq i (+ i step))))
		#'(lambda () (setq i base))))))

(defun make-getter-and-advancer (sequence length start end from-end)
  ;; Return two closures; the first enumerates the elements of the
  ;; sequence, starting either from START or from the END, depending
  ;; on FROM-END value.  The second advances the original position by
  ;; one element and resets the first closure to the new original
  ;; position.  LENGTH is passed by the caller to avoid doing it twice
  ;; on lists.
  (if (listp sequence)
      (let* ((tail (if from-end
		       (nthcdr (- length end) (reverse sequence))
		       (nthcdr start sequence)))
	     (s tail))
	(values #'(lambda () (pop s))
		#'(lambda () (pop tail) (setq s tail))))
      (let* ((base (if from-end end (1- start)))
	     (i base)
	     (step (if from-end -1 1)))
	(values #'(lambda () (aref sequence (setq i (+ i step))))
		#'(lambda () (setq i (setq base (+ base step))))))))

(defun search (sequence1 sequence2 
	       &key (from-end nil)
			 (test #'eql) 
			 test-not
			 key
			 (start1 0) (start2 0)
			 end1 end2)
  (if test-not (setq test #'(lambda (x y) (not (funcall test-not x y)))))
  (let (length1
	length2
	(predicate (if key
		       #'(lambda (a b) (funcall test
						(funcall key a)
						(funcall key b)))
		       test)))
    (multiple-value-setq (length1 length2 end1 end2)
      (validate-2-bounding-indices sequence1 sequence2 start1 end1 start2 end2))
    ;; Lightning crackles at the tips of my fingers...
    (multiple-value-bind (pattern-elt-getter pattern-resetter)
	(make-getter-and-resetter sequence1 length1 start1 end1 from-end)
      (multiple-value-bind (seq-elt-getter seq-advancer)
	  (make-getter-and-advancer sequence2 length2 start2 end2 from-end)
	(let ((pat-length (- end1 start1)))
	  (dotimes (si (- end2 start2 (1- pat-length)))
	    (block try-match
	      (dotimes (pat-i pat-length)
		(unless (funcall predicate
				 (funcall pattern-elt-getter)
				 (funcall seq-elt-getter))
		  (return-from try-match nil)))
	      ;; pattern match loop complete, we've got a match
	      (return-from search (if from-end
				    (- end2 (- end1 start1) si)
				    (+ start2 si))))
	    ;; returned from try-match - no match at SI
	    (funcall pattern-resetter)
	    (funcall seq-advancer)))))))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  REMOVE
;;;  REMOVE-IF
;;;  REMOVE-IF-NOT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; All the REMOVE* functions are actually just calls to the following.

(defun %remove-if (test sequence start end from-end count key)
  (let (length)
    (multiple-value-setq (length end)
      (validate-bounding-indices sequence start end))
    (let ((predicate (if key
			 #'(lambda (elt) (funcall test (funcall key elt)))
			 test))
	  (result nil)
	  (deletions 0)
	  (getter (if from-end
		      (%make-reverse-getter-iterator sequence)
		      (%make-getter-iterator sequence))))
      (dotimes (i (if from-end (- length end) start))
	(push (funcall getter) result))
      (dotimes (rep (- end start))
	(let ((elt (funcall getter)))
	  (cond ((funcall predicate elt)
		 (incf deletions)
		 (when (and count (= count deletions))
		   (dotimes (i (- end start (1+ rep)))
		     (push (funcall getter) result))
		   (return)))
		(t
		 (push elt result)))))
      (dotimes (i (if from-end start (- length end)))
	(push (funcall getter) result))
      (if (vectorp sequence)
	  (if from-end
	      (%make-vector-from-list result
				      (array-element-type sequence)
				      (- length deletions))
	      (%make-vector-from-reversed-list result
					       (array-element-type sequence)
					       (- length deletions)))
	  (if from-end
	      result
	      (nreverse result))))))
  
(defun remove-if (test sequence 
		       &key (from-end nil)
		       (start 0)
		       end
		       (count nil)
		       (key nil))
  (%remove-if test sequence start end from-end count key))

(defun remove-if-not (test sequence 
			   &key (from-end nil)
			   (start 0)
			   end
			   (count nil)
			   (key nil))
  (%remove-if #'(lambda (elt) (not (funcall test elt)))
	      sequence start end
	      from-end count key))

(defun remove (item sequence 
		    &key (from-end nil)
		    (test #'eql)
		    (test-not nil)
		    (start 0)
		    (end nil)
		    (count nil)
		    (key nil))
  (when test-not
    (setq test #'(lambda (a b) (not (funcall test-not a b)))))
  (%remove-if #'(lambda (elt) (funcall test item elt))
	      sequence start end
	      from-end count key))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  DELETE
;;;  DELETE-IF
;;;  DELETE-IF-NOT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun %delete-if (test list start end count key)
  (let (length)
		(declare (ignore length))
    (multiple-value-setq (length end)
      (validate-bounding-indices list start end))
    (let ((predicate (if key
			 #'(lambda (elt) (funcall test (funcall key elt)))
			 test))
	  (element-count (- end start))
	  (head (cons nil list)))
      (do ((cell (nthcdr start list) (cdr cell))
	   (last-to-keep (nthcdr start head))
	   (rep 0 (1+ rep))
	   (deletions 0))
	  ((= rep element-count) (rplacd last-to-keep cell))
	(cond ((funcall predicate (car cell))
	       (when (and count (= (incf deletions) count))
		 (rplacd last-to-keep (cdr cell))
		 (return)))
	      (t
	       (rplacd last-to-keep cell)
	       (setq last-to-keep cell))))
      (cdr head))))

(defun delete (item sequence
	       &key from-end (test #'eql) test-not (start 0) end count key)
  (when test-not
    (setq test #'(lambda (a b) (not (funcall test-not a b)))))
  (cond ((or (vectorp sequence) (and from-end count))
	 ;; Cannot do destructive delete on vectors, and counted from-end
	 ;; destructive delete on lists is too much of a hassle.
	 (%remove-if #'(lambda (elt) (funcall test item elt))
		     sequence start end
		     from-end count key))
	(t
	 ;; SEQUENCE is a list and we can delete either all that matches the
	 ;; test, or COUNT elements from the beginning.  The HyperSpec says it
	 ;; is OK to ignore FROM-END.
	 (%delete-if #'(lambda (elt) (funcall test item elt))
		     sequence start end
		     count key))))

(defun delete-if (test sequence
		       &key (start 0) end from-end count key)
  (cond ((or (vectorp sequence) (and from-end count))
	 (%remove-if test sequence start end from-end count key))
	(t
	 (%delete-if test sequence start end count key))))

(defun delete-if-not (test sequence
			   &key (start 0) end from-end count key)
  (cond ((or (vectorp sequence) (and from-end count))
	 (%remove-if #'(lambda (elt) (not (funcall test elt)))
		     sequence start end from-end count key))
	(t
	 (%delete-if #'(lambda (elt) (not (funcall test elt)))
		     sequence start end count key))))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  COPY-SEQ
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun copy-seq (sequence)
  (cond ((listp sequence)
	 (copy-list sequence))
	((vectorp sequence)
	 (make-array (length sequence) 
		     :element-type (array-element-type sequence)
		     :initial-contents sequence))
	(t
	 (error "Not a sequence: ~S" sequence))))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  REMOVE-DUPLICATES
;;;  DELETE-DUPLICATES
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun remove-duplicates (sequence 
			  &key from-end (test #'eql) test-not (start 0) end key)
  (let (length)
    (multiple-value-setq (length end)
      (validate-bounding-indices sequence start end))
    (when test-not
      (setq test #'(lambda (a b) (not (funcall test-not a b)))))
    (let ((getter (%make-getter-iterator sequence))
	  (predicate (if key
			 #'(lambda (a b) (funcall test (funcall key a) (funcall key b)))
			 test))
	  (result (cons nil nil))
	  (result-length (+ start (- length end))) ; the elements outside the "work area"
	  (result-prefix nil))
      (when (> start 0)
	(dotimes (i start)
	  (push (funcall getter) result-prefix)))
      (dotimes (i (- end start))
	(let ((elt (funcall getter)))
	  (do ((result-cell (cdr result) (cdr result-cell))
	       (prev-cell result (cdr prev-cell)))
	      ((null result-cell)
	       ;; No duplicates found, ELT belongs in the RESULT
	       (incf result-length)
	       (rplacd result (cons elt (cdr result))))
	    (when (funcall predicate (car result-cell) elt)
	      (unless from-end
		(rplacd prev-cell (cdr result-cell))
		(rplacd result (cons elt (cdr result))))
	      (return)))))
      (unless (= length end)
	(dotimes (i (- length end))
	  (rplacd result (cons (funcall getter) (cdr result)))))
      (setq result (nconc (nreverse result-prefix) (nreverse (cdr result))))
      (if (listp sequence)
	  result
	  (make-array result-length
		      :element-type (array-element-type sequence)
		      :initial-contents result)))))

(setf (symbol-function 'delete-duplicates) (symbol-function 'remove-duplicates))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  MERGE
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun merge-lists (list1 list2 predicate key)
  ;; The argument lists are destroyed, the list we return is built of
  ;; their cells.
  (let* ((result (cons nil nil))
	 (last result)
	 elt1
	 elt2)
    (do () ((not (and list1 list2)))
      (unless elt1
	(setq elt1 (if key (funcall key (car list1)) (car list1))))
      (unless elt2
	(setq elt2 (if key (funcall key (car list2)) (car list2))))
      (cond ((funcall predicate elt2 elt1)
	     (rplacd last list2)
	     (pop list2)
	     (setq elt2 nil))
	    (t
	     (rplacd last list1)
	     (pop list1)
	     (setq elt1 nil)))
      (setq last (cdr last)))
    (if list1
	(rplacd last list1)
	(if list2
	    (rplacd last list2)))
    (cdr result)))

(defun merge-vectors (target source1 source2 predicate key
		      start start1 end1 start2 end2)
  ;; TARGET is modified; it should be large enough to fit all the
  ;; elements merged from the source sequences starting at START
  ;; index.  SOURCE1 and SOURCE2 could be same vector.
  ;; This is used by both MERGE and STABLE-SORT (through NMERGESSORT-VECTOR).
  (let* ((src1 start1)
	 (src2 start2)
	 (dst start)
	 (elt1 nil)
	 (elt2 nil)
	 (keyed1 nil)
	 (keyed2 nil))
    (do () ((or (= src1 end1) (= src2 end2)))
      (unless elt1
	(setq elt1 (aref source1 src1)
	      keyed1 (if key (funcall key elt1) elt1)))
      (unless elt2
	(setq elt2 (aref source2 src2)
	      keyed2 (if key (funcall key elt2) elt2)))
      (cond ((funcall predicate keyed2 keyed1)
	     (setf (aref target dst) elt2)
	     (incf src2)
	     (setq elt2 nil))
	    (t				; i.e. k1 <= k2 -- k1 goes first
	     (setf (aref target dst) elt1)
	     (incf src1)
	     (setq elt1 nil)))
      (incf dst))
    ;; One of the subsequences ran out; copy the rest of the other.
    (if (< src1 end1)
	(do () ((= src1 end1))
	  (setf (aref target dst) (aref source1 src1))
	  (incf src1)
	  (incf dst))
	(if (< src2 end2)
	    (do () ((= src2 end2))
	      (setf (aref target dst) (aref source2 src2))
	      (incf src2)
	      (incf dst)))))
  target)
  
(defun merge (result-type sequence1 sequence2 predicate &key key)
  (cond ((and (listp sequence1) (listp sequence2))
	 (let ((result (merge-lists sequence1 sequence2 predicate key)))
	   (coerce result result-type)))
	((and (vectorp sequence1) (vectorp sequence2))
	 (let* ((length1 (length sequence1))
		(length2 (length sequence2))
		(result (make-array (+ length1 length2))))
	   (merge-vectors result sequence1 sequence2 predicate key
			  0 0 length1 0 length2)
	   (coerce result result-type)))
	(t
	 (merge result-type
		(coerce sequence1 'vector)
		(coerce sequence2 'vector)
		predicate
		:key key))))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  SORT
;;;  STABLE-SORT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;    VB 12/27/1998 -- got rid of bubblesort and added STABLE-SORT
;;;    function.  SORT uses quicksort algorithm, STABLE-SORT does
;;;    merge sort.  The two helper functions below do the work.  For
;;;    list sorting, we copy list content to a temporary vector, sort
;;;    the vector, then copy the sorted vector's elements back to the
;;;    list.

(defun nquicksort-vector (vector predicate key)
  (labels ((QS (start end)
	     (when (< start end)
	       (let ((i (partition start end)))
		 (qs start i)
		 (qs (1+ i) end))))
	   (PARTITION (start end)
	     (do ((x (if key
			 (funcall key (aref vector start))
			 (aref vector start)))
		  (i (1- start))
		  (j (1+ end))
		  elt)
		 (nil)
	       (do () (nil)
		 (decf j)
		 (setq elt (aref vector j))
		 (unless (funcall predicate x (if key (funcall key elt) elt))
		   (return nil)))
	       (do () (nil)
		 (incf i)
		 (setq elt (aref vector i))
		 (unless (funcall predicate (if key (funcall key elt) elt) x)
		   (return nil)))
	       (cond ((< i j)
		      (setq elt (aref vector i))
		      (setf (aref vector i) (aref vector j))
		      (setf (aref vector j) elt))
		     (t
		      (return-from partition j))))))
    (qs 0 (1- (length vector))))
  vector)

(defun nmergesort-vector (vector predicate key)
  (let ((alter (copy-seq vector)))
    (labels ((mergesort (source target start end)
	       (when (< start (1- end))
		 (let ((mid (truncate (/ (+ start end) 2))))
		   (mergesort target source start mid)
		   (mergesort target source mid end)
		   (merge-vectors target source source predicate key
				  start start mid mid end)))))
      (mergesort alter vector 0 (length vector))))
  vector)

(defun sort (sequence predicate &key key)
  (let ((seq (if (listp sequence)
		 (make-array (length sequence) :initial-contents sequence)
		 sequence)))
    (nquicksort-vector seq predicate key)
    (when (listp sequence)
      (do ((i 0 (1+ i))
	   (s sequence (cdr s)))
	  ((null s))
	(rplaca s (aref seq i)))))
  sequence)

(defun stable-sort (sequence predicate &key key)
  (let ((seq (if (listp sequence)
		 (make-array (length sequence) :initial-contents sequence)
		 sequence)))
    (nmergesort-vector seq predicate key)
    (when (listp sequence)
      (do ((i 0 (1+ i))
	   (s sequence (cdr s)))
	  ((null s))
	(rplaca s (aref seq i)))))
  sequence)


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  NREVERSE
;;;  REVERSE
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(pl:defasm nreverse-list (list)
	{
		push	ebp
		mov		ebp, esp
		push	ebx
		cmp 	ecx, 1
		jz 		short :t1
		callp 	_wrong-number-of-args-error
	:t1
		mov		eax, [esi]		;eax = nil
		mov		ebx, [ebp + ARGS_OFFSET]
		mov		edx, ebx
		and		edx, 7
		cmp		edx, cons-tag
		jne		short :exit		;if no elements, just return the list
		mov		ecx, [ebx]		;eax = prev, ebx = curr, ecx = next
	:loop
		mov		[ebx], eax		;CDR(curr) = prev
		mov		edx, ecx		;(if (not (consp next)) (go exit))
		and		edx, 7
		cmp		edx, cons-tag
		jne		short :exit
		mov		eax, ebx		;prev = curr		;
		mov		ebx, ecx		;curr = next
		mov		ecx, [ecx]		;next = CDR(next)
		jmp		short :loop
	:exit
		mov		eax, ebx
		mov		ecx, 1
		pop		ebx
		pop		ebp
		ret
	})

(defun nreverse (x)
  (cond ((listp x)(cl::nreverse-list x))
	((vectorp x)
	 (let* ((length (length x))
		(middle (truncate length 2))
		temp
		(high (- length 1)))
	   (dotimes (low middle)
	     (setf temp (elt x low))
	     (setf (elt x low) (elt x high))
	     (setf (elt x high) temp)
	     (decf high))
	   x))
	(t (error "Sequence expected, got: ~S" x))))

(defun reverse (sequence)
  (cond ((listp sequence)
	 (let ((reversed-list nil))
	   (dolist (x sequence)
	     (push x reversed-list))
	   reversed-list))
	((vectorp sequence)
	 (let* ((reversed-vector 
		 (make-array (length sequence) 
			     :element-type (array-element-type sequence)
			     :initial-contents sequence))
		(length (length sequence))
		(iterations (truncate length 2)))
	   (dotimes (i iterations)
	     (rotatef (elt reversed-vector i) (elt reversed-vector (- length 1 i))))
	   reversed-vector))
	(t
	 (error "Sequence expected, got: ~S" sequence))))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  SUBSTITUTE
;;;  SUBSTITUTE-IF
;;;  SUBSTITUTE-IF-NOT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun %substitute-if (newitem test sequence start end from-end count key)
  (let (length)
    (multiple-value-setq (length end)
      (validate-bounding-indices sequence start end))
    (let ((predicate (if key
			 #'(lambda (elt) (funcall test (funcall key elt)))
			 test))
	  (result nil)
	  (subst-count 0)
	  (getter (if from-end
		      (%make-reverse-getter-iterator sequence)
		      (%make-getter-iterator sequence))))
      (dotimes (i (if from-end (- length end) start))
	(push (funcall getter) result))
      (dotimes (rep (- end start))
	(let ((elt (funcall getter)))
	  (cond ((funcall predicate elt)
		 (push newitem result)
		 (when (and count (= count (incf subst-count)))
		   (dotimes (i (- end start (1+ rep)))
		     (push (funcall getter) result))
		   (return)))
		(t
		 (push elt result)))))
      (dotimes (i (if from-end start (- length end)))
	(push (funcall getter) result))
      (if (vectorp sequence)
	  (if from-end
	      (%make-vector-from-list result
				      (array-element-type sequence)
				      length)
	      (%make-vector-from-reversed-list result
					       (array-element-type sequence)
					       length))
	  (if from-end
	      result
	      (nreverse result))))))

(defun substitute (newitem olditem sequence 
			   &key (test #'eql) test-not
			        from-end (start 0) end count key)
  (when test-not 
    (setq test #'(lambda (x y) (not (funcall test-not x y)))))
  (%substitute-if newitem
		  #'(lambda (elt) (funcall test olditem elt))
		  sequence
		  start end from-end count key))

(defun substitute-if (newitem test sequence 
			      &key from-end (start 0) end count key)
  (%substitute-if newitem test sequence start end from-end count key))

(defun substitute-if-not (newitem test sequence 
				  &key from-end (start 0) end count key)
  (%substitute-if newitem
		  #'(lambda (elt) (not (funcall test elt)))
		  sequence
		  start end from-end count key))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  NSUBSTITUTE
;;;  NSUBSTITUTE-IF
;;;  NSUBSTITUTE-IF-NOT
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun %nsubst-vector-if (newitem predicate sequence start end count)
  (do ((i start (1+ i))
       (subst-count 0))
      ((= i end))
    (when (funcall predicate (aref sequence i))
      (setf (aref sequence i) newitem)
      (when (and count (= count (incf subst-count)))
	(return)))))

(defun %nsubst-list-if (newitem predicate list start end count)
  (do ((cell (nthcdr start list) (cdr cell))
       (i start (1+ i))
       (subst-count 0))
      ((= i end))
    (when (funcall predicate (car cell))
      (rplaca cell newitem)
      (when (and count (= count (incf subst-count)))
	(return)))))

(defun %nsubstitute-if (newitem test sequence start end from-end count key)
  (if (and from-end count)
      (%substitute-if newitem test sequence start end from-end count key)
      (let (length
	    (predicate (if key
			 #'(lambda (elt) (funcall test (funcall key elt)))
			 test)))
			(declare (ignore length))
	(multiple-value-setq (length end)
	  (validate-bounding-indices sequence start end))
	(if (vectorp sequence)
	    (%nsubst-vector-if newitem predicate sequence start end count)
	    (%nsubst-list-if newitem predicate sequence start end count))
	sequence)))

(defun nsubstitute (newitem olditem sequence 
			   &key (test #'eql) test-not
			        from-end (start 0) end count key)
  (when test-not 
    (setq test #'(lambda (x y) (not (funcall test-not x y)))))
  (%nsubstitute-if newitem
		   #'(lambda (elt) (funcall test olditem elt))
		   sequence
		   start end from-end count key))

(defun nsubstitute-if (newitem test sequence 
			      &key from-end (start 0) end count key)
  (%nsubstitute-if newitem test sequence start end from-end count key))

(defun nsubstitute-if-not (newitem test sequence 
				  &key from-end (start 0) end count key)
  (%nsubstitute-if newitem
		   #'(lambda (elt) (not (funcall test elt)))
		   sequence
		   start end from-end count key))

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  REDUCE
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun reduce (function sequence 
						&key (key #'identity)
						from-end
						(start 0)
						end
						(initial-value nil initial-value-present-p))
  (let (length)
    (multiple-value-setq (length end)
      (validate-bounding-indices sequence start end))
    (let* ((subseq-length (- end start))
		   (reduce-steps (if initial-value-present-p
							 subseq-length
							 (1- subseq-length)))
		   (canonical-fun (if from-end
							  #'(lambda (a b) (funcall function b a))
							  function)))
      (cond ((= 0 subseq-length)
			 (if initial-value-present-p
				 initial-value
				 (funcall function)))
			((= 1 subseq-length)
			 (let ((elt (elt sequence start)))
			   (if initial-value-present-p
				   (funcall canonical-fun initial-value (funcall key elt))
				   (funcall key elt))))
			((listp sequence)
			 (let* ((sublist (if from-end
								 (nthcdr (- length end) (reverse sequence))
								 (nthcdr start sequence)))
					(value (if initial-value-present-p
							   initial-value
							   (funcall key (pop sublist)))))
			   (dotimes (s reduce-steps value)
				 (setq value (funcall canonical-fun
									  value
									  (funcall key (pop sublist)))))))
			((vectorp sequence)
			 (let* ((step (if from-end -1 1))
					(index (if from-end (1- end) start))
					(value (if initial-value-present-p
							   initial-value
							   (prog1 (funcall key (aref sequence index))
								 (setq index (+ index step))))))
			   (dotimes (s reduce-steps value)
				 (setq value (funcall canonical-fun
									  value
									  (funcall key (aref sequence index))))
				 (setq index (+ index step)))))
			(t
			 (error "Not a sequence: ~S" sequence))))))

;;; RGC  Redefining this here, because the earlier loaded version
;;; had an error. Not sure why...
(defun %make-setter-iterator (sequence &optional (start 0))
  (cond ((null sequence) #'(lambda (value)(declare (ignore value))))
	((listp sequence)
	 (let ((s (nthcdr start sequence)))
	   #'(lambda (value)
	       (rplaca s value)
	       (setq s (cdr s)))))
	(t
	 (let ((index (1- start)))
	   #'(lambda (value)
	       (setf (elt sequence (incf index)) value))))))

;;;
;;;  Common Lisp (SETF SUBSEQ) function
;;;
(defun (setf subseq) (value sequence start &optional end)
	(unless (sequencep value)
		(error "Not a sequence: ~S" value))
	(let* ((length (length sequence))
		   (value-length (length value))
		   (end (min (or end length) (+ value-length start) length))
		   (elements (- end start)))
		(unless (<= 0 start end)
			(error "Invalid START = ~S, END = ~S arguments" start end))
		(if (vectorp sequence)
			(dotimes (i elements)
				(setf (elt sequence (+ i start)) (elt value i)))
			(let ((x (nthcdr start sequence)))
				(dotimes (i elements)
					(setf (car x) (elt value i))
					(setq x (cdr x))))))
	  value)
