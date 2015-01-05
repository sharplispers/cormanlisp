;;;; test-sequences.lisp

;;;; Copyright (C) 1998 Vassili Bykov <vassili@objectpeople.com>
;;;; Created: 12/27/1998

;;;; Tests for the functions defined in sequences.lisp Corman Lisp
;;;; module.  The tests are mostly based on the examples given in the
;;;; HyperSpec.

(require "TESTKIT")

(in-package :common-lisp-user)

(defun vector= (v1 v2)
  (and (vectorp v1)
       (vectorp v2)
       (equal (coerce v1 'list)
	      (coerce v2 'list))))

(define-test-suite test-position ()
  ;; Test vector and list cases separately (separate code)
  ;; Test (from-end nil) and (from-end t) separately.
  (position #\a "baobab") => 1
  (position #\a "baobab" :from-end t) =>  4
  (position #\b "baobab" :start 1) => 3
  (position #\b "baobab" :start 4 :end 1) => :error
  (position #\b "baobab" :start nil) => :error
  (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car) =>  2
  (position-if #'oddp '(1 2 3 4 5 6 7 8) :start 1 :end 6 :from-end t)
  => 4
  (position 595 '()) =>  NIL
  (position-if-not #'integerp '(1 2 3 4 5.0)) =>  4
  )

(define-test-suite test-count ()
  (count #\a "how many A's are there in here?") =>  2
  (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car) =>  2
  (count-if #'upper-case-p "The Crying of Lot 49" :start 4) =>  2)

(define-test-suite test-fill ()
  (fill (list 0 1 2 3 4 5) '(444)) => ((444) (444) (444) (444) (444) (444))
  (fill (copy-seq "01234") #\e :start 3) =>  "012ee"
  (setq x (vector 'a 'b 'c 'd 'e)) =>  (lambda (r) (vector= r #(A B C D E)))
  (fill x 'z :start 1 :end 3) =>  (lambda (r) (vector= r #(A Z Z D E)))
  x =>  (lambda (r) (vector= r #(A Z Z D E)))
  (fill x 'p) =>  (lambda (r) (vector= r #(P P P P P)))
  x =>  (lambda (r) (vector= r #(P P P P P)))
  )

(define-test-suite test-replace ()
  (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4)
    => "abcd456hij"
  (setq lst "012345678") => "012345678"
  (replace lst lst :start1 2 :start2 0) => "010123456"
  lst => "010123456")

(define-test-suite test-mismatch ()
  ;; HyperSpec examples
  (mismatch "abcd" "ABCDE" :test #'char-equal) =>  4
  (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) =>  3
  (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp) =>  NIL
  (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4) =>  NIL
  ;; test bounds handling
  (mismatch '(1 2 3 4 5 6 7) '(2 3 4 5 777 888) :start1 1) => 5
  (mismatch '(1 2 3 4 5 6 7) '(2 3 4 5 777 888) :start1 1 :end2 3) => 4
  (mismatch '(1 2 3 4 5 6 7) '(0 5 6 7) :start1 2 :from-end t) => 4
  (mismatch '(1 2 3 4 5 6 7) '(0 5 6 7) :start1 4  :start2 1 :from-end t) => NIL
  )

(define-test-suite test-search ()
  (search "dog" "it's a dog's life living a dog's life") =>  7
  ;; Test various boundary cases
  (search "multiple-value" "mul") => nil
  (search "moo" "multiple-value") => nil
  (search "mul" "multiple-value" :from-end t) => 0
  (search "non" "sauvignon") => 6
  (search "foobar" "foobar") => 0
  ;; Test :START and :END handling
  (search "foo" "012foo678foo2345foo89" :start2 4 :end2 12) => 9
  (search "foo" "012foo678foo2345foo89" :start2 3 :end2 12) => 3
  (search "foo" "012foo678foo2345foo89" :start2 3 :end2 12 :from-end t) => 9
  (search "foo" "012foo678foo2345foo89" :start2 3 :end2 11 :from-end t) => 3
  (search "foo" "012foo678foo2345foo89" :start2 4 :end2 11 :from-end t) => nil
  (search "kaboom" "abookabookaboom" :start1 1) => 10
  (search "kaboom" "abookabookaboom" :start1 1 :end1 5) => 0
  (search "kaboom" "abookabookaboom" :start1 1 :end1 6) => 10
  (search "kaboom" "abookabookaboom" :end1 5) => 4
  (search "dog" "it's a dog's life living a dog's life" :from-end t) =>  27
  (search '(0 1) '(2 4 6 1 3 5) :key #'oddp) =>  2
  )

(define-test-suite test-remove ()
  (remove 4 '(1 3 4 5 9)) =>  (1 3 5 9)
  (remove 4 '(1 2 4 1 3 4 5)) =>  (1 2 1 3 5)
  (remove 4 #(1 2 4 1 3 4 5)) =>  (lambda (v) (vector= v #(1 2 1 3 5)))
  (remove 4 '()) => nil
  (remove 4 '(4)) => nil
  (remove 2 '(1 2 1 2 1 2 1 2) :start 2 :end 6) =>  (1 2 1 1 1 2)
  (remove 2 '(1 2 1 2 1 2 1 2) :start 2 :end 6 :from-end t) =>  (1 2 1 1 1 2)
  (remove 2 '(1 2 1 2 1 2 1 2) :start 1 :end 6) =>  (1 1 1 1 2)
  (remove 2 '(1 2 1 2 1 2 1 2) :start 1 :end 6 :from-end t) =>  (1 1 1 1 2)
  (remove 2 '(1 2 1 2 1 2 1 2) :start 2 :end 5) =>  (1 2 1 1 2 1 2)
  (remove 2 '(1 2 1 2 1 2 1 2) :start 2 :end 5 :from-end t) =>  (1 2 1 1 2 1 2)
  (remove 4 '(1 2 4 1 3 4 5) :count 1) =>  (1 2 1 3 4 5)
  (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t) =>  (1 2 4 1 3 5)
  (remove 3 '(1 2 4 1 3 4 5) :test #'>) =>  (4 3 4 5)
  (remove-if #'oddp '(1 2 4 1 3 4 5)) =>  (2 4 4)
  (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t) =>  (1 2 4 1 3 5)
  (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
  =>  (1 2 3 4 5 6 8))

(define-test-suite test-delete ()
  (setq lst '(list of four elements))
  (setq lst2 (copy-seq lst))
  (setq lst3 (delete 'four lst)) =>  (LIST OF ELEMENTS)
  (eq lst3 lst) => #'identity		; for my implementation --VB
  (equal lst lst2) => nil		; ditto
  (setq tester (list 'list 'of 'four 'elements))
  (delete 'list tester) => (OF FOUR ELEMENTS)
  (setq tester (list 'list 'of 'four 'elements))
  (delete 'elements tester) => (LIST OF FOUR)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 4 tester) =>  (1 2 1 3 5)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 4 tester :count 1) =>  (1 2 1 3 4 5)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 4 tester :count 1 :from-end t) =>  (1 2 4 1 3 5)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 3 tester :test #'>) =>  (4 3 4 5)
  ;; Test :START keyword handling
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 4 tester :start 3) =>  (1 2 4 1 3 5)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 4 tester :start 2) =>  (1 2 1 3 5)
  ;; Test :END keyword handling
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 4 tester :end 5) =>  (1 2 1 3 4 5)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete 4 tester :end 6) =>  (1 2 1 3 5)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete-if #'oddp tester) =>  (2 4 4)
  (setq tester (list 1 2 4 1 3 4 5))
  (delete-if #'evenp tester :count 1 :from-end t) =>  (1 2 4 1 3 5)    
  (setq tester (list 1 2 3 4 5 6))
  (delete-if #'evenp tester) =>  (1 3 5) 
  )

(define-test-suite test-remove-duplicates ()
  (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t) =>  "aBcD"
  (remove-duplicates '(a b c b d d e)) =>  (A C B D E)
  (remove-duplicates '(a b c b d d e) :from-end t) =>  (A B C D E)
  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
		     :test #'char-equal :key #'cadr)
  =>  ((BAR #\%) (BAZ #\A))
  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A)) 
		     :test #'char-equal :key #'cadr :from-end t)
  =>  ((FOO #\a) (BAR #\%))
  (setq tester (list 0 1 2 3 4 5 6))
  (delete-duplicates tester :key #'oddp :start 1 :end 6) =>  (0 4 5 6))

(define-test-suite test-reverse ()
  (setq str "abc") =>  "abc"
  (reverse str) =>  "cba"
  str =>  "abc"
  (setq str (copy-seq str)) =>  "abc"
  (nreverse str) =>  "cba"
  (setq l (list 1 2 3)) =>  (1 2 3)
  (nreverse l) =>  (3 2 1))

(define-test-suite test-merge ()
  (setq test1 (list 1 3 4 6 7))
  (setq test2 (list 2 5 8))
  (merge 'list test1 test2 #'<) =>  (1 2 3 4 5 6 7 8)
  (setq test1 (copy-seq "BOY"))
  (setq test2 (copy-seq "nosy"))
  (merge 'string test1 test2 #'char-lessp) =>  "BnOosYy"
  (setq test1 (vector '(red . 1) '(blue . 4)))
  (setq test2 (vector '(yellow . 2) '(green . 7)))
  (merge 'vector test1 test2 #'< :key #'cdr) 
  =>  (lambda (result)
	(vector= result #((RED . 1) (YELLOW . 2) (BLUE . 4) (GREEN . 7))))
  ;; (merge '(vector * 4) '(1 5) '(2 4 6) #'<) => :error
  )

(define-test-suite test-sort ()
   (setq tester (copy-seq "lkjashd")) =>  "lkjashd"
   (sort tester #'char-lessp) =>  "adhjkls"
   (setq tester (list '(1 2 3) '(4 5 6) '(7 8 9)))
   (sort tester #'> :key #'car)  =>  ((7 8 9) (4 5 6) (1 2 3)) 
   (setq tester (list 1 2 3 4 5 6 7 8 9 0))
   (stable-sort tester #'(lambda (x y) (and (oddp x) (evenp y))))
   => (1 3 5 7 9 2 4 6 8 0)
   (sort (setq committee-data
             (vector (list (list "JonL" "White") "Iteration")
                     (list (list "Dick" "Waters") "Iteration")
                     (list (list "Dick" "Gabriel") "Objects")
                     (list (list "Kent" "Pitman") "Conditions")
                     (list (list "Gregor" "Kiczales") "Objects")
                     (list (list "David" "Moon") "Objects")
                     (list (list "Kathy" "Chapman") "Editorial")
                     (list (list "Larry" "Masinter") "Cleanup")
                     (list (list "Sandra" "Loosemore") "Compiler")))
       #'string< :key #'cadar)
   => (lambda (result)
	(vector= result
		 #((("Kathy" "Chapman") "Editorial")
		   (("Dick" "Gabriel") "Objects")
		   (("Gregor" "Kiczales") "Objects")
		   (("Sandra" "Loosemore") "Compiler")
		   (("Larry" "Masinter") "Cleanup")
		   (("David" "Moon") "Objects")
		   (("Kent" "Pitman") "Conditions")
		   (("Dick" "Waters") "Iteration")
		   (("JonL" "White") "Iteration"))))
   (setq committee-data 
       (stable-sort committee-data #'string< :key #'cadr))
   => (lambda (result)
	(vector= result
		 #((("Larry" "Masinter") "Cleanup")
		   (("Sandra" "Loosemore") "Compiler")
		   (("Kent" "Pitman") "Conditions")
		   (("Kathy" "Chapman") "Editorial")
		   (("Dick" "Waters") "Iteration")
		   (("JonL" "White") "Iteration")
		   (("Dick" "Gabriel") "Objects")
		   (("Gregor" "Kiczales") "Objects")
		   (("David" "Moon") "Objects"))))
   )

(define-test-suite test-substitute ()
  (substitute #\. #\SPACE "0 2 4 6") =>  "0.2.4.6"
  (substitute 9 4 '(1 2 4 1 3 4 5)) =>  (1 2 9 1 3 9 5)
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1) =>  (1 2 9 1 3 4 5)
  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)=>  (1 2 4 1 3 9 5)
  (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>) =>  (9 9 4 9 3 4 5)
  (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  =>  ((1) (2) (3) 0)
  (substitute-if 9 #'oddp '(1 2 4 1 3 4 5)) =>  (9 2 4 9 9 4 9)
  (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  =>  (1 2 4 1 3 9 5))

(define-test-suite test-nsubstitute ()
  (setq some-things (list 'a 'car 'b 'cdr 'c))
  (nsubstitute-if "function was here" #'fboundp some-things
		  :count 1)
  =>  (A "function was here" B CDR C)
  some-things =>  (A "function was here" B CDR C)
  (setq alpha-tester (copy-seq "ab ")) =>  "ab "
  (nsubstitute-if-not #\z #'alpha-char-p alpha-tester) =>  "abz"
  alpha-tester =>  "abz"
  )

(define-test-suite test-reduce ()
  (reduce #'* '(1 2 3 4 5)) =>  120
  (reduce #'append '((1) (2)) :initial-value '(i n i t)) =>  (I N I T 1 2)
  (reduce #'append '((1) (2)) :from-end t                  
	  :initial-value '(i n i t)) =>  (1 2 I N I T) 
  (reduce #'- '(1 2 3 4)) =>  -8
  (reduce #'- '(1 2 3 4) :from-end t)    ;Alternating sum.
  =>  -2
  (reduce #'+ '()) =>  0
  (reduce #'+ '(3)) =>  3
  (reduce #'+ '(foo)) =>  FOO (reduce #'list '(1 2 3 4)) =>  (((1 2) 3) 4)
  (reduce #'list '(1 2 3 4) :from-end t) =>  (1 (2 (3 4)))
  (reduce #'list '(1 2 3 4) :initial-value 'foo) =>  ((((foo 1) 2) 3) 4)
  (reduce #'list '(1 2 3 4)
	  :from-end t :initial-value 'foo) =>  (1 (2 (3 (4 foo))))
  )


(defun test-sequences-module ()
  (apply #'+
   (list
    (test-count)
    (test-position)
    (test-fill)
    (test-replace)
    (test-mismatch)
    (test-search)
    (test-remove)
    (test-delete)
    (test-remove-duplicates)
    (test-merge)
    (test-sort)
    (test-reverse)
    (test-substitute)
    (test-nsubstitute)
    (test-reduce))))


