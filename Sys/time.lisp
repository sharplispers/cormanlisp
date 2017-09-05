;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		time.lisp
;;;;	Contents:	Common Lisp time functions.
;;;;	History:	RGC  12/1/96  Created.
;;;;				RGC  3/25/01  Fixed ENCODE-UNIVERSAL-TIME so it now defaults
;;;;							  to the current time zone.
;;;;				RGC 11/23/01  Fixed a problem with handling of local time
;;;;                              offsets and FORMAT-UNIVERSAL-TIME.
;;;;                RGC 9/8/04    DECODE-UNIVERSAL-TIME now handles daylight time
;;;;                              in a manner consistent with other common lisp implementations
;;;;                              (I couldn't understand the standard) and the time zone
;;;;                              does not include the adjustment for daylight time.
;;;;

(in-package :win32)

#! (:export t :library "KERNEL32" :ignore "APIENTRY" :pascal "WINAPI")
typedef struct _LARGE_INTEGER {
		DWORD LowPart;
		LONG HighPart;
} LARGE_INTEGER,*PLARGE_INTEGER;
BOOL WINAPI QueryPerformanceCounter(PLARGE_INTEGER);
BOOL WINAPI QueryPerformanceFrequency(PLARGE_INTEGER);
!#

(in-package :common-lisp)

(defconstant internal-run-time-buffer (ct:malloc (ct:sizeof 'win:LARGE_INTEGER)))
(defconstant internal-time-units-buffer (ct:malloc (ct:sizeof 'win:LARGE_INTEGER)))

(defun large-integer-to-lisp-integer (large-integer)
    (let* ((low  (ct:cref win:LARGE_INTEGER large-integer win::LowPart ))
           (high (ct:cref win:LARGE_INTEGER large-integer win::HighPart)))
        (if (>= high 0)
            (+ low (* high #x100000000))
            (let ((total (+ low (* high #x100000000))))
                (- (+ 1 (lognot total)))))))
            
(defun cl::get-internal-run-time ()
    (win:QueryPerformanceCounter internal-run-time-buffer)
    (large-integer-to-lisp-integer internal-run-time-buffer))

(defun cl::get-internal-time-units-per-second ()
    (win:QueryPerformanceFrequency internal-time-units-buffer)
    (large-integer-to-lisp-integer internal-time-units-buffer))

(defvar calibration-value -1)

;;;; calibrate the timer
(defun calibrate-timer ()
    (let ((tm (get-internal-run-time)) 
  	      (result-time))
	(setq tm (- (get-internal-run-time) tm))
	(setq result-time (/ (float tm) internal-time-units-per-second))
	(setq calibration-value result-time)))
	
(defmacro time (x)
	(let ()
        `(progn (calibrate-timer)
    		(let* ((tm (get-internal-run-time)) 
    			    (gtm cl::*gc-time-counter*)
    			    (result-time)
    			    (result-gc-time)
    			    ret)
    			(setq ret (multiple-value-list ,x))
    			(setq tm (- (get-internal-run-time) tm))
    			(setq gtm (- cl::*gc-time-counter* gtm))
    			(setq result-time (/ (float tm) internal-time-units-per-second))
    			(setq result-gc-time (/ (float gtm) gc-time-units-per-second))
    			(format t "Total Execution time: ~A seconds~%" (max (- result-time calibration-value) 0))
    			(format t "Time spent garbage collecting: ~A seconds~%" result-gc-time)
    			(values-list ret)))))	

(defconstant file-time-1900-01-01 9435484800)

(defun file-time-to-universal-time (ftime)
	(- (truncate ftime 10000000) file-time-1900-01-01))

(defun universal-time-to-file-time (utime)
	(* (+ utime  file-time-1900-01-01) 10000000))
	
;;;
;;; Common Lisp GET-UNIVERSAL-TIME function.
;;;
(defun get-universal-time ()
	(file-time-to-universal-time
		(apply #'system-time-to-file-time (get-system-time))))

;;;
;;;	Common Lisp DECODE-UNIVERSAL-TIME function.
;;;	Usage: (DECODE-UNIVERSAL-TIME universal-time &optional time-zone)
;;;		=> second, minute, hour, date, month, year, day, daylight-p, zone
;;;
(defun decode-universal-time (utime &optional time-zone)
	(let (stime
		  (second 0)
		  (minute 0)
		  (hour 0)
		  (date 0)
		  (month 0)
		  (year 0)
		  (day-of-week 0)
		  (daylight-p nil)
		  (zone time-zone))

		(unless (and (integerp utime) (>= utime 0))
			(error "Invalid universal time: ~A" utime))
		(multiple-value-bind (time-zone-offset daylight)
			(local-time-zone)
			(unless zone 
				(setq zone time-zone-offset)
				(setq daylight-p daylight)))
        
        ;; Apparently this function is supposed to return a time zone offset
        ;; which is independent of daylight time i.e. a value valid all year.
        ;; Therefore if daylight is true, we add an hour to the time zone offset to
        ;; get us to the correct value to return. Apparently daylight is always a
        ;; whole hour offset (hopefully).
        ;;
        (when daylight-p
            (incf zone)
            (if (>= zone 24)
                (decf zone 24)))
        
		(decf utime (* zone 3600))
		(setq stime (file-time-to-system-time 
						(universal-time-to-file-time utime)))
		(setq year (first stime))
		(setq month	(second stime))
		(setq day-of-week (- (third stime) 1))
		(if (= day-of-week -1) (setq day-of-week 6))
		(setq date (fourth stime))
		(setq hour (fifth stime))
		(setq minute (sixth stime))
		(setq second (seventh stime))
		(values second minute hour date month year day-of-week daylight-p zone)))

;;;
;;;	Common Lisp ENCODE-UNIVERSAL-TIME function.
;;;	Usage: (ENCODE-UNIVERSAL-TIME second minute hour date month year
;;;				&optional time-zone)
;;;	This varies from the standard slightly, in that two digit years
;;; are relative to the year 2000 rather than the current year.
;;;
(defun encode-universal-time (second minute hour date month year
			&optional time-zone)
	(unless time-zone (setf time-zone (local-time-zone)))
	(if (<= 0 year 99)
		(if (>= year 50) 
			(setq year (+ 1900 year))
			(setq year (+ 2000 year))))		
	(let ((utime (file-time-to-universal-time
			(system-time-to-file-time year month 0 
				date hour minute second 0))))
		(if time-zone (incf utime (* 3600 time-zone)))
		utime))

;;;
;;;	Common Lisp GET-DECODED-TIME function.
;;;
(defun get-decoded-time ()
	(decode-universal-time (get-universal-time)))

(defconstant month-names #("January" "February" "March" "April" "May"
	"June" "July" "August" "September" "October" "November" "December"))

(defvar local-time-zone-name nil)

;;;
;;;	Corman Lisp FORMAT-UNIVERSAL-TIME function.
;;;
(defun format-universal-time (time &optional (stream *standard-output*))
	(multiple-value-bind (second minute hour date
					month year day-of-week
					daylight-p time-zone)
		(decode-universal-time time)
		(declare (ignore second day-of-week time-zone))
		(format stream "~2,'0D:~2,'0D ~A ~A ~A, ~A ~A~AT"
			(if (= hour 0) 12 (if (> hour 12) (- hour 12) hour))
			minute
			(if (= hour 0) "AM" (if (>= hour 12) "PM" "AM"))
			(aref month-names (- month 1))
			date
			year
			local-time-zone-name
			(if daylight-p "D" "S"))))

;;;
;;;	Returns three values: 
;;;    the bias (in hours)
;;;    daylight-flag (t if daylight time, nil if standard time)
;;;    time-zone-name (string)
;;;                    
(defun cl::local-time-zone ()
	(ct:with-fresh-foreign-block (tzi 'win:TIME_ZONE_INFORMATION)
		(ct:with-c-struct (s tzi win:TIME_ZONE_INFORMATION)
			(let ((result (win:GetTimeZoneInformation tzi)))
				(cond
					((or (= result win:TIME_ZONE_ID_INVALID) (= result win:TIME_ZONE_ID_UNKNOWN))
					 (values 0 nil "Unknown"))
					((= result win:TIME_ZONE_ID_STANDARD)
					 (unless local-time-zone-name
						(setf local-time-zone-name (ct:c-string-to-lisp-string win::StandardName)))
					 (values (truncate win::Bias 60)
							 nil 
							(ct:c-string-to-lisp-string win::StandardName)))
					((= result win:TIME_ZONE_ID_DAYLIGHT)
					 (unless local-time-zone-name
					 	(setf local-time-zone-name (ct:c-string-to-lisp-string win::DaylightName)))
					 (values (- (truncate win::Bias 60) 1)
							 t 
							(ct:c-string-to-lisp-string win::DaylightName))))))))


;;;
;;; Common Lisp GET-INTERNAL-REAL-TIME function.
;;;
(defun get-internal-real-time () (get-internal-run-time))

