;; Meta class to provide :class allocation slots.
(in-package :common-lisp)

(defmethod shared-initialize ((instance standard-object) 
                              slot-names &rest all-keys)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
            (get-properties
              all-keys (slot-definition-initargs slot))
         (declare (ignore init-key))
         (if foundp
             (setf (slot-value instance slot-name) init-value)
             (when (and (not (slot-boundp instance slot-name))
							(instance-slot-p slot)
                        (not (null (slot-definition-initfunction slot)))
                        (or (eq slot-names t)
                            (member slot-name slot-names)))
               (setf (slot-value instance slot-name)
                     (funcall (slot-definition-initfunction slot))))))))
  instance)

(defclass class-slot-class (standard-class)
     ((class-allocated-slot-values
        :initform ()
        :accessor class-allocated-slots)))

(defun class-slot-p (slot)
  (eq (slot-definition-allocation slot) ':class))

(defvar unbound-class-slot (list "unbound class slot"))

(defmethod initialize-instance :after
           ((class class-slot-class) &key)
	(setf (class-allocated-slots class)
		(mapcar
			#'(lambda (slot)
				(let ((initfunction
							(slot-definition-initfunction slot)))
					(cons (slot-definition-name slot)
						(if (not (null initfunction))
							(funcall initfunction)
							unbound-class-slot))))
			(remove-if-not #'class-slot-p 
				(class-direct-slots class)))))

(defun class-slot-value (class slot-name)
  (dolist (super (class-precedence-list class))
     (let ((slot (find slot-name (class-direct-slots super)
                       :key #'slot-definition-name)))
        (when slot
           (let ((value (cdr (assoc slot-name
                                    (class-allocated-slots super)))))
             (when (eq value secret-unbound-value)
                (error "Unbound class slot named ~A in class ~S."
                       slot-name class))
             (return-from class-slot-value value))))))

(defun (setf class-slot-value) (new-value class slot-name)
  (block class-slot-value
    (dolist (super (class-precedence-list class))
      (let ((slot (find slot-name (class-direct-slots super)
                        :key #'slot-definition-name)))
        (when slot
          (setf (cdr (assoc slot-name
                            (class-allocated-slots super)))
                new-value)
          (return-from class-slot-value new-value))))))

(defun class-slot-boundp (class slot-name)
  (dolist (super (class-precedence-list class))
     (let ((slot (find slot-name (class-direct-slots super)
                       :key #'slot-definition-name)))
        (when slot
           (let ((value (cdr (assoc slot-name
                                    (class-allocated-slots super)))))
             (return-from class-slot-boundp 
                          (eq value secret-unbound-value)))))))

(defun class-slot-makunbound (class slot-name)
  (dolist (super (class-precedence-list class))
     (let ((slot (find slot-name (class-direct-slots super)
                       :key #'slot-definition-name)))
        (when slot
           (setf (cdr (assoc slot-name
                             (class-allocated-slots super)))
                 secret-unbound-value)
           (return-from class-slot-makunbound)))))

(defmethod slot-value-using-class
           ((class class-slot-class)
            instance slot-name)
  (let ((slot (find slot-name (class-slots class)
                    :key #'slot-definition-name)))
    (if (and slot (class-slot-p slot))
        (class-slot-value class slot-name)
        (call-next-method))))

(defmethod (setf slot-value-using-class)
            (new-value
             (class class-slot-class)
              instance slot-name)
  (let ((slot (find slot-name (class-slots class)
                    :key #'slot-definition-name)))
    (if (and slot (class-slot-p slot))
        (setf (class-slot-value class slot-name) new-value)
        (call-next-method))))

(defmethod slot-boundp-using-class
           ((class class-slot-class)
            instance slot-name)
  (let ((slot (find slot-name (class-slots class)
                    :key #'slot-definition-name)))
    (if (and slot (class-slot-p slot))
        (class-slot-boundp class slot-name)
        (call-next-method))))

(defmethod slot-makunbound-using-class
           ((class class-slot-class)
            instance slot-name)
  (let ((slot (find slot-name (class-slots class)
                    :key #'slot-definition-name)))
    (if (and slot (class-slot-p slot))
        (progn (class-slot-makunbound class slot-name)
               instance)
        (call-next-method))))

(in-package :common-lisp-user)