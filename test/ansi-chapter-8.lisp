;;;;
;;;;	File:       ansi-chapter-8.lisp
;;;;   Contents:   Examples from the Hyperspec
;;;;

;;; STRUCTURES
(dotests DEFSTRUCT
	 (defstruct (door (:conc-name dr-)) knob-color width material) =>  DOOR
)
(dotests DEFSTRUCT
	 (setq my-door (make-door :knob-color 'red :width 5.0)) 
	=>  #S(DOOR :KNOB-COLOR RED :WIDTH 5.0 :MATERIAL NIL)
	 (dr-width my-door) =>  5.0
	 (setf (dr-width my-door) 43.7) =>  43.7
	 (dr-width my-door) =>  43.7

 	(defstruct person name age sex) => person
 	(defstruct (astronaut (:include person)
                       (:conc-name astro-))
    helmet-size
    (favorite-beverage 'tang)) => astronaut
)
(dotests DEFSTRUCT
	 (setq x (make-astronaut :name 'buzz
	                         :age 45.
	                         :sex t
	                         :helmet-size 17.5))
	=> #S( ASTRONAUT :NAME BUZZ :AGE 45 :SEX T :HELMET-SIZE 17.5 :FAVORITE-BEVERAGE TANG )
	 (person-name x) =>  BUZZ
	 (astro-name x) =>  BUZZ
	 (astro-favorite-beverage x) =>  TANG
 	(typep (make-astronaut) 'person) =>  true

 (defstruct (binop (:type list) :named (:initial-offset 2))
   (operator '? :type symbol)   
   operand-1
   operand-2) =>  BINOP
 (defstruct (annotated-binop (:type list)
                             (:initial-offset 3)
                             (:include binop))
  commutative associative identity) =>  ANNOTATED-BINOP
 (make-annotated-binop :operator '*
                       :operand-1 'x
                       :operand-2 5
                       :commutative t
                       :associative t
                       :identity 1)
   =>  (NIL NIL BINOP * X 5 NIL NIL NIL T T 1)

	(defstruct (binop (:type list) (:initial-offset 2))
	(operator '? :type symbol)	
	operand-1
	operand-2) =>  BINOP
	 (make-binop :operator '+ :operand-1 'x :operand-2 5)
	=>  (NIL NIL + X 5)
	 (make-binop :operand-2 4 :operator '*)
	=>  (NIL NIL * NIL 4)

	 (defstruct (binop (:type list) :named (:initial-offset 2))
	   (operator '? :type symbol)
	   operand-1
	   operand-2) =>  BINOP
	
	 (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (NIL NIL BINOP + X 5)
	 (make-binop :operand-2 4 :operator '*) =>  (NIL NIL BINOP * NIL 4)
	
	 (defstruct (binop (:type list) :named)
	   (operator '? :type symbol)
	   operand-1
	   operand-2) =>  BINOP
	 (make-binop :operator '+ :operand-1 'x :operand-2 5) =>  (BINOP + X 5)
	 (make-binop :operand-2 4 :operator '*) =>  (BINOP * NIL 4)
	 (defun binop-p (x)
	   (and (consp x) (eq (car x) 'binop))) =>  BINOP-P

	;;;
	;;; Example 1
	;;; define town structure type
	;;; area, watertowers, firetrucks, population, elevation are its components
	;;;
	 (defstruct town
	             area
	             watertowers
	             (firetrucks 1 :type fixnum)    ;an initialized slot
	             population 
	             (elevation 5128 :read-only t)) ;a slot that can't be changed
	=>  TOWN
)
(dotests DEFSTRUCT
	;create a town instance
	 (setq town1 (make-town :area 0 :watertowers 0)) =>  #S( TOWN :AREA 0 :WATERTOWERS 0 :FIRETRUCKS 1 :POPULATION NIL :ELEVATION 5128 )
	;town's predicate recognizes the new instance
	 (town-p town1) =>  true
	;new town's area is as specified by make-town
	 (town-area town1) =>  0
	;new town's elevation has initial value
	 (town-elevation town1) =>  5128
	;setf recognizes reader function
	 (setf (town-population town1) 99) =>  99
	 (town-population town1) =>  99
	;copier function makes a copy of town1
	 (setq town2 (copy-town town1)) => #S( TOWN :AREA 0 :WATERTOWERS 0 :FIRETRUCKS 1 :POPULATION 99 :ELEVATION 5128 ) 
	 (= (town-population town1) (town-population town2))  =>  true
	;since elevation is a read-only slot, its value can be set only
	;when the structure is created
	 (setq town3 (make-town :area 0 :watertowers 3 :elevation 1200))
	=>  #S( TOWN :AREA 0 :WATERTOWERS 3 :FIRETRUCKS 1 :POPULATION NIL :ELEVATION 1200 )
	;;;
	;;; Example 2
	;;; define clown structure type
	;;; this structure uses a nonstandard prefix
	;;;
	 (defstruct (clown (:conc-name bozo-))
	             (nose-color 'red)         
	             frizzy-hair-p polkadots) =>  CLOWN
)
(dotests DEFSTRUCT
	 (setq funny-clown (make-clown)) =>  #S(CLOWN)
	;use non-default reader name
	 (bozo-nose-color funny-clown) =>  RED        
	 (defstruct (klown (:constructor make-up-klown) ;similar def using other
	             (:copier clone-klown)              ;customizing keywords
	             (:predicate is-a-bozo-p))
	             nose-color frizzy-hair-p polkadots) =>  klown
	;custom constructor now exists
	 (fboundp 'make-up-klown) =>  true
	;;;
	;;; Example 3
	;;; define a vehicle structure type
	;;; then define a truck structure type that includes 
	;;; the vehicle structure
	;;;
	 (defstruct vehicle name year (diesel t :read-only t)) =>  VEHICLE
	 (defstruct (truck (:include vehicle (year 79)))
	             load-limit                          
	             (axles 6)) =>  TRUCK
	 (setq x (make-truck :name 'mac :diesel t :load-limit 17))
	=>  #S( TRUCK :NAME MAC :YEAR 79 :DIESEL T :LOAD-LIMIT 17 :AXLES 6 )
	;vehicle readers work on trucks
	 (vehicle-name x)
	=>  MAC
	;default taken from :include clause 
	 (vehicle-year x)
	=>  79 
	 (defstruct (pickup (:include truck))     ;pickup type includes truck
	             camper long-bed four-wheel-drive) =>  PICKUP
)
(dotests DEFSTRUCT
	 (setq x (make-pickup :name 'king :long-bed t)) 
		=>  #S( PICKUP :NAME KING :YEAR 79 :DIESEL T :LOAD-LIMIT NIL 
				:AXLES 6 :CAMPER NIL :LONG-BED T :FOUR-WHEEL-DRIVE NIL )
	
	;:include default inherited
	 (pickup-year x) =>  79
	;;;
	;;; Example 4
	;;; use of BOA constructors
	;;;
	 (defstruct (dfs-boa                      ;BOA constructors
	               (:constructor make-dfs-boa (a b c)) 
	               (:constructor create-dfs-boa
	                 (a &optional b (c 'cc) &rest d &aux e (f 'ff))))
	             a b c d e f) =>  DFS-BOA
)
(dotests DEFSTRUCT
	;a, b, and c set by position, and the rest are uninitialized
	 (setq x (make-dfs-boa 1 2 3)) =>  #S( DFS-BOA :A 1 :B 2 :C 3 :D NIL :E NIL :F NIL )
	 (dfs-boa-a x) =>  1
	;a and b set, c and f defaulted
	 (setq x (create-dfs-boa 1 2)) =>  #S( DFS-BOA :A 1 :B 2 :C 3 :D NIL :E NIL :F NIL )
	 (dfs-boa-b x) =>  2
	 (eq (dfs-boa-c x) 'cc) =>  true
	;a, b, and c set, and the rest are collected into d
	 (setq x (create-dfs-boa 1 2 3 4 5 6)) =>  #S( DFS-BOA :A 1 :B 2 :C 3 :D (4 5 6) :E NIL :F FF )
	 (dfs-boa-d x) =>  (4 5 6)
)

