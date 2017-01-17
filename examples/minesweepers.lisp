;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:		minesweepers.lisp
;;;;	Contents:	This application uses AI techniques including neural networks and
;;;;                genetic algorithms, to create minesweepers which learn to find and
;;;;                dispose of mines on the field.
;;;;
;;;;                This is based on an application by Mat Buckland, from 2002, which was written in C++
;;;;                and published online.
;;;;					
;;;;				example:
;;;;					(load "examples/minesweepers.lisp")
;;;;					(save-application "minesweepers" #'win:minesweepers :static t)
;;;;
;;;;				If you run this from the IDE, be sure to run
;;;;				it in a separate thread, since it calls EXIT-THREAD.
;;;;
;;;;				example:
;;;;					(th:create-thread #'win:minesweepers)
;;;;
;;;;	Author:		Roger Corman
;;;;

(in-package :win32)
(require 'wingdi)
(export 'minesweepers)

(defconstant szAppName "Minesweepers")
(defconstant NULL cl::C_NULL)
(defvar *messages-processed* 0)
(defvar *ps* nil)
(defvar *rect* nil)
(defvar hwnd-save nil)
(defvar ps-save nil)
(defvar imsg-save nil)
(defvar *app-window* nil)
(defvar *timer-id* 1)
(defvar *hdc* nil)
(defvar *height* 800)
(defvar *width* 800)
(defvar *refresh-milliseconds* 20)

(defvar *hdc-backbuffer* NULL)
(defvar *bitmap* NULL)
(defvar *old-bitmap* NULL)
(defvar *controller* nil)

;;; parameters
;;  general parameters
(defvar *half-pi* (/ pi 2))
(defvar *two-pi* (* pi 2))
(defvar *frames-per-second* 60)
  
;; used for the neural network
(defvar *num-inputs* 4)
(defvar *num-hidden* 1)
(defvar *neurons-per-hidden-layer* 6)
(defvar *num-outputs* 2)
(defvar *activation-response* 1d0)      ; for tweeking the sigmoid function
(defvar *bias* -1d0) ; bias value

;; used to define the sweepers
(defvar *max-turn-rate* 0.3d0)          ; limits how fast the sweepers can turn
(defvar *max-speed* 2d0)
(defvar *sweeper-scale* 5)              ; for controlling the size

;; controller parameters
(defvar *num-sweepers* 30)
(defvar *num-mines* 40)
(defvar *num-ticks* 2000)               ; number of time steps we allow for each generation to live
(defvar *mine-scale* 2d0)               ; scaling factor for mines

;; genetic algorithm parameters
(defvar *crossover-rate* 0.7d0)
(defvar *mutation-rate* 0.1d0)
(defvar *max-perturbation* 0.3d0)       ; the maximum amount the GA may mutate each weight by

;; used for elitism
(defvar *num-elite* 4)
(defvar *num-copies-elite* 1)

(defun install-refresh-timer ()
	(win:SetTimer *app-window* *timer-id* *refresh-milliseconds* NULL))
 
(defun uninstall-refresh-timer ()
	(win:KillTimer *app-window* *timer-id*)) 

(defstruct point x y)                   ; point struct to store shape vertices
(defstruct vector2d x y)                ; 2-dimensional vector
(defstruct sweeper vertices)            ; list of vertices
(defstruct mine vertices)               ; list of vertices

(defstruct neuron
    num-inputs                          ; the number of inputs into the neuron
    weights)                            ; the weights for each input (vector)

(defstruct neuron-layer
    num-neurons                         ; the number of neurons in this layer
    neurons)                            ; the layer of neurons (vector)

(defstruct neural-net
    num-inputs
    num-outputs
    num-hidden-layers
    neurons-per-hidden-layer
    layers)                             ; storage for each layer of neurons including the output layer (vector)

(defstruct genome 
    weights                             ; vector of weights
    fitness)                            ; double float fitness variable

(defstruct genetic-algorithm 
    population                          ; vector of genome
    population-size
    chromo-length                       ; amount of weights per chromo
    total-fitness                       ; total fitness of population
    best-fitness                        ; best fitness of this population
    average-fitness                     ; average fitness of this population
    worst-fitness
    fittest-genome                      ; keeps track of the best genome
    mutation-rate                       ; probability that a chromosones bits will mutate (Try figures around 0.05 to 0.3 ish)
    crossover-rate                      ; probability of chromosones crossing over bits (0.7 is pretty good)
    generation-counter)
        
(defun sweeper-vertices ()
  (list 
    (make-point :x -1d0 :y -1d0)
    (make-point :x -1d0 :y 1d0)
    (make-point :x -0.5d0 :y 1d0)
    (make-point :x -0.5d0 :y -1d0)
    
    (make-point :x 0.5d0 :y -1d0)
    (make-point :x 1d0 :y -1d0)
    (make-point :x 1d0 :y 1d0)
    (make-point :x 0.5d0 :y 1d0)
   
    (make-point :x -0.5d0 :y -0.5d0)
    (make-point :x 0.5d0 :y -0.5d0)
    
    (make-point :x -0.5d0 :y 0.5d0)
    (make-point :x -0.25d0 :y 0.5d0)
    (make-point :x -0.25d0 :y 1.75d0)
    (make-point :x 0.25d0 :y 1.75d0)
    (make-point :x 0.25d0 :y 0.5d0)
    (make-point :x 0.5d0 :y 0.5d0)))

(defun mine-vertices ()
  (list 
    (make-point :x -1d0 :y -1d0)
    (make-point :x -1d0 :y 1d0)
    (make-point :x 1d0 :y 1d0)
    (make-point :x 1d0 :y -1d0)))

(defstruct minesweeper
    brain                   ; the minesweeper's neural-net
    position                ; its position in the world (vector2d)
    direction               ; direction sweeper is facing (vector2d)
    rotation                ; its rotation
    speed
    ltrack                  ; to store output from the ANN
    rtrack
    fitness                 ; the sweeper's fitness score 
    scale                   ; the scale of the sweeper when drawn
    closest-mine)           ; index position of closest mine
           
(defstruct controller 
	population              ; storage for the population of genomes (vector)
    sweepers                ; the minesweepers (vector)
    mines                   ; the mines (vector)
    genetic-algorithm       ; pointer to the Genetic Algorithm struct
    num-sweepers
    num-mines
    num-weights-in-NN
    sweeper-shape           ; the sweeper shape's vertices (vector)
    mine-shape              ; vertex buffer for the mine shape's vertices (vector)
    average-fitness-vector  ; stores the average fitness per generation for use
    best-fitness-vector     ; stores the best fitness per generation
    
    ;; pens (HPEN) for the stats
    red-pen
    blue-pen
    green-pen
    old-pen
	
    hwnd-main               ; handle to the application window
    fast-render-mode        ; toggles the speed at which the simulation runs (if true, do fast render)
	ticks                   ; cycles per generation
    generation-counter
    
    window-width
    window-height)

(defun debug-msg (msg &rest args)
    (apply 'format *terminal-io* msg args)
    (terpri *terminal-io*)
    (force-output *terminal-io*)) 

;;;----------------------------------------------------------------------------
;;;	some random number functions.
;;;----------------------------------------------------------------------------

;;; returns a random integer between x and y
(defun rand-int (x y) (+ x (random (+ (- y x) 1))))

;;; returns a random float between zero and 1
(defun rand-float () (random 1d0))

;;; returns a random bool
(defun rand-bool ()
    (if (= (random 2) 0) nil t))

;;; returns a random float in the range -1 < n < 1
(defun random-clamped ()
    (- (rand-float)(rand-float)))

;;;
;;; clamps the first argument between the second two
;;;
(defun clamp (arg min max)
    (if (< arg min)
        min
        (if (> arg max)
            max
            arg)))
;;;
;;; Sigmoid function
;;;
(defun sigmoid (netinput response)
    (/ 1 (+ 1 (exp (/ (- netinput) response)))))
    
;;;
;;; Return the length of a 2D vector
;;;
(defun vector2d-length (v)
    (let ((x (vector2d-x v))
          (y (vector2d-y v)))
        (sqrt (+ (* x x) (* y y)))))

;;;
;;; Normalize a 2D vector
;;;
(defun vector2d-normalize (v)
    (let ((length (vector2d-length v)))
        (make-vector2d :x (/ (vector2d-x v) length)
                       :y (/ (vector2d-y v) length))))

;;;
;;; Add vector2d + vector2d
;;;
(defun vector2d+ (v1 v2)
    (make-vector2d :x (+ (vector2d-x v1)(vector2d-x v2))
                   :y (+ (vector2d-y v1)(vector2d-y v2))))

;;;
;;; Subtract vector2d - vector2d
;;;
(defun vector2d- (v1 v2)
    (make-vector2d :x (- (vector2d-x v1)(vector2d-x v2))
                   :y (- (vector2d-y v1)(vector2d-y v2))))

;;;
;;; Multiply a vector2d by a number
;;;
(defun vector2d-multiply (v c)
    (make-vector2d :x (* (vector2d-x v) c) :y (* (vector2d-y v) c)))

;;;
;;; matrix transformations
;;;
(defstruct matrix
    _11 _12 _13
	_21 _22 _23
	_31 _32 _33)

;;;
;;; Create an identity matrix
;;;
(defun create-matrix ()
    (make-matrix :_11 1 :_12 0 :_13 0
                          :_21 0 :_22 1 :_23 0
                          :_31 0 :_32 0 :_33 1))
     
;; multiply two matrices together
(defun matrix-multiply (m1 m2)
    (make-matrix
        ;; first row
        :_11 (+ (* (matrix-_11 m1) (matrix-_11 m2))(* (matrix-_12 m1) (matrix-_21 m2))(* (matrix-_13 m1) (matrix-_31 m2)))
        :_12 (+ (* (matrix-_11 m1) (matrix-_12 m2))(* (matrix-_12 m1) (matrix-_22 m2))(* (matrix-_13 m1) (matrix-_32 m2)))
        :_13 (+ (* (matrix-_11 m1) (matrix-_13 m2))(* (matrix-_12 m1) (matrix-_23 m2))(* (matrix-_13 m1) (matrix-_33 m2)))
        ;; second row
        :_21 (+ (* (matrix-_21 m1) (matrix-_11 m2))(* (matrix-_22 m1) (matrix-_21 m2))(* (matrix-_23 m1) (matrix-_31 m2)))
        :_22 (+ (* (matrix-_21 m1) (matrix-_12 m2))(* (matrix-_22 m1) (matrix-_22 m2))(* (matrix-_23 m1) (matrix-_32 m2)))
        :_23 (+ (* (matrix-_21 m1) (matrix-_13 m2))(* (matrix-_22 m1) (matrix-_23 m2))(* (matrix-_23 m1) (matrix-_33 m2)))
        ;; third row
        :_31 (+ (* (matrix-_31 m1) (matrix-_11 m2))(* (matrix-_32 m1) (matrix-_21 m2))(* (matrix-_33 m1) (matrix-_31 m2)))
        :_32 (+ (* (matrix-_31 m1) (matrix-_12 m2))(* (matrix-_32 m1) (matrix-_22 m2))(* (matrix-_33 m1) (matrix-_32 m2)))
        :_33 (+ (* (matrix-_31 m1) (matrix-_13 m2))(* (matrix-_32 m1) (matrix-_23 m2))(* (matrix-_33 m1) (matrix-_33 m2)))))

(defun matrix-translate (m x y)
    (let ((temp (make-matrix :_11 1 :_12 0 :_13 0
                             :_21 0 :_22 1 :_23 0
                             :_31 x :_32 y :_33 1)))
        (matrix-multiply m temp)))
    
(defun matrix-scale (m xscale yscale)
    (let ((temp (make-matrix :_11 xscale :_12 0      :_13 0
                             :_21 0      :_22 yscale :_23 0
                             :_31 0      :_32 0      :_33 1)))
        (matrix-multiply m temp)))

(defun matrix-rotate (m rotation)
    (let* ((sin (sin rotation))
           (cos (cos rotation))
           (temp (make-matrix :_11 cos    :_12 sin   :_13 0
                             :_21 (- sin) :_22 cos   :_23 0
                             :_31 0      :_32 0      :_33 1)))
        (matrix-multiply m temp)))

;; applies a 2D transformation matrix to a vector of points
(defun transform-points (points m)
    (dotimes (i (length points))
        (let ((tempx (+ (* (matrix-_11 m)(point-x (aref points i)))
                        (* (matrix-_21 m)(point-y (aref points i)))
                        (matrix-_31 m)))
              (tempy (+ (* (matrix-_12 m)(point-x (aref points i)))
                        (* (matrix-_22 m)(point-y (aref points i)))
                        (matrix-_32 m))))
            (setf (point-x (aref points i)) tempx
                  (point-y (aref points i)) tempy)))) 

;;
;;	Sets up the translation matrices for the mines and applies the
;;	world transform to each vertex in the vertex buffer passed to this
;;	method.
(defun world-transform (points pos scale rotation)
    (let ((m (create-matrix)))
        (setf m (matrix-scale m scale scale))
        (if (/= rotation 0)
            (setf m (matrix-rotate m rotation)))
        (setf m (matrix-translate m (vector2d-x pos)(vector2d-y pos)))
        (transform-points points m)
        points))

(defun create-neuron (num-inputs)
    ;; we need an additional weight for the bias hence the +1
    (let ((neuron (make-neuron :num-inputs (+ num-inputs 1) 
                               :weights (make-array (+ num-inputs 1)))))
        (dotimes (i (+ num-inputs 1))
            (setf (aref (neuron-weights neuron) i) (random-clamped)))
        neuron))

(defun create-neuron-layer (num-neurons num-inputs-per-neuron)
    (let ((neuron-layer (make-neuron-layer :num-neurons num-neurons 
                                           :neurons (make-array num-neurons))))
        (dotimes (i num-neurons)
            (setf (aref (neuron-layer-neurons neuron-layer) i) (create-neuron num-inputs-per-neuron)))
        neuron-layer))

(defun create-neural-net ()
    (let ((neural-net 
                (make-neural-net :num-inputs *num-inputs*
                                 :num-outputs *num-outputs*
                                 :num-hidden-layers *num-hidden*
                                 :neurons-per-hidden-layer *neurons-per-hidden-layer*
                                 :layers (make-array 0 :fill-pointer t))))
        
        ;; create the layers of the network
        (if (> *num-hidden* 0)
            (let* ((layers (neural-net-layers neural-net)))
                ;; create first hidden layer
                (vector-push-extend (create-neuron-layer *neurons-per-hidden-layer* *num-inputs*) layers)
                ;; create remaining hidden layers (num-inputs is different for subsequent hidden layers)
                (dotimes (i (- *num-hidden* 1))
                    (vector-push-extend (create-neuron-layer *neurons-per-hidden-layer* *neurons-per-hidden-layer*) layers))
                ;; create the output layer
                (vector-push-extend (create-neuron-layer *num-outputs* *neurons-per-hidden-layer*) layers))
            ;; else no hidden layers, create output layer only
            (vector-push-extend  (create-neuron-layer *num-outputs* *num-inputs*)
                (neural-net-layers neural-net)))
        neural-net))                               

;;;
;;; returns the total number of weights needed for the net
;;;
(defun neural-net-number-of-weights (neural-net)
    (let ((weights 0))
        ;; for each layer
        (dotimes (i (length (neural-net-layers neural-net)))
            (let ((layer (aref (neural-net-layers neural-net) i)))
                ;; for each neuron
                (dotimes (j (length (neuron-layer-neurons layer)))
                    (let ((neuron (aref (neuron-layer-neurons layer) j)))
                        ;; for each weight
                        (incf weights (length (neuron-weights neuron)))))))
        weights))

;;; get the neural net's weights vector
(defun neural-net-get-weights (neural-net) (declare (ignore neural-net)))

;;; set the neural net's weights vector
(defun neural-net-set-weights (neural-net vec-weights) 
    (let ((cweight 0))   
        ;; for each layer
        (dotimes (i (1+ (neural-net-num-hidden-layers neural-net)))
            (let ((layer (aref (neural-net-layers neural-net) i)))
                ;; for each neuron
                (dotimes (j (neuron-layer-num-neurons layer))
                    (let ((neuron (aref (neuron-layer-neurons layer) j)))
                        ;; for each weight
                        (dotimes (k (neuron-num-inputs neuron))
                            (setf (aref (neuron-weights neuron) k) (aref vec-weights cweight))
                            (incf cweight))))))
        nil))

;;;
;;; given an input vector this function calculates the output vector
;;;
(defun neural-net-update (neural-net inputs)
    (let ((outputs (make-array 0 :fill-pointer t))
          (weight 0))
        
        ;; first check that we have the correct amount of inputs
        (if (/= (length inputs) *num-inputs*)
            ;; just return an empty vector if incorrect
            (return-from neural-net-update outputs))
        
        ;; for each layer
        (dotimes (i (neural-net-num-hidden-layers neural-net))
            (let ((layer (aref (neural-net-layers neural-net) i)))
                (if (> i 0)
                    (setf inputs outputs))
                
                (setf (fill-pointer outputs) 0)  ;; clear the array
                (setf weight 0)
                
                ;; For each neuron sum the (inputs * corresponding weights).
                ;; Throw the total at our sigmoid function to get the output.
                (dotimes (j (neuron-layer-num-neurons layer))
                    (let* ((netinput 0)
                           (neuron (aref (neuron-layer-neurons layer) j))
                           (num-inputs (neuron-num-inputs neuron)))
                        
                        ;; for each weight
                        (dotimes (k (- num-inputs 1))
                            ;; sum the weights * inputs
                            (incf netinput (* (aref (neuron-weights neuron) k) (aref inputs weight)))
                            (incf weight))
                        
                        ;; add in the bias
                        (incf netinput (* (aref (neuron-weights neuron) (- *num-inputs* 1)) *bias*))
                        
                        ;; We can store the outputs from each layer as we generate them. 
                        ;; The combined activation is first filtered through the sigmoid function.
                        (vector-push-extend (sigmoid netinput *activation-response*) outputs)
                        
                        (setf weight 0)))))
        outputs))
                                                           
(defun create-genome (weights fitness)
    (make-genome :weights weights :fitness fitness))

(defun create-genetic-algorithm (pop-size mutation-rate crossover-rate num-weights)
    (let ((ga (make-genetic-algorithm
                    :population (make-array pop-size)
                    :population-size pop-size 
                    :mutation-rate mutation-rate
                    :crossover-rate crossover-rate
                    :chromo-length num-weights
                    :total-fitness 0d0
                    :best-fitness 0d0
                    :average-fitness 0d0
                    :worst-fitness 99999999d0
                    :fittest-genome 0d0
                    :generation-counter 0
                    )))
        	;; initialize population with chromosomes consisting of random weights and all fitnesses set to zero
        (dotimes (i pop-size)
            (let* ((weights (make-array num-weights))
                   (fitness 0d0)
                   (genome (create-genome weights fitness)))
                (dotimes (j num-weights)
                    (setf (aref weights j) (random-clamped)))
                (setf (aref (genetic-algorithm-population ga) i) genome)))
        ga))

;;; 
;;; resets all the relevant variables ready for a new generation
;;;
(defun genetic-algorithm-reset (ga)
    (setf (genetic-algorithm-total-fitness ga) 0
          (genetic-algorithm-best-fitness ga) 0
          (genetic-algorithm-worst-fitness ga) 9999999
          (genetic-algorithm-average-fitness ga) 0))

;;;
;;; calculates the fittest and weakest genome and the average/total fitness scores
;;;
(defun genetic-algorithm-calculate-fitness (ga)
    (setf (genetic-algorithm-total-fitness ga) 0)
    (let ((highest-so-far 0)
          (lowest-so-far 9999999))
        (dotimes (i (genetic-algorithm-population-size ga))
            (let ((genome (aref (genetic-algorithm-population ga) i)))
                ;; update fittest if necessary
                (when (> (genome-fitness genome) highest-so-far)
                    (setf highest-so-far (genome-fitness genome))
                    (setf (genetic-algorithm-fittest-genome ga) i)
                    (setf (genetic-algorithm-best-fitness ga) highest-so-far))
                ;; update worst if necessary
                (when (< (genome-fitness genome) lowest-so-far)
                    (setf lowest-so-far (genome-fitness genome))
                    (setf (genetic-algorithm-worst-fitness ga) lowest-so-far))
                
                (incf (genetic-algorithm-total-fitness ga) (genome-fitness genome))))
        (setf (genetic-algorithm-average-fitness ga) 
            (/ (genetic-algorithm-total-fitness ga)(genetic-algorithm-population-size ga))))) 

;;;
;;; This works like an advanced form of elitism by inserting num-copies
;;; copies of the nbest most fittest genomes into a population vector.
;;;
(defun genetic-algorithm-grab-n-best (ga nbest num-copies population)
    ;; add the required amount of copies of the n most fittest to the supplied vector
    (do ()
        ((= nbest 0))
        (decf nbest)
        (dotimes (i num-copies)
            (vector-push-extend (aref (genetic-algorithm-population ga) 
                    (- (1- (genetic-algorithm-population-size ga)) nbest)) population))))

;;;
;;; given parents and storage for the offspring this method performs
;;;	crossover according to the GAs crossover rate
;;;
(defun genetic-algorithm-crossover (ga mom dad baby1 baby2)
    (let ((cp 0))
        ;; just return parents as offspring dependent on the rate or if parents are the same
        (if (or (> (rand-float) (genetic-algorithm-crossover-rate ga))(eq mom dad))
            (setf cp (length mom)))         ; baby1 = mom, baby2 = dad
            (progn
                ;; determine a crossover point
                (setf cp (rand-int 0 (1- (genetic-algorithm-chromo-length ga)))))
                ;; create the offspring
                (dotimes (i cp)
                    (vector-push-extend (aref mom i) baby1)
                    (vector-push-extend (aref dad i) baby2))
                (do* ((i cp (+ i 1)))
                     ((>= i (length mom)))
                    (vector-push-extend (aref dad i) baby1)
                    (vector-push-extend (aref mom i) baby2))))
    
;;;
;;; returns a chromosome based on roulette wheel sampling
;;;
(defun genetic-algorithm-get-chromo-roulette (ga)
    (let* ((slice (* (rand-float) (genetic-algorithm-total-fitness ga)))
           (chosen (create-genome (make-array 0 :fill-pointer t) 0))
           (fitness-so-far 0))
        (dotimes (i (genetic-algorithm-population-size ga))
            (incf fitness-so-far (genome-fitness (aref (genetic-algorithm-population ga) i)))
            
            ;; if the fitness so far > random number return the chromosome at this point
            (when (>= fitness-so-far slice)
                (return-from genetic-algorithm-get-chromo-roulette 
                    (aref (genetic-algorithm-population ga) i))))
        chosen))

;;;
;;; mutates a chromosome by perturbing its weights by an amount not greater than *max-perturbation*
;;;
(defun genetic-algorithm-mutate (ga chromosomes)
    ;; traverse the chromosome and mutate each weight dependent on the mutation rate
    (dotimes (i (length chromosomes))
        ;; do we perturb this weight?
        (if (< (rand-float) (genetic-algorithm-mutation-rate ga))
            ;; add or subtract a small value to the weight
            (incf (aref chromosomes i) (* (random-clamped) *max-perturbation*)))))
    
(defun genetic-algorithm-epoch (ga old-population)
    (setf (genetic-algorithm-population ga) old-population)
    
    ;; reset the appropriate variables
    (genetic-algorithm-reset ga)
    
    ;; calculate best, worst, average and total fitness
    (genetic-algorithm-calculate-fitness ga)
    
    (let ((new-population (make-array 0 :fill-pointer t)))
        (setf *new-population* new-population)
        ;; Now to add a little elitism we shall add in some copies of the fittest genomes. 
        ;; Make sure we add an EVEN number or the roulette wheel sampling will crash.
        (if (evenp (* *num-copies-elite* *num-elite*))
            (genetic-algorithm-grab-n-best ga *num-elite* *num-copies-elite* new-population))
        
        ;; now we enter the GA loop
        (do ()
            ((>= (length new-population) (genetic-algorithm-population-size ga)))
            ;; grab two chromosones
            (let* ((mom (genetic-algorithm-get-chromo-roulette ga))
                   (dad (genetic-algorithm-get-chromo-roulette ga))
                   (baby1 (make-array 0 :fill-pointer t))
                   (baby2 (make-array 0 :fill-pointer t)))
                (genetic-algorithm-crossover ga (genome-weights mom) (genome-weights dad) baby1 baby2)

                ;; now we mutate
                (genetic-algorithm-mutate ga baby1)
                (genetic-algorithm-mutate ga baby2)
                
                ;; now copy into new population
                (vector-push-extend (make-genome :weights baby1 :fitness 0) new-population)
                (vector-push-extend (make-genome :weights baby2 :fitness 0) new-population)))

        new-population))
                                               
(defun create-minesweeper ()
    (let ((minesweeper (make-minesweeper
                    :rotation (* (rand-float) *two-pi*)
                    :ltrack 0.16d0
                    :rtrack 0.16d0
                    :fitness 0
                    :scale *sweeper-scale*
                    :closest-mine 0
                    :brain (create-neural-net))))
        ;; create a random start position
        (setf (minesweeper-position minesweeper)
            (make-vector2d :x (* (rand-float) *width*)
                        :y (* (rand-float) *height*)))
        (setf (minesweeper-direction minesweeper)
            (make-vector2d :x 0 :y 0))
        minesweeper))

;;;
;;; Resets the sweepers position, fitness and rotation
;;;           
(defun reset-minesweeper (minesweeper)
    ;; reset the sweeper's position
    (setf (minesweeper-position minesweeper)
            (make-vector2d :x (* (rand-float) *width*)
                           :y (* (rand-float) *height*)))
    ;; reset the fitness
    (setf (minesweeper-fitness minesweeper) 0d0)
    
    ;; reset the rotation
    (setf (minesweeper-rotation minesweeper)(* (rand-float) *two-pi*)))

(defun minesweeper-get-closest-mine (minesweeper mines)
    (let ((closest-so-far 99999)
          (closest-object (make-vector2d :x 0 :y 0)))
        ;; cycle through the mines to find closest
        (dotimes (i (length mines))
            (let ((len-to-object (vector2d-length (vector2d- (aref mines i) (minesweeper-position minesweeper)))))
                (when (< len-to-object closest-so-far)
                    (setf closest-so-far len-to-object)
                    (setf closest-object (vector2d- (minesweeper-position minesweeper) (aref mines i)))
                    (setf (minesweeper-closest-mine minesweeper) i))))
        closest-object))
                            
(defun minesweeper-update (minesweeper mines) 
    (let* ((inputs (make-array 0 :fill-pointer 0))
           (closest-mine (vector2d-normalize (minesweeper-get-closest-mine minesweeper mines))))  ; get the vector2d to the closest mine an normalize it
        
        ;; add in vector to closest mine
        (vector-push-extend (vector2d-x closest-mine) inputs)
        (vector-push-extend (vector2d-y closest-mine) inputs)
       
        ;; add in sweepers direction vector
        (vector-push-extend (vector2d-x (minesweeper-direction minesweeper)) inputs)
        (vector-push-extend (vector2d-y (minesweeper-direction minesweeper)) inputs)
         
        ;; update the brain and get feedback
        (let ((output (neural-net-update (minesweeper-brain minesweeper) inputs)))
            ;; make sure there were no errors in calculating the output
            (if (< (length output) *num-outputs*)
                (return-from minesweeper-update nil))
            
            ;; assign the outputs to the sweepers left & right tracks
            (setf (minesweeper-ltrack minesweeper) (aref output 0))
            (setf (minesweeper-rtrack minesweeper) (aref output 1))
            
            ;; calculate steering forces, clamping rotation
            (let ((rot-force 
                        (clamp (- (minesweeper-ltrack minesweeper) (minesweeper-rtrack minesweeper))
                            (- *max-turn-rate*) 
                            *max-turn-rate*)))
                
                (incf (minesweeper-rotation minesweeper) rot-force)
                (setf (minesweeper-speed minesweeper)(+ (minesweeper-ltrack minesweeper) (minesweeper-rtrack minesweeper)))
                
                ;; update direction
                (setf (minesweeper-direction minesweeper)
                    (make-vector2d :x (- (sin (minesweeper-rotation minesweeper)))
                                   :y (cos (minesweeper-rotation minesweeper))))
                
                ;; update position
                (setf (minesweeper-position minesweeper)
                    (vector2d+ (minesweeper-position minesweeper)
                        (vector2d-multiply (minesweeper-direction minesweeper) (minesweeper-speed minesweeper))))
                
                ;; wrap around window limits
                (let ((pos (minesweeper-position minesweeper)))
                    (if (> (vector2d-x pos) *width*)
                        (setf (vector2d-x pos) 0))
                    (if (< (vector2d-x pos) 0)
                        (setf (vector2d-x pos) *width*))
                    (if (> (vector2d-y pos) *height*)
                        (setf (vector2d-y pos) 0))
                    (if (< (vector2d-y pos) 0)
                        (setf (vector2d-y pos) *height*)))
                
                t))))
 
;;;
;;; Check whether the sweeper hit a mine. If so, return its index. Otherwise return nil.
;;;       
(defun minesweeper-check-for-mine (minesweeper mines size) 
    (let ((dist-to-object 
                (vector2d- (minesweeper-position minesweeper) 
                    (aref mines (minesweeper-closest-mine minesweeper)))))
        (if (< (vector2d-length dist-to-object) (+ size 5))
            (minesweeper-closest-mine minesweeper)
            nil))) 

(defun minesweeper-put-weights (minesweeper weights)
    (neural-net-set-weights (minesweeper-brain minesweeper) weights))   
         
(defun create-controller (hwnd) 
    (let ((controller (make-controller
                    :fast-render-mode nil
                    :ticks 0
                    :num-mines *num-mines*
                    :num-sweepers *num-sweepers*
                    :hwnd-main hwnd
                    :generation-counter 0
                    :window-width *width*
                    :window-height *height*
                    :sweepers (make-array *num-sweepers*)
                    :mines (make-array *num-mines*)
                    :average-fitness-vector (make-array 0 :fill-pointer t)
                    :best-fitness-vector (make-array 0 :fill-pointer t))))
        ;;create the mine sweepers
        (let ((sweepers (controller-sweepers controller)))
            (dotimes (i *num-sweepers*)
                (setf (aref sweepers i) (create-minesweeper)))
              
            ;; get/set the total number of weights used in the sweepers
            (setf (controller-num-weights-in-NN controller) 
                (neural-net-number-of-weights (minesweeper-brain (aref sweepers 0))))

            ;; initialize the Genetic Algorithm class
            (setf (controller-genetic-algorithm controller)
                (create-genetic-algorithm *num-sweepers* *mutation-rate* *crossover-rate*
                    (controller-num-weights-in-NN controller))) 
            
            ;; Get the weights from the genetic-algorithm and insert into the sweepers brains
            (setf (controller-population controller) (genetic-algorithm-population (controller-genetic-algorithm controller)))
            (dotimes (i *num-sweepers*)
                (let* ((vec-weights (genome-weights (aref (controller-population controller) i)))
                       (sweeper (aref sweepers i))
                       (neural-net (minesweeper-brain sweeper)))
                    (neural-net-set-weights neural-net vec-weights)))
            
            ;; initialize mines in random positions within the application window
            (dotimes (i *num-mines*)
                (setf (aref (controller-mines controller) i) 
                    (make-vector2d :x (* (rand-float) *width*) :y (* (rand-float) *height*))))
            
            ;; create and save pens for the graph drawing
            (setf (controller-blue-pen controller) (CreatePen PS_SOLID 1 (RGB 0 0 255)))
            (setf (controller-red-pen controller) (CreatePen PS_SOLID 1 (RGB 255 0 0)))
            (setf (controller-green-pen controller) (CreatePen PS_SOLID 1 (RGB 0 150 0)))
            (setf (controller-old-pen controller) nil)
            
            ;; fill the vertex buffers
            (setf (controller-sweeper-shape controller)(apply 'vector (sweeper-vertices)))
            (setf (controller-mine-shape controller)(apply 'vector (mine-vertices))))
        controller))
 
(defun controller-fast-render-toggle (controller) 
    (setf (controller-fast-render-mode controller) 
        (if (controller-fast-render-mode controller) nil t)))

;;; 
;;; Given a surface to draw on this function displays stats and a crude
;;; graph showing best and average fitness
;;;          
(defun controller-plot-stats (controller surface) 
    (let* ((best-str (format nil "Best Fitness:       ~A" (genetic-algorithm-best-fitness (controller-genetic-algorithm controller))))
           (ave-str (format nil "Average Fitness:       ~A" (genetic-algorithm-average-fitness (controller-genetic-algorithm controller))))
           (hslice (/ *width* (1+ (controller-generation-counter controller))))
           (vslice (/ *height* (* (1+ (genetic-algorithm-best-fitness (controller-genetic-algorithm controller))) 2)))
           (x 0)
           (old-pen (SelectObject surface (controller-red-pen controller))))
        ;; show captions
        (TextOut surface 5 20 (create-c-string best-str) (length best-str))
        (TextOut surface 5 40 (create-c-string ave-str) (length ave-str))
        
        ;; plot the best fitness
        (MoveToEx surface 0 *height* NULL)
        (dotimes (i (length (controller-best-fitness-vector controller)))
            (LineTo surface (floor x) (floor (- *height* (* vslice (aref (controller-best-fitness-vector controller) i)))))
            (incf x hslice))
       
        ;; plot the average fitness
        (setf x 0)
        (SelectObject surface (controller-blue-pen controller))
        (MoveToEx surface 0 *height* NULL)
        (dotimes (i (length (controller-average-fitness-vector controller)))
            (LineTo surface (floor x) (floor (- *height* (* vslice (aref (controller-average-fitness-vector controller) i)))))
            (incf x hslice))
        
        ;; replace the old pen
        (SelectObject surface old-pen)))

(defun controller-render (controller surface)
    ;; render the stats
    (let ((s (format nil "Generation:          ~D" (controller-generation-counter controller))))
        (TextOut surface 5 0 (create-c-string s) (length s)))
    
    (if (not (controller-fast-render-mode controller)) ;; do not render if running at accelerated speed
        (let ((old-pen (SelectObject surface (controller-green-pen controller)))) ;; keep a record of the old pen
            ;; render the mines
            (dotimes (i (controller-num-mines controller))
                (let ((mine-vertices                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                            (world-transform (apply 'vector (mine-vertices)) 
                                (aref (controller-mines controller) i)
                                *mine-scale*
                                0)))
                    ;; draw the mines
                    (MoveToEx surface (floor (point-x (aref mine-vertices 0))) (floor (point-y (aref mine-vertices 0))) NULL)
                    (dotimes (i (- (length mine-vertices) 1))
                        (LineTo surface (floor (point-x (aref mine-vertices (+ i 1))))
                                        (floor (point-y (aref mine-vertices (+ i 1))))))
                    (LineTo surface (floor (point-x (aref mine-vertices 0))) (floor (point-y (aref mine-vertices 0)))))) 
            
            ;; we want the fittest displayed in red
            (SelectObject surface (controller-red-pen controller))
                
            ;; render the sweepers
            (dotimes (i (controller-num-sweepers controller)) 
                (if (= i *num-elite*)
                    (SelectObject surface old-pen))
                (let* ((sweeper (aref (controller-sweepers controller) i))
                       (sweeper-vertices         ; grab the sweeper vertices
                            ;; transform the vertex buffer
                            (world-transform (apply 'vector (sweeper-vertices)) 
                                (minesweeper-position (aref (controller-sweepers controller) i))
                                *sweeper-scale*
                                (minesweeper-rotation sweeper))))
                    ;; draw the sweeper left track
                    (MoveToEx surface (floor (point-x (aref sweeper-vertices 0))) (floor (point-y (aref sweeper-vertices 0))) NULL)        
                    (dotimes (i 3)
                        (LineTo surface (floor (point-x (aref sweeper-vertices (+ i 1))))
                                        (floor (point-y (aref sweeper-vertices (+ i 1))))))
                    (LineTo surface (floor (point-x (aref sweeper-vertices 0))) (floor (point-y (aref sweeper-vertices 0))))
                    
                    ;; draw the sweeper right track
                    (MoveToEx surface (floor (point-x (aref sweeper-vertices 4))) (floor (point-y (aref sweeper-vertices 4))) NULL)        
                    (dotimes (i 3)
                        (LineTo surface (floor (point-x (aref sweeper-vertices (+ i 5))))
                                        (floor (point-y (aref sweeper-vertices (+ i 5))))))
                    (LineTo surface (floor (point-x (aref sweeper-vertices 4))) (floor (point-y (aref sweeper-vertices 4))))
                    
                    (MoveToEx surface (floor (point-x (aref sweeper-vertices 8))) (floor (point-y (aref sweeper-vertices 8))) NULL)
                    (LineTo surface (floor (point-x (aref sweeper-vertices 9))) (floor (point-y (aref sweeper-vertices 9))))       
                                  
                    (MoveToEx surface (floor (point-x (aref sweeper-vertices 10))) (floor (point-y (aref sweeper-vertices 10))) NULL)
                    (dotimes (i 5)
                        (LineTo surface (floor (point-x (aref sweeper-vertices (+ i 11))))
                                        (floor (point-y (aref sweeper-vertices (+ i 11))))))
                    
                    (SelectObject surface old-pen))))      ;; put the old pen back
        
        (controller-plot-stats controller surface)))

;;;
;;; Run the sweepers through *num-ticks* cycles. During this loop each sweepers NN is constantly 
;;; updated with the appropriate information from its surroundings. The output from the NN is obtained
;;; and the sweeper is moved. If it encounters a mine its fitness is updated appropriately.
;;;
(defun controller-update (controller) 
    (if (< (controller-ticks controller) *num-ticks*)
        (progn
            (incf (controller-ticks controller))
            (dotimes (i (controller-num-sweepers controller))
                (let ((sweeper (aref (controller-sweepers controller) i))
                      (genome (aref (controller-population controller) i)))
                    
                    ;; update the NN and position
                    (minesweeper-update sweeper (controller-mines controller))
                
                    ;; see if it found a mine
                    (let ((hit (minesweeper-check-for-mine sweeper (controller-mines controller) *mine-scale*)))
                        (when hit
                            ;; we have discovered a mine so increase fitness
                            (incf (minesweeper-fitness sweeper) 1)
                            
                            ;; mine found so replace the mine with another at a random position
                            (setf (aref (controller-mines controller) hit) 
                                (make-vector2d :x (* (rand-float) *width*) 
                                            :y (* (rand-float) *height*))))
                            
                        ;; update the chromosome fitness score
                        (setf (genome-fitness genome) (minesweeper-fitness sweeper))))))
        
        ;; else, another generation has been completed.
        ;; Time to run the GA and update the sweepers with their new NNs.
        (let ((ga (controller-genetic-algorithm controller)))
                    
            ;; update the stats to be used in our stat window
            (vector-push-extend (genetic-algorithm-average-fitness ga) 
                (controller-average-fitness-vector controller))
            (vector-push-extend (genetic-algorithm-best-fitness ga) 
                (controller-best-fitness-vector controller))
            
            ;; increment the generation counter
            (incf (controller-generation-counter controller))
            (debug-msg "Generation ~A" (controller-generation-counter controller))
            
            ;; reset cycles
            (setf (controller-ticks controller) 0)
            
            ;; run the GA to create a new population
            (setf (controller-population controller) (genetic-algorithm-epoch ga (controller-population controller)))

            ;; insert the new (hopefully)improved brains back into the sweepers and reset their positions etc
            (dotimes (i (controller-num-sweepers controller))
                (let ((sweeper (aref (controller-sweepers controller) i)))
                    (if (= 0 (length (genome-weights (aref (controller-population controller) i))))
                        (debug-msg "trying to put an empty weight vector into the brain")
                        (minesweeper-put-weights sweeper (genome-weights (aref (controller-population controller) i))))
                    (reset-minesweeper sweeper)))))
    t)
			
(ct:defun-callback WndProc ((hwnd HWND)(iMsg UINT)(wParam WPARAM)(lParam LPARAM))

  	(incf *messages-processed*)
    ;(debug-msg "xxxxxxx: hwnd=~A, imsg=~A, wParam=~A, lParam=~A" hwnd iMsg wParam lParam)
    (when (= iMsg WM_CREATE)

        (debug-msg "WM_CREATE: hwnd=~A, imsg=~A, wParam=~A, lParam=~A" hwnd iMsg wParam lParam)
    ;; get the size of the client window
        (GetClientRect hwnd *rect*)
        (setf *width* (cref RECT *rect* right))
        (setf *height* (cref RECT *rect* bottom))
        (setf *controller* (create-controller hwnd))
        (setf *hdc-backbuffer* (CreateCompatibleDC NULL)) ; create a surface for us to render to (backbuffer)
        (setf *hdc* (GetDC hwnd))
        (setf *bitmap* (CreateCompatibleBitmap *hdc* *width* *height*))
        (ReleaseDC hwnd *hdc*)            
        (setf *old-bitmap* (SelectObject *hdc-backbuffer* *bitmap*)))
    
    (when (= iMsg WM_KEYUP)
        (debug-msg "WM_KEYUP hwnd=~A, imsg=~A, wParam=~A, lParam=~A" hwnd iMsg wParam lParam)
        (if (= wParam VK_ESCAPE)
            (PostQuitMessage 0)
            (if (= wParam (char-int #\F))
                (controller-fast-render-toggle *controller*)
                (if (= wParam (char-int #\R))
                    (setf *controller* (create-controller hwnd)))))) ;; reset--create a new controller

    (when (= iMsg WM_SIZE)
        (debug-msg "WM_SIZE hwnd=~A, imsg=~A, wParam=~A, lParam=~A" hwnd iMsg wParam lParam)
		(setf *width*  (max (LOWORD lParam) 1))
		(setf *height* (max (HIWORD lParam) 1))
        (SelectObject *hdc-backbuffer* *old-bitmap*) ;; resize the backbuffer accordingly
        (setf *hdc* (GetDC hwnd))
        (setf *bitmap* (CreateCompatibleBitmap *hdc* *width* *height*))
        (ReleaseDC hwnd *hdc*)
        (setf *old-bitmap* (SelectObject *hdc-backbuffer* *bitmap*)))
    
	(when (= iMsg WM_PAINT)
	;;	(debug-msg "WM_PAINT hwnd=~A, imsg=~A, wParam=~A, lParam=~A" hwnd iMsg wParam lParam)
        (BeginPaint hwnd *ps*)
        (BitBlt *hdc-backbuffer* 0 0 *width* *height* NULL 0 0 WHITENESS) ;; fill	our backbuffer with white
        (controller-render *controller* *hdc-backbuffer*)
        (BitBlt (cref PAINTSTRUCT *ps* hdc) 0 0 *width* *height* *hdc-backbuffer* 0 0 SRCCOPY)
		(EndPaint hwnd *ps*))
    
	(when (= iMsg WM_TIMER)
	;;	(debug-msg "WM_TIMER hwnd=~A, imsg=~A, wParam=~A, lParam=~A" hwnd iMsg wParam lParam)
        (unless (controller-update *controller*) 
            (PostQuitMessage 0)) ;; we have a problem, end app
		(win:InvalidateRect hwnd NULL 1)
        (UpdateWindow hwnd))
    
    (when (= iMsg WM_DESTROY)
		(debug-msg "WM_DESTROY hwnd=~A, imsg=~A, wParam=~A, lParam=~A" hwnd iMsg wParam lParam)
        (uninstall-refresh-timer)
        (SelectObject *hdc-backbuffer* *old-bitmap*)
        
        ;; clean up our backbuffer objects
		(DeleteDC *hdc-backbuffer*)
        (DeleteObject *bitmap*)

		(PostQuitMessage 0))
    (DefWindowProc hwnd iMsg wParam lParam))

(defun WinMain (hInstance hPrevInstance szCmdLine iCmdShow)
	(declare (ignore hPrevInstance szCmdLine))
	(let ((*app-window* *app-window*)	;; rebind for per-thread copy
		  (*width* *width*)
		  (*height* *height*)
		  (msg (ct:malloc (sizeof 'MSG)))
		  (wndclass (ct:malloc (sizeof 'WNDCLASSEX)))
		  (*ps* (ct:malloc (sizeof 'PAINTSTRUCT)))
		  (*rect* (ct:malloc (sizeof 'RECT))))

		(setf (cref WNDCLASSEX wndclass cbSize) (sizeof 'WNDCLASSEX))
		(setf (cref WNDCLASSEX wndclass style) (logior CS_HREDRAW CS_VREDRAW))
		(setf (cref WNDCLASSEX wndclass lpfnWndProc) (get-callback-procinst 'WndProc))
		(setf (cref WNDCLASSEX wndclass cbClsExtra) 0)
		(setf (cref WNDCLASSEX wndclass cbWndExtra) 0)
		(setf (cref WNDCLASSEX wndclass hInstance) hInstance)
		(setf (cref WNDCLASSEX wndclass hIcon) (LoadIcon NULL IDI_APPLICATION))
		(setf (cref WNDCLASSEX wndclass hCursor) (LoadCursor NULL IDC_ARROW))
		(setf (cref WNDCLASSEX wndclass hbrBackground) NULL)
		(setf (cref WNDCLASSEX wndclass lpszMenuName) NULL)
		(setf (cref WNDCLASSEX wndclass lpszClassName) (ct:create-c-string szAppName))
		(setf (cref WNDCLASSEX wndclass hIconSm) (LoadIcon NULL IDI_APPLICATION))
		
		(RegisterClassEx wndclass)
		(setq *app-window* 
			(CreateWindowEx 0
				(ct:create-c-string szAppName)				;; window class name
				(ct:create-c-string "Minesweepers") 		;; window caption
				(logior WS_OVERLAPPEDWINDOW	WS_VISIBLE 
                    WS_CAPTION WS_SYSMENU)					;; window style
				CW_USEDEFAULT								;; initial x position
				CW_USEDEFAULT								;; initial y position
				*width*             						;; initial x size
				*height*                					;; initial y size
				(cl::get-application-main-window)			;; parent window handle
				NULL										;; window menu handle
				hInstance									;; program instance handle
				NULL))					                    ;; creation parameters

		(install-refresh-timer)
		(ShowWindow *app-window* iCmdShow)
		(UpdateWindow *app-window*)
		(do ((ret (GetMessage msg NULL 0 0)(GetMessage msg NULL 0 0)))
			((not ret))
			(TranslateMessage msg)
			(if (= (cref MSG msg message) WM_QUIT)
				(return))
			(DispatchMessage msg))

		(cref MSG msg wParam)))

(defun minesweepers ()
	(restart-case
		(handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'error))))
			(winmain (cl::get-application-instance) 
				null (ct:create-c-string "") SW_SHOW))
		(error () (return-from minesweepers))))


