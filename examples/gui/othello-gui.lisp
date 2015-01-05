;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	Windows OTHELLO program.
;;;;    Based on Othello, Paradigms of AI, P.Norvig
;;;;    
;;;;    To run from lisp, load this file and enter:
;;;;
;;;;		(th:create-thread #'win::othello-gui)
;;;;
;;;;    To create a single-file executable, load this file and enter:
;;;;    (save-application "othello-gui" #'win::othello-gui :static t)
;;;;

(in-package :win)

(require "GUI")

(defvar *horiz-cells-default* 8)
(defvar *vert-cells-default* 8)

(defvar *gui-move* nil)

(defconstant all-directions '(-11 -10 -9 -1 1 9 10 11))

(defconstant empty 0 "Empty square")
(defconstant black 1 "Black square")
(defconstant white 2 "White square")
(defconstant out-of-bounds 3 "Out of bounds square")

(defconstant *initial-white-moves* '(44 55))
(defconstant *initial-black-moves* '(45 54))

(defparameter *previous-player* white)  ;; initialize to white so black is first to move.
(defconstant default-move-time-millis 1800)
(defparameter *move-time-millis* default-move-time-millis)
(defparameter *move-timer-id* 1)

(defparameter *black-moves* '())
(defparameter *white-moves* '())
(defparameter *random-moves* 5)
(defparameter *move-number* 0)

(defun initialize-moves ()
    (setf *black-moves* '() *white-moves* '())   
    (push (first  *initial-black-moves*) *black-moves*)
    (push (second *initial-black-moves*) *black-moves*)   
    (push (first  *initial-white-moves*) *white-moves*)
    (push (second *initial-white-moves*) *white-moves*))

(defun move-history (player)
    (let* ((moves (if (eql player white) *white-moves* *black-moves*))
           (but-last-move (reverse (cdr (mapcar #'get-square-from-move moves))))
           (last-move (get-square-from-move (first moves))))
        (concatenate 'string 
            (format nil "~{~a : ~}" but-last-move) 
            (format nil "~a" last-move))))

(defun move-history-game ()
    (format nil "~& Black:  ~a~&White:  ~a~&" (move-history black) (move-history white)))

(deftype piece () `(integer ,empty ,out-of-bounds))

(defun name-of (piece) (char ".BW?" piece))
(defun long-name-of (piece) (elt (list "Empty" "Black" "White" "Out of Bounds") piece))

(defun opponent (player) (if (eql player black) white black))

(deftype cells () `(simple-array piece (100)))

(defun cells-ref (cells square) (aref cells square))
(defsetf cells-ref (cells square) (val)
    `(setf (aref ,cells ,square) ,val))

(defmethod copy-cells (cells)
    (copy-seq cells))

(defconstant all-squares
    (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

(defmacro set-initial-squares ()
    `(setf (cells-ref cells ,(first  *initial-white-moves*)) white 
           (cells-ref cells ,(second *initial-white-moves*)) white
           (cells-ref cells ,(first  *initial-black-moves*)) black
           (cells-ref cells ,(second *initial-black-moves*)) black))

(defmethod initialize-cells ()
    "Return an array of with the starting configuration of 4 center squares taken."
    ;; Boards are 100 element arrays with border cells out of bounds so that the
    ;; central 64 squares represent the area of play.
    ;;
    (let ((cells (make-array 100 :element-type 'piece :initial-element out-of-bounds)))
        (dolist (square all-squares)
            (setf (cells-ref cells square) empty))  ; initialize playable squares to empty
        
        ;; Now initialize the four central squares to their proper color
        (set-initial-squares)
        
        cells))

(defmethod reset-game (board)
    (let ((cells (cells board)))
        (dolist (square all-squares)
            (setf (cells-ref cells square) empty))
        (set-initial-squares)
        (initialize-moves)
        (print-board (cells board))
        (setf *previous-player* white)
        ;;(setf *white-strategy* (get-beginner-strategy))
        (setf *move-number* 0)))

(defconstant col-names '(A B C D E F G H))

(defun count-difference (player cells)
    "Count player's pieces minus oppenent's pieces."
    (- (count player cells)
       (count (opponent player) cells)))

(defun display-move-history (window)
    (MessageBox (window-hwnd window)
        (ct:create-c-string (move-history-game))
        (ct:create-c-string "Othello - Move History")
        MB_OK))

(defun display-score-message (window cells)
    (MessageBox (window-hwnd window)
        (ct:create-c-string (get-score-string cells))
        (ct:create-c-string "Othello - Score")
        MB_OK))

(defun get-score-string (cells)
    (format nil "~2&[~a=~2a ~a=~2a (~@d)]"
        (long-name-of black) (count black cells)
        (long-name-of white) (count white cells)
        (count-difference black cells)))

(defun print-board (cells)
    "Print a board.  Show some stats."
    (format t "~2&    1 2 3 4 5 6 7 8    [~c=~2a ~c=~2a (~@d)]"
        (name-of black) (count black cells)
        (name-of white) (count white cells)
        (count-difference black cells))
    (loop for row from 1 to 8 do 
        (format t "~&  ~d " (elt col-names (- row 1)))
        (loop for col from 1 to 8
              for piece = (cells-ref cells (+ col (* 10 row)))
            do (format t "~c " (name-of piece))))
    (format t "~2&")
    (force-output))

(defun find-bracket-square (square player cells dir)
    (cond ((eql (cells-ref cells square) player) square)
          ((eql (cells-ref cells square) (opponent player))
            (find-bracket-square (+ square dir) player cells dir))
          (t nil)))

(defun would-flip? (move player cells dir)
    "If move results in flips, return bracket-square otherwise nil."
    ;; Note the bracket square is the first player square after a string of opponent
    ;; squares moving by dir.
    ;;
    (let ((c (+ move dir)))
        (and (eql (cells-ref cells c) (opponent player))
             (find-bracket-square (+ c dir) player cells dir))))

(defun make-flips (move player cells dir)
    "Make necessary flips in the given direction"
    (let ((bracket-square (would-flip? move player cells dir)))
        (when bracket-square
            (loop for square from (+ move dir) by dir until (eql square bracket-square)
                do (setf (cells-ref cells square) player)))))

        
(defun valid-move-p (move)
    "Must be an integer, in the range 11-88 and end in 1-8"
    (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defun legal-move-p (move player cells)
    "Must be a valid move into an empty square, and must flip an opponent piece."
    (and (valid-move-p move)
         (eql (cells-ref cells move) empty)
         (some #'(lambda (dir) (would-flip? move player cells dir))
            all-directions)))

(defun track-move (move player)
    (if (eql player black)
        (setf *black-moves* (push move *black-moves*))
        (setf *white-moves* (push move *white-moves*))))

(defun make-move (move player cells)
    "Update board to reflect move by player"
    (setf (cells-ref cells move) player)
    (dolist (dir all-directions)
        (make-flips move player cells dir))
    cells)

(defun any-legal-move? (player cells)
    ;; Is there at least one legal move for player in all-squares?
    (some #'(lambda (move) (legal-move-p move player cells))
        all-squares))

(defun next-to-play (board previous-player print)
    "Find the next player to move.  Nil if no one can move."
    (let ((opp (opponent previous-player)))
        (cond ((any-legal-move? opp (cells board)) opp)
              ((any-legal-move? previous-player (cells board)) 
                (when print
                    (format t "~&~c has no moves and must pass." (name-of opp))
                    (force-output))
                previous-player)
              (t nil))))

(defconstant square-move-map '((a1 . 11) (a2 . 12) (a3 . 13) (a4 . 14) (a5 . 15) (a6 . 16) (a7 . 17) (a8 . 18)
                               (b1 . 21) (b2 . 22) (b3 . 23) (b4 . 24) (b5 . 25) (b6 . 26) (b7 . 27) (b8 . 28)
                               (c1 . 31) (c2 . 32) (c3 . 33) (c4 . 34) (c5 . 35) (c6 . 36) (c7 . 37) (c8 . 38)
                               (d1 . 41) (d2 . 42) (d3 . 43) (d4 . 44) (d5 . 45) (d6 . 46) (d7 . 47) (d8 . 48)
                               (e1 . 51) (e2 . 52) (e3 . 53) (e4 . 54) (e5 . 55) (e6 . 56) (e7 . 57) (e8 . 58)
                               (f1 . 61) (f2 . 62) (f3 . 63) (f4 . 64) (f5 . 65) (f6 . 66) (f7 . 67) (f8 . 68)
                               (g1 . 71) (g2 . 72) (g3 . 73) (g4 . 74) (g5 . 75) (g6 . 76) (g7 . 77) (g8 . 78)
                               (h1 . 81) (h2 . 82) (h3 . 83) (h4 . 84) (h5 . 85) (h6 . 86) (h7 . 87) (h8 . 88)))

(defparameter *initial-weights*
    '#(0   0   0  0  0  0  0   0   0 0
       0 120 -20 20  5  5 20 -20 120 0
       0 -20 -40 -5 -5 -5 -5 -40 -20 0
       0  20  -5 15  3  3 15  -5  20 0
       0   5  -5  3  3  3  3  -5   5 0
       0   5  -5  3  3  3  3  -5   5 0
       0  20  -5 15  3  3 15  -5  20 0
       0 -20 -40 -5 -5 -5 -5 -40 -20 0
       0 120 -20 20  5  5 20 -20 120 0
       0   0   0  0  0  0  0   0   0 0))

(defparameter *weights*
    #(0 0 0 0 0 0 0 0 0 0 0 14933/125 -2516/125 167/8 3069/500 618/125 2473/125 -2518/125 59907/500 0 0 -10053/500 -4933/125 -1501/250 -4623/1000 -2873/500 -1731/500 -40271/1000 -18763/1000 0 0 10839/500 -1087/200 15167/1000 2459/1000 819/250 1962/125 -1189/200 19843/1000 0 0 887/200 -2577/500 919/250 3901/1000 2807/1000 4067/1000 -1299/250 2349/500 0 0 239/50 -686/125 1131/500 2473/1000 1393/1000 1899/1000 -6441/1000 5163/1000 0 0 9839/500 -4973/1000 15271/1000 1171/500 1467/500 3813/250 -2137/500 10101/500 0 0 -9787/500 -38561/1000 -381/125 -477/100 -5481/1000 -5329/1000 -41397/1000 -9979/500 0 0 59911/500 -20397/1000 18397/1000 824/125 158/25 21371/1000 -209/10 24081/200 0 0 0 0 0 0 0 0 0 0 0))

(defun get-move-from-square (square)
    (cdr (assoc square square-move-map)))

(defun get-square-from-move (move)
    (car (find move square-move-map :key #'cdr)))

(defun get-move (strategy player cells print)
    "Call player's strategy function to get a move for this board."
    (when print (print-board cells))
    (let ((move (funcall strategy player (copy-cells cells))))
        (cond ((legal-move-p move player cells)
               (when print
                    (format t "~&~c moves to ~d" (name-of player) (get-square-from-move move))
                    (force-output))
               (make-move move player cells)
               (track-move move player))
            (t (warn "Illegal move.")
               (get-move strategy player cells print)))))

(defun get-gui-move ()
    "Polls the *gui-move* var looking for a value.
     Resets it to nil after collecting."
    (do ()
        ((not (null *gui-move*)))
        (sleep 1/100)) ; sleep 10ms
    (let ((move *gui-move*))
        (setf *gui-move* nil) ; reset *gui-move*
        move))

(defun gui-human-player (player cells)
    "Human player strategy...get moves from gui."
    (declare (ignore player cells))
    (get-gui-move))

        
    
(defun human-player (player cells)
    "Human player strategy"
    (declare (ignore cells))
    (format t "~&~c's turn to move:~%" (name-of player))
    (get-move-from-square (read)))

(defun legal-moves (player cells)
    "return a list of legal moves for player on this board"
    (loop for move in all-squares
        when (legal-move-p move player cells) collect move))

(defun random-elt (list)
    (elt list (random (length list))))

(defun random-strategy (player cells)
    "Make a random legal move"
    (random-elt (legal-moves player cells)))

(defun maximizer (eval-fn)
    #'(lambda (player cells)
        (let* ((moves (legal-moves player cells))
               (scores (mapcar #'(lambda (move) (funcall eval-fn player (make-move move player (copy-cells cells)))) moves))
               (best (apply #'max scores)))
            (elt moves (position best scores)))))

(defun maximize-difference (player cells)
    ;;(format t "In Maximize difference~%")
    ;;(force-output)
    (funcall (maximizer #'count-difference) player cells))

(defun weighted-squares (player cells)
    (let ((opp (opponent player)))
        (loop for i in all-squares
            when (eql (cells-ref cells i) player) sum (aref *weights* i)
            when (eql (cells-ref cells i) opp) sum (- (aref *weights* i)))))

(let ((neighbor-table (make-array 100 :initial-element nil)))
    (loop for square in all-squares do
        (loop for dir in all-directions do
            (if (valid-move-p (+ square dir))
                (push (+ square dir) (aref neighbor-table square)))))
    
    (defun neighbors (cell)
        (aref neighbor-table cell)))

(defun smart-weighted-squares (player cells)
    ;; uses the weighted squares functions and makes tweaks in weight vals
    ;; for populated corners.
    (let ((w (weighted-squares player cells)))
        (loop for corner in '(11 18 81 88) do
            (when (not (eql (cells-ref cells corner) empty))
                (loop for neighbor in (neighbors corner) do
                    (when (not (eql (cells-ref cells neighbor) empty))
                        (incf w (* (- 5 (aref *weights* neighbor))
                                   (if (eql (cells-ref cells neighbor) player) 1 -1)))))))
        w))

(defun maximize-weight (player cells)
    (funcall (maximizer #'weighted-squares) player cells))

(defconstant winning-value most-positive-fixnum)
(defconstant losing-value  most-negative-fixnum)

(defun final-value (player cells)
    (case (signum (count-difference player cells))
        (-1 losing-value)
        (0 0)
        (1 winning-value)))

;;; alpha-beta search
(defun minimax (player cells alpha beta ply eval-fn)
    (if (= ply 0)
        (funcall eval-fn player cells)
        (let ((moves (legal-moves player cells)))
            (if (null moves)
                (if (any-legal-move? (opponent player) cells)
                    (- (minimax (opponent player) cells (- beta) (- alpha) (- ply 1) eval-fn))
                    (final-value player cells))
                (let ((best-move (first moves)))
                    (loop for move in moves do
                        (let* ((cells2 (make-move move player (copy-cells cells)))
                               (val (- (minimax (opponent player) cells2 (- beta) (- alpha) (- ply 1) eval-fn))))
                            ;;(print-board cells2)
                            ;;(format t "~a (~a):  ~a ~a~&" (long-name-of player) (- 3 ply) val (get-square-from-move move))
                            (when (> val alpha)
                                (setf alpha val
                                      best-move move)))
                        until (>= alpha beta))
                    ;;(format t "~&Best is:  ~a (~a):  ~a ~a~2&" (long-name-of player) (- 3 ply) alpha (get-square-from-move best-move))
                    (force-output)
                    (values alpha best-move))))))

(defun minimax-search (ply eval-fn)
    #'(lambda (player cells)
        (incf *move-number*)
        (if (< *move-number* *random-moves*)
            (progn
                (format t "**********RANDOM-STRATEGY***********")
                (force-output)
                (random-strategy player cells))
            (multiple-value-bind (val move) (minimax player cells losing-value winning-value ply eval-fn)
                (format t "**********Searching ~d plies deep.***********" ply)
                (format t "~&Alpha is:  ~f~2&" val)  ;;(declare (ignore val))
                (force-output)
                move))))

(defconstant beginner-strategies 
    (list #'random-strategy 
        #'maximize-difference
        #'maximize-weight))
(defun get-beginner-strategy ()
    (random-elt beginner-strategies))

(defconstant intermediate-strategies 
    (list (minimax-search 5 #'count-difference)
        (minimax-search 4 #'smart-weighted-squares))) 
(defun get-intermediate-strategy ()
    (random-elt intermediate-strategies))

(defconstant advanced-strategies 
    (list (minimax-search 6 #'smart-weighted-squares))) ;;(minimax-search 6 #'count-difference)
(defun get-advanced-strategy ()
    (random-elt advanced-strategies))

(defparameter *white-strategy* (get-beginner-strategy))  ;; set white to use a beginner strategy by default

(defclass board ()
	((cells :accessor cells 
			:initform (initialize-cells))
	
	 (view-width :initform 1 :accessor view-width)
	 (view-height :initform 1 :accessor view-height)))

(defun create-board () (make-instance 'board))

(defmethod horiz-cells (board) (declare (ignore board)) *horiz-cells-default*)
(defmethod vert-cells  (board) (declare (ignore board)) *vert-cells-default*)

(defun start-move-timer (hwnd)
	(win:SetTimer hwnd *move-timer-id* *move-time-millis* NULL))
 
(defun stop-move-timer (hwnd)
	(win:KillTimer hwnd *move-timer-id*))

(defclass <othello-window> (<main-menu-mixin> <frame>)
	((board
		:accessor board
		:initform (create-board)
		:initarg :board)
	 (rect :accessor othello-rect :initform (ct:malloc (sizeof 'RECT)))))

(defun othello (black-strategy white-strategy &optional (print t))
    "Play a game of othello."
    (setf *move-number* 0)
    (let* ((board (create-board))
           (cells (cells board)))
        (loop for player = black
                  then (next-to-play board player print)
              for strategy = (if (eql player black)
                                  black-strategy
                                  white-strategy)
            until (null player)
            do (get-move strategy player cells print))
        (when print
            (format t "~2&*** Game Over. ***~2&Final Result:")
            (print-board cells))
        (count-difference black cells)))

(defun othello-gui ()
    (gui-initialize)
	(let ((window (make-instance '<othello-window>)))
        (reset-game (board window))
		(create-menu window '(:menu "File") nil 1)
		(create-menu window 
			(list :command "New Game"  
				(lambda () 
					(reset-game(board window))
  					(refresh-board window)))         
			"File" 1)
		(create-menu window 
			(list :command "Quit"  
				(lambda ()
                    (format t "~&Quit game.~&")
                    (win:CloseWindow (window-hwnd window))
                    (win:DestroyWindow (window-hwnd window)))) 	  
			"File" 2)      
        (create-menu window '(:menu "Difficulty Level") nil 2)
        (create-menu window
            (list :command "Beginner"
                (lambda ()
                    (setf *white-strategy* (get-beginner-strategy))
                    (SetWindowText (window-hwnd window) "Othello - Beginner")))
            "Difficulty Level" 3)
        (create-menu window
            (list :command "Intermediate"
                (lambda ()
                    (setf *white-strategy* (get-intermediate-strategy))
                    (SetWindowText (window-hwnd window) "Othello - Intermediate")))
            "Difficulty Level" 2)
        (create-menu window
            (list :command "Advanced"
                (lambda ()
                    (setf *white-strategy* (get-advanced-strategy))
                    (SetWindowText (window-hwnd window) "Othello - Advanced")))
            "Difficulty Level" 1)
        (create-menu window '(:menu "View") nil 3)
        (create-menu window
            (list :command "Move History..."
                (lambda ()
                    (display-move-history window)))
             "View" 1)
        (create-menu window
            (list :command "Current Score..."
                (lambda ()
                    (display-score-message window (cells (board window)))))
            "View" 2)
		(create-window window
			:caption "Othello - Beginner"
			:style (logior WS_OVERLAPPEDWINDOW WS_MINIMIZEBOX)
			:width 500
			:height 500)			
		(show-window window SW_SHOW)
		(update-window window)
		(standard-message-loop)))

(defmethod handle-message ((window <othello-window>) (message <create-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method)
	0)

(defmethod handle-message ((window <othello-window>) (message <destroy-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method)
	0)

(defmethod handle-message ((window <othello-window>) (message <paint-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (begin-paint window)
	(let ((board (board window)))
		(draw-board board (window-hdc window) (othello-rect window)))
    (end-paint window)
	0)

(defmethod handle-message ((window <othello-window>)(message <timer-message>) wparam lparam)
	(declare (ignore message wparam lparam))
    (call-next-method)
    
    ;; Stop the timer so it doesn't keep sending events
    (stop-move-timer (window-hwnd window))
    
	;; Make computer move(s) and refresh board
    ;; Return if next-player is black or null otherwise continue making moves
    ;; until one of those is true.
    ;;
    (let* ((board (board window))
           (player (next-to-play board *previous-player* t))
           (total-white-moves 0))
        (loop
            (if (> total-white-moves 0)
                (MessageBox (window-hwnd window) 
                    (ct:create-c-string "Black has no moves, and must pass.") 
                    (ct:create-c-string "Othello") 
                    MB_OK))
            (format t "about to make white move.")
            (get-move *white-strategy* player (cells board) t)
            (format t "just made white move.")
            (incf total-white-moves)
            (refresh-board window)                 
            (setf *previous-player* player
                  player (next-to-play board player t))
            (cond
                ((null player) 
                    (print-board (cells board))
                    (let ((msg (format nil "~a" (get-game-over-message (cells board)))))
                        (force-output)
                        (MessageBox (window-hwnd window) 
                            (ct:create-c-string msg) 
                            (ct:create-c-string "Othello") 
                            MB_OK))
                    (return-from handle-message 0))
                ((eql player black) (return-from handle-message 0)))))
    (refresh-board window)
	0)

(defmethod handle-message ((window <othello-window>) (message <size-message>) wparam lparam)
	(declare (ignore window message wparam lparam))
    (call-next-method)
	(let ((board (board window)))
		(setf (view-width board) (max (LOWORD lParam) 1))
		(setf (view-height board) (max (HIWORD lParam) 1)))
	0)

(defmethod handle-message ((window <othello-window>) (message <lbuttondown-message>) wparam lparam)
	(declare (ignore message wparam))
    (call-next-method)
	(let* ((board (board window))
           (player (next-to-play board *previous-player* t))) 
        (if (eql player black)
            (progn
                (format t "~a~%" (name-of player))
    		    (let ((x (LOWORD lParam))
    			      (y (HIWORD lParam)))
    			    (if (move-to-cell-at-position x y player board)
                        (progn
                            (refresh-board window)
                            ;; Check to see if game is over
                            (setf *previous-player* player)
                            (setf player (next-to-play board player t))
                            (cond
                                ((null player)
                                    (print-board (cells board))
                                    (let ((msg (format nil "~a" (get-game-over-message (cells board)))))
                                        (force-output)
                                        (MessageBox (window-hwnd window) 
                                            (ct:create-c-string msg) 
                                            (ct:create-c-string "Othello") 
                                            MB_OK))
                                    (return-from handle-message 0))
                                ((eql player black) 
                                    (MessageBox (window-hwnd window)
                                        (ct:create-c-string "White has no moves and must pass.")
                                        (ct:create-c-string "Othello")
                                        MB_OK)
                                    (return-from handle-message 0)))
                            ;; Start timer on computer move
                            (start-move-timer (window-hwnd window)))                        
                        ;;return if move cannot be made
                        (progn
                            (format t "~&Could not make move.~%")
                            (return-from handle-message 0)))))
            (progn
                (warn "It's not your turn.")
                (return-from handle-message 0))))
	0)

(defun determine-winner (cells)
    (let ((score (count-difference black cells)))
        (cond 
            ((> score 0) (long-name-of black))
            ((< score 0) (long-name-of white))
            (t nil))))

(defun get-game-over-message (cells)
    (let ((winner (determine-winner cells)))
        (if winner
            (format nil "~2&*** Game Over ***~&*** ~a wins!! ***~2&~a" winner (get-score-string cells))
            (format nil "~2&*** Game Over ***~&*** Tie game. ***~2&~a" (get-score-string cells)))))

(defun draw-colored-rect (x1 y1 x2 y2 r g b hdc rect)
	(let ((brush (CreateSolidBrush (rgb r g b))))
		(with-c-struct (s rect RECT)
			(setf left   x1
				  top    y1
				  right  x2
				  bottom y2)
			(FillRect hdc rect brush)
			(DeleteObject brush))))

(defun draw-colored-ellipse (left top right bottom r g b hdc)
	(let ((brush (CreateSolidBrush (rgb r g b)))
		  (prev-object))
		(setf prev-object (SelectObject hdc brush))	
		(Ellipse hdc left top right bottom)
		(SelectObject hdc prev-object)
		(DeleteObject brush)))

;; draw black vertical grid lines
(defun draw-vertical-lines (num cell-width height hdc rect)
	(dotimes (i num)
		(draw-colored-rect (* cell-width i) 0 
			(+ (* cell-width i) 1) height
			0 0 0 hdc rect)))

;; draw black horizontal grid lines
(defun draw-horizontal-lines (num cell-height width hdc rect)
	(dotimes (i num)
		(draw-colored-rect 0 (* cell-height i) 
			width (+ (* cell-height i) 1)
			0 0 0 hdc rect)))

(defun draw-piece (piece x y cell-width cell-height hdc)
    (let ((rgb-list '()))
        (if (eql piece white)
            (setf rgb-list '(255 255 255))
            (setf rgb-list '(0 0 0)))     
        (draw-colored-ellipse (+ 2 (* x cell-width)) (+ 2 (* y cell-height))
			(1- (* (+ x 1) cell-width)) (1- (* (+ y 1) cell-height))
            (first rgb-list) (second rgb-list) (third rgb-list) hdc)))

(defun update-pieces (cells horiz-cells cell-width vert-cells cell-height hdc)
    (loop for y from 1 to vert-cells do
	   (loop for x from 1 to horiz-cells do
                (let ((piece (cells-ref cells (+ x (* 10 y)))))
				    (if (or (eql piece white) (eql piece black))
					   (draw-piece piece (- x 1) (- y 1) cell-width cell-height hdc))))))
 
(defun draw-board (board hdc rect)
	(let* ((horiz (horiz-cells board))
		   (vert (vert-cells board))
		   (cells (cells board))
		   (cell-width (truncate (view-width board) horiz))
		   (cell-height(truncate (view-height board) vert)))
		(draw-vertical-lines (+ horiz 1) cell-width (* cell-height vert) hdc rect)
		(draw-horizontal-lines (+ vert 1) cell-height (* cell-width horiz) hdc rect)
		(update-pieces cells horiz cell-width vert cell-height hdc)))

(defun refresh-board (window)
    (win:InvalidateRect (window-hwnd window) NULL t))

(defun make-gui-move (x y player cells)
    "Post the move made by user in gui for collection by the gui-human-player strategy"
    (let ((move (+ (* 10 (+ 1 y)) (+ 1 x))))
        (if (legal-move-p move player cells)
            (progn
                (format t "~&~c moves to ~d (~d)" (name-of black) (get-square-from-move move) move)
                (make-move move player cells)
                (track-move move player))
            (progn
                (warn "Illegal move.")
                nil))))
        
(defun move-to-cell-at-position (x y player board)
	(let* ((horiz (horiz-cells board))
           (vert (vert-cells board))
           (cell-width (truncate (view-width board) horiz))
		   (cell-height(truncate (view-height board) vert))
		   (cell-x (truncate x cell-width))
		   (cell-y (truncate y cell-height)))
		(if (>= cell-x horiz)
			(setf cell-x (1- horiz)))
		(if (>= cell-y vert)
			(setf cell-y (1- vert)))
		(make-gui-move cell-x cell-y player (cells board))))

(defun average (list)
    (/ (reduce #'+ list) (length list)))

(defun test-strategy (strategy-1 strategy-2)
    "Play 10 games and report the average score"
    (average (loop for i from 1 to 10 collecting (othello strategy-1 strategy-2 nil))))

(defun crossover (pair)
    (map 'vector #'random-elt (map 'list #'list (first pair) (second pair))))

(defun make-ob-elts-zero (weights)
    (loop for i from 0 to 10 do 
        (setf (aref weights i) 0))
    (loop for i from 11 to 88 do 
        (when (or (< (mod i 10) 1)  (> (mod i 10) 8))
            (setf (aref weights i) 0)))
    (loop for i from 89 to 99 do 
        (setf (aref weights i) 0))
    weights)

(defun mutate (x)
    (let* ((x-as-list (concatenate 'list x))
           (mutated-list (loop for item in x-as-list collect
                (funcall (random-elt (list #'+ #'-)) item (/ (random 1000) 1000)))))
        (concatenate 'vector mutated-list)))

(defun new-generation (parent n)
    (let ((children '()))
        (dotimes (i n)
            (push (list (mutate parent) (mutate parent)) children))
        children))

(defun show-progress (num-completed total &optional (steps 100))
    (when (= (mod num-completed (max (truncate total steps) 1)) 0)
        (format t "~d~a~c" (truncate (* (/ num-completed total) 100)) "%" (code-char 13))
        (force-output)))

(defmacro dotimes-with-progress ((var times) &body body)
    `(progn
        (dotimes (,var ,times)
            (show-progress ,var ,times)
            ,@body)
        (format t "100%...Done.~c" (code-char 13))))
    

(defun evolve-weights (initial-weights generations pop-size)
    (let*((best-score 0)
          (*weights* initial-weights)
          (best-weights *weights*))
        (dotimes-with-progress (i generations) 
            (dolist (weights-arr 
                    (mapcar #'make-ob-elts-zero 
                        (mapcar #'crossover (new-generation best-weights pop-size))))
                (setf *weights* weights-arr)
                (let ((score (test-strategy (maximizer #'smart-weighted-squares) (maximizer #'count-difference))))
                    ;;(format t "~2&~s~&~a" *weights* score)
                    (when (> score best-score)
                        (setf best-score score)
                        (setf best-weights *weights*)))))
        (values best-weights best-score)))

        