(defvar *board* (make-list 9 :initial-element '-))
(defconstant board-size 9)
(defvar *current-player* nil)
(defvar *move* nil)
(defvar *human* nil)
(defvar *computer* nil)



;;; Board functions
(defun display-board ()
  (dotimes
      (i 9)
	(when (= (mod i 3) 0) (terpri))
	(format t "~a" (get-value-of i)))
  (format t "~%~a" "***********************")
  (values))


(defun clear-board ()
  (setf *board*  (make-list 9 :initial-element '-))
  (format t "*** Board cleared ***")
  (values))


;;; Helper function - to be deleted
(defun fill-board ()
  (dotimes (i 9)
    (set-value-of i 'X)))


(defun has-board-free-cells ()
  (let ((free-cells ()))
    (dotimes (i 9)
      (if
       (is-cell-empty (get-value-of i))
       (push i free-cells)))
    (if
     (eql free-cells ())
     (return-from has-board-free-cells nil)
     (return-from has-board-free-cells (reverse free-cells)))))


(defun is-board-full ()
  (not (has-board-free-cells)))


;;; Cell accessor functions
(defun get-value-of (cell)
  (if
   (not (is-cell-within-limits cell))
   nil
   (nth cell *board*)))


(defun set-value-of (cell value)
  (if
   (and
    (numberp cell)
    (symbolp value)
    (or
     (equal value 'X)
     (equal value 'O))
    (is-cell-within-limits cell))
   (setf (nth cell *board*) value)
   nil))


;;; Cell and move predicates
(defun is-cell-within-limits (cell)
  (if
   (and
    (>= cell 0)
    (< cell board-size))
   t
   nil))


(defun is-cell-empty (cell)
   (if (symbolp cell)
    (if
     (equal cell '-)
     t
     nil)))


(defun is-cell-X (cell)
  (eql cell 'X))


(defun is-cell-O (cell)
  (eql cell 'O))



(defun is-move-valid (index)
  ;; Checks whether the user input is a valid cell and if the cell is empty
  ;; Acceptable values are numbers [0-8]
  (when
      (is-board-full)
    (print "Board is full. Game over.")
    (return-from is-move-valid nil))
  (when
      (or (symbolp index) (not (numberp index)))
    (format t "~a not a number" index)
    (return-from is-move-valid nil))
  (when (not (integerp index))
    (format t "~&~a not an integer" index)
    (return-from is-move-valid nil))
  (when (not (is-cell-within-limits index))
    (format t "~&Number ~a is outside the board limits. Select a number between 0 and ~a" index  (1- board-size))
    (return-from is-move-valid nil))
  (when
      (not (is-cell-empty (get-value-of index)))
    (format t "~&Cell ~a is captured" index)
    (return-from is-move-valid nil))
  (when
      (and (integerp index)(is-cell-within-limits index))
    (return-from is-move-valid t)))


;;; Player functions
(defun select-player ()
  (progn
    (format *query-io* "~&Select player [X/O]: ")
    (force-output *query-io*)
    (let ((user nil))
      (setf user (read))
      (if
       (or (equal user 'X) (equal user 'O))
       (progn
	 (setf *human* user)
	 (setf *computer* (oppponent-of user))
	 (format t "~&Human player: ~a" *human*)
	 (format t "~&Computer player: ~a" *computer*))
       (select-player)))))


(defun make-move (player cell)
  (if
   (is-move-valid cell)
   (set-value-of cell player)
   nil))


(defun human-move ()
  (if
   (not (is-board-full))
   (progn
     (format t "~&Player ~a select move: " *human*)
     (force-output *query-io*)
     (let ((move nil))
       (setf move (read))
       (if
	(not (is-move-valid move))
	(human-move)
	(progn
	  (make-move *human* move)
	  (format t "~&*** Player ~a made a move to cell ~a ***" *human* move)
	  (values))))))
  nil)



(defun computer-move ()
  (let ((free-cells (has-board-free-cells)))
    ;;(print free-cells)
    (if free-cells
        (progn
	  (let ((index (random (length free-cells))))
	    (make-move *computer* (nth index free-cells))
	    (format t "~&*** Player ~a made a move to cell ~a ***" *computer* index)
	    (values)))
	(progn
	  (format t "No moves available")
	  nil))))


(defun get-current-player ()
  *current-player*)


  
(defun next-player ()
  (cond
    ((equal *current-player* *human*)
     (setf *current-player* *computer*))
    ((equal *current-player* *computer*)
     (setf *current-player* *human*))
    (t nil)))


(defun oppponent-of (player)
  (cond
    ((equal player 'X) 'O)
    ((equal player 'O) 'X)
    (t nil)))



;;; Game win predicates
(defun is-row-win ()
  (let ((row-1 (subseq *board* 0 3))
        (row-2 (subseq *board* 3 6))
        (row-3 (subseq *board* 6 9)))

    (when
	(or (every #'is-cell-X row-1)
	    (every #'is-cell-X row-2)
	    (every #'is-cell-X row-3))
      (return-from is-row-win 'X))

    (when
	(or (every #'is-cell-O row-1)
	    (every #'is-cell-O row-2)
	    (every #'is-cell-O row-3))
      (return-from is-row-win 'O)))
  
  (return-from is-row-win nil))
   

(defun is-col-win ()
  (let ((col-1 (list (get-value-of 0)(get-value-of 3)(get-value-of 6)))
	(col-2 (list (get-value-of 1)(get-value-of 4)(get-value-of 7)))
	(col-3 (list (get-value-of 2)(get-value-of 5)(get-value-of 8))))
    (when
	(or (every #'is-cell-X col-1)
	    (every #'is-cell-X col-2)
	    (every #'is-cell-X col-3))
      (return-from is-col-win 'X))

    (when
	(or (every #'is-cell-O col-1)
	    (every #'is-cell-O col-2)
	    (every #'is-cell-O col-3))
      (return-from is-col-win 'O)))
  
  (return-from is-col-win nil))


(defun is-draw ()
 (if
  (and
   (is-board-full)
   (not (is-row-win))
   (not (is-col-win)))
 (return-from is-draw t)
 (return-from is-draw nil)))
  

(defun player-won ()
  (let ((row (is-row-win))
	(col (is-col-win)))
	(cond ((not (eql row nil)) (return-from player-won row))
	      ((not (eql col nil)) (return-from player-won col))
	      (t (return-from player-won nil)))))
  

(defun is-game-over ()
  (cond ((is-board-full) (return-from is-game-over t))
	((player-won) (return-from is-game-over t))
	(t (return-from is-game-over nil))))


(defun get-winner()
  (when (is-game-over)
    (if (is-draw)
	(return-from get-winner nil)
	(return-from get-winner (player-won)))))



    
(defun play-game ()
  (clear-board)
  (select-player)
  (display-board)

  (dotimes (i 9)
    (progn
      (human-move)
      (next-player)
      (display-board)
      (if (is-game-over)
	  (progn
	    (format t "~%Game over. Winner: ~a" (get-winner))
	    (return-from play-game (get-winner))))
      
      (computer-move)
      (next-player)
      (display-board)
      (if (is-game-over)
	  (progn
	    (format t "~%Game over. Winner: ~a" (get-winner))
	    (return-from play-game (get-winner)))))))
