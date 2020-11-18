;; (defpackage :utilities
;;   (:use :common-lisp :utilities)
;;   (:export :print-hello))



(defstruct (cell (:print-function print-cell))
  (i -1)
  (j -1)
  (value '-))

(defun print-cell (c stream depth)
  (format stream "#<Cell (~a,~a):{~a}>" (cell-i c) (cell-j c) (cell-value c)))


(defun set-cell-value (p v)
  (setf (cell-value p) v))

(defun get-cell-value (p)
  (return-from get-cell-value (cell-value p)))


(defun set-cell-coords (p coords)
  (setf (cell-i p) (first coords))
  (setf (cell-j p) (second coords)))

			
(defun get-cell-coords (p)
  (let* ((i-coord (cell-i p))
	 (j-coord (cell-j p)))
    `(,i-coord ,j-coord)))

(defun update-cell (c &key (i nil) (j nil) (v nil))
  (when (not (eql i nil))
    (setf (cell-i c) i))
  (when (not (eql j nil))
    (setf (cell-j c) j))
  (when (not (eql v nil))
    (setf (cell-value c) v)))


(defun create-board ()
  (return-from create-board (make-array '(3 3)
					:element-type 'cell
					:initial-element (make-cell))))


(defun get-cell-in (board i j)
  (return-from get-cell-in (aref board i j)))


;;; EACH CELL REFERS TO THE SAME OBJECT IN MEMORY!
(defun initialize-board (board)
  (let ((ce nil))
    (destructuring-bind (m n) (array-dimensions board)
      (loop for row from 0 below m do
	   (loop for col from 0 below n do
		(setf ce (aref board row col))
		(format t "~%%Cell: ~a" (aref board row col))
		(format t "~%Board: ~a" board)
		(update-cell ce :i row :j col :v '-)
		(format t "~%Updated cell: ~a ~%" (get-cell-in board row col)))))))



(defun display-board (board)
  (let ((m (first (array-dimensions board)))) 
    (loop for row from 0 below m do
	 (loop for col from 0 below m do
	      (format t "~&[~a,~a] ~a"
		      row col (get-cell-value (aref board row col)))))))
