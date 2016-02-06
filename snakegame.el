;; Snake game

(defun snake ()
  "Start playing snake"
  (interactive)
  (switch-to-buffer "snake")
  (snake-mode)
  (snake-init))

(define-derived-mode snake-mode special-mode
  "snake")

(defvar *snake-area* nil
  "The field in which the snake can travel.")

(defconst *area-size* 50
  "The width and height of the area")

(defun snake-init()
  "Start a new game of snake"
  (setq *snake-area*
        (make-vector
         (* *area-size* *area-size*) ?\-))
  (snake-set-player)
  (snake-print-area))

(defun snake-print-area ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *area-size*)
      (dotimes(column *area-size*)
        (insert (area-get-square row column)))
      (insert "\n"))))

(defun area-get-square (row column)
  "Get the value in the (row, column) square."
  (elt *snake-area* (+ column (* row *area-size*))))

(defun area-set-square (row column value)
  "Set the value in the (row, column) square."
  (aset *snake-area*
        (+ column
           (* row *area-size*)) value))


(defun snake-set-player ()
  "prints a snake in the center of the screen"
  (area-set-square (\ *area-size* 2) (\ *area-size* 2) ?\O))
    
        

