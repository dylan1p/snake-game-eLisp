;; Snake game

(defvar snake-buffer-name "*Snake*")


(defun snake ()
  "Start playing snake"
  (interactive)
  (switch-to-buffer snake-buffer-name)
  (snake-mode)
  (snake-init))

(define-derived-mode snake-mode special-mode
  "snake"
  (define-key snake-mode-map (kbd "<right>") 'snake-move-right)
  (define-key snake-mode-map (kbd "<left>") 'snake-move-left)
  (define-key snake-mode-map (kbd "<up>") 'snake-move-up)
  (define-key snake-mode-map (kbd "<down>") 'snake-move-down))

(defconst *area-height* 20
  "The height of the area")

(defconst *area-width* 40
  "The width of the area")

(defvar *snake-length* 5
  "The length of snake, beginning at 5")

(defvar *snake-body* (list)
  "A list of cells that are part of the snake's body. For
example, ((0 0) (0 1) (0 2)) is a snake of length 3")

(defvar *snake-head* nil
  "Will contain the coordinance of the head e.g (2,0)")

(defun snake-init()
  "Start a new game of snake"
  (setq *snake-body* nil)
  (setq *snake-area*
        (make-vector
         (* *area-height* *area-width*) ?\-))
  (snake-player-init)
  (snake-set-player)
  (snake-print-area)
  )

(defun snake-print-area ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *area-height*)
      (dotimes(column *area-width*)
        (insert "-"))
      (insert "\n"))

    (dolist (cell *snake-body*)
      (goto-char (point-min))
      (forward-line (1- (nth 0 cell)))
      (forward-char (nth 1 cell))
      (delete-forward-char 1)
      (insert "O"))

    (goto-char (point-max))));; Put cursor outside the area


(defun area-get-square (row column)
  "Get the value in the (row, column) square."
  (elt *snake-area* (+ column (* row *area-width*))))

(defun area-set-square (row column value)
  "Set the value in the (row, column) square."
  (aset *snake-area*
        (+ column
           (* row *area-width*)) value))

(defun snake-player-init ()
  "Set point of initial snake"
  (dotimes (length *snake-length*)
    (setq *snake-body*
          (append (list (list 0 (+ 15 (+ length 1)))) *snake-body*))))

(defun snake-set-player ()
  "Set the snakes squares in the area"
  (setq *snake-area*
        (make-vector
         (* *area-height* *area-width*) ?\-))
  (dotimes (length (length *snake-body*))
    (area-set-square
          (nth 0 (nth length *snake-body*))
          (nth 1 (nth length *snake-body*)) ?\O)))

(defun snake-move-right ()
  "Move snake to the right by setting extra
   point to the right and remove point from tail"
  (interactive)
  (message "move-right")
  (setq *snake-body*
        (butlast (cons (list (nth 0 (nth 0 *snake-body*)) ;;Add element to list
                             (+ 1 (nth 1 (nth 0 *snake-body*))))
                       *snake-body*)))
  (snake-set-player)
  (snake-print-area))

(defun snake-move-down ()
  "Move snake down by setting extra
   point to the bottom and remove point from tail"
  (interactive)
  (message "move-down")
  (setq *snake-body*
        (butlast (cons (list (+ 1 (nth 0 (nth 0 *snake-body*))) ;;Add element to list
                             (nth 1 (nth 0 *snake-body*)))
                       *snake-body*)))
  (snake-set-player)
  (snake-print-area))

(defun snake-move-left ()
  "Move snake to the left by setting extra
   point to the left and remove point from tail"
  (interactive)
  (message "move-left")
  (setq *snake-body*
        (butlast (cons (list (nth 0 (nth 0 *snake-body*)) ;;Add element to list
                             (- (nth 1 (nth 0 *snake-body*)) 1))
                       *snake-body*)))
  (snake-set-player)
  (snake-print-area))

(defun snake-move-up ()
  "Move snake to the up by setting extra
   point to the top and remove point from tail"
  (interactive)
    (message "move-up")
    (setq *snake-body*
          (butlast (cons (list (- (nth 0 (nth 0 *snake-body*)) 1) ;;Add element to list
                               (nth 1 (nth 0 *snake-body*)))
                         *snake-body*)))
    (snake-set-player)
    (snake-print-area))


