;;; waff.el --- Tasty Emacs waffles

(defvar waffle-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "f" 'waffle-fill)
	(define-key map "l" 'waffle-flip)
	(define-key map "r" 'waffle-remove)
	(define-key map "e" 'waffle-eat)
	;(define-key map "?" 'waffle-help)
	map)
  "Keymap for `waffle-mode'.")

(define-derived-mode waffle-mode nil "Waffle"
  "Major mode for making waffles."
  (use-local-map waffle-mode-map))

(if (boundp 'evil-mode) ; maybe make this check better
	(evil-make-overriding-map waffle-mode-map))

(add-hook 'waffle-mode-hook (lambda () (interactive) (visual-line-mode -1) (setq-local truncate-lines t)))

(defvar-local waffle-iron-filled nil
  "Whether waffle iron is filled.")

(defvar-local waffle-iron-flipped nil
  "Whether waffle iron is flipped.")

(defvar-local waffle-plate-filled nil
  "Whether waffle plate is filled.")

(defvar-local waffle-cooked1 0
  "How cooked first side of waffle is.")

(defvar-local waffle-cooked2 0
  "How cooked second side of waffle is.")

(defun waffle-fill ()
  "Fill waffle iron with dough."
  (interactive)
  (if (not waffle-iron-filled)
	  (progn (message "Waffle iron filled.")
			 (setq-local waffle-iron-filled t)
			 (waffle-cook))
	(message "Waffle iron already filled.")))

(defun waffle-flip ()
  "Flip waffle iron."
  (interactive)
  (if waffle-iron-filled
	  (progn (message "Waffle flipped.")
			 (if waffle-iron-flipped
				 (setq-local waffle-iron-flipped nil)
			   (setq-local waffle-iron-flipped t)))
	(message "No waffle to flip.")))

(defun waffle-remove ()
  "Remove waffle from waffle iron."
  (interactive)
  (if waffle-iron-filled
	  (progn (message "Waffle removed.")
			 (setq-local waffle-iron-filled nil)
			 (setq-local waffle-iron-flipped nil)
			 (setq-local waffle-plate-filled t))
	(message "No waffle to remove.")))

(defun waffle-eat ()
  "Eat waffle!"
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Waffle eaten!")
			 (setq-local waffle-plate-filled nil)
			 (setq-local waffle-cooked1 0)
			 (setq-local waffle-cooked2 0))
	(message "No waffle to eat ;(")))

(defun waffle-cook ()
  "Increment waffle cookedness."
  (if waffle-iron-flipped
	  (progn (setq-local waffle-cooked1 (+ waffle-cooked1 1))
			 (setq-local waffle-cooked2 (+ waffle-cooked2 4)))
	(progn (setq-local waffle-cooked1 (+ waffle-cooked1 4))
		   (setq-local waffle-cooked2 (+ waffle-cooked2 1))))
  (if waffle-iron-filled (run-with-timer 5 nil 'waffle-cook)))

(provide 'waffle-mode)
