;;; waff.el --- Tasty Emacs waffles

(if (boundp 'waffle-mode-map) (makunbound 'waffle-mode-map)) ; convenience
(defvar waffle-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "f" 'waffle-fill)
	(define-key map "l" 'waffle-flip)
	(define-key map "r" 'waffle-remove)
	(define-key map "e" 'waffle-eat)
	(define-key map "a" 'waffle-add)
	(define-key map "\C-c\C-b" 'waffle-add-bananas)
	(define-key map "\C-c\C-u" 'waffle-add-butter)
	(define-key map "\C-c\C-r" 'waffle-add-blueberries)
	(define-key map "\C-c\C-m" 'waffle-add-maplesyrup)
	(define-key map "\C-c\C-r" 'waffle-add-raspberries)
	(define-key map "\C-c\C-s" 'waffle-add-strawberries)
	(define-key map "\C-c\C-w" 'waffle-add-whippedcream)
	(define-key map "\C-c\C-yb" 'waffle-add-yogurt-blueberry)
	(define-key map "\C-c\C-yk" 'waffle-add-yogurt-keylime)
	(define-key map "\C-c\C-ym" 'waffle-add-yogurt-mango)
	(define-key map "\C-c\C-ys" 'waffle-add-yogurt-strawban)
	;; (define-key map "?" 'waffle-help)
	map)
  "Keymap for `waffle-mode'.")

(define-derived-mode waffle-mode nil "Waffle"
  "Major mode for making waffles."
  (use-local-map waffle-mode-map))

(if (boundp 'evil-mode) ; maybe make this check better
	(evil-make-overriding-map waffle-mode-map))
;; (add-to-list 'evil-motion-state-modes 'waffle-mode) ; Doesn't work for all binds

(add-hook 'waffle-mode-hook (lambda () (interactive) (visual-line-mode -1) (setq-local truncate-lines t)))

;; Cooking variables
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

;; Topping variables
(defvar-local waffle-bananas nil
  "Whether waffle has bananas.")
(defvar-local waffle-butter nil
  "Whether waffle has butter.")
(defvar-local waffle-blueberries nil
  "Whether waffle has blueberries.")
(defvar-local waffle-maplesyrup nil
  "Whether waffle has maple syrup.")
(defvar-local waffle-raspberries nil
  "Whether waffle has raspberries.")
(defvar-local waffle-strawberries nil
  "Whether waffle has strawberries.")
(defvar-local waffle-whippedcream nil
  "Whether waffle has whipped cream.")
(defvar-local waffle-yogurt-blueberry nil
  "Whether waffle blueberry yogurt.")
(defvar-local waffle-yogurt-keylime nil
  "Whether waffle key lime yogurt.")
(defvar-local waffle-yogurt-mango nil
  "Whether waffle mango yogurt.")
(defvar-local waffle-yogurt-strawbana nil
  "Whether waffle has strawberry banana yogurt.")

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
			 (setq-local waffle-cooked2 0)
			 (setq-local waffle-bananas nil)
			 (setq-local waffle-butter nil)
			 (setq-local waffle-blueberries nil)
			 (setq-local waffle-maplesyrup nil)
			 (setq-local waffle-raspberries nil)
			 (setq-local waffle-strawberries nil)
			 (setq-local waffle-whippedcream nil)
			 (setq-local waffle-yogurt-blueberry nil)
			 (setq-local waffle-yogurt-keylime nil)
			 (setq-local waffle-yogurt-mango nil)
			 (setq-local waffle-yogurt-strawbana nil))
	(message "No waffle to eat ;(")))

(defun waffle-cook ()
  "Increment waffle cookedness."
  (if waffle-iron-flipped
	  (progn (setq-local waffle-cooked1 (+ waffle-cooked1 1))
			 (setq-local waffle-cooked2 (+ waffle-cooked2 4)))
	(progn (setq-local waffle-cooked1 (+ waffle-cooked1 4))
		   (setq-local waffle-cooked2 (+ waffle-cooked2 1))))
  (if waffle-iron-filled (run-with-timer 5 nil 'waffle-cook)))

; There may be a better way to do this :/
(defun waffle-add-bananas ()
  "Add bananas to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Bananas added to waffle")
			 (setq-local waffle-bananas t))
	(message "No waffle on plate.")))
(defun waffle-add-butter ()
  "Add butter to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Butter added to waffle")
			 (setq-local waffle-butter t))
	(message "No waffle on plate.")))
(defun waffle-add-blueberries ()
  "Add blueberries to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Blueberries added to waffle")
			 (setq-local waffle-blueberries t))
	(message "No waffle on plate.")))
(defun waffle-add-maplesyrup ()
  "Add maple syrup to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Maple syrup added to waffle")
			 (setq-local waffle-maplesyrup t))
	(message "No waffle on plate.")))
(defun waffle-add-raspberries ()
  "Add raspberries to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Raspberries added to waffle")
			 (setq-local waffle-raspberries t))
	(message "No waffle on plate.")))
(defun waffle-add-strawberries ()
  "Add strawberries to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Strawberries added to waffle")
			 (setq-local waffle-strawberries t))
	(message "No waffle on plate.")))
(defun waffle-add-whippedcream ()
  "Add whipped cream to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Whipped cream added to waffle")
			 (setq-local waffle-whippedcream t))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-blueberry ()
  "Add blueberry yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Blueberry yogurt added to waffle")
			 (setq-local waffle-yogurt-blueberry t))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-keylime ()
  "Add key lime yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Key lime yogurt added to waffle")
			 (setq-local waffle-yogurt-keylime t))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-mango ()
  "Add mango yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Mango yogurt added to waffle")
			 (setq-local waffle-yogurt-mango t))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-strawban ()
  "Add strawberry banana yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Strawberry banana yogurt added to waffle")
			 (setq-local waffle-yogurt-strawbana t))
	(message "No waffle on plate.")))

(provide 'waffle-mode)
