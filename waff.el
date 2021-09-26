;;; waff.el --- Tasty Emacs waffles

(if (boundp 'waffle-mode-map) (makunbound 'waffle-mode-map)) ; convenience
(defvar waffle-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "i" 'waffle-view-iron)
	(define-key map "p" 'waffle-view-plate)
	(define-key map "f" 'waffle-fill)
	(define-key map "c" 'waffle-close)
	(define-key map "l" 'waffle-flip)
	(define-key map "o" 'waffle-open)
	(define-key map "r" 'waffle-remove)
	(define-key map "e" 'waffle-eat)
	;; (define-key map "a" 'waffle-add)
	(define-key map "\C-c\C-b" 'waffle-add-bananas)
	(define-key map "\C-c\C-u" 'waffle-add-butter)
	(define-key map "\C-c\C-l" 'waffle-add-blueberries)
	(define-key map "\C-c\C-m" 'waffle-add-maplesyrup)
	(define-key map "\C-c\C-r" 'waffle-add-raspberries)
	(define-key map "\C-c\C-s" 'waffle-add-strawberries)
	(define-key map "\C-c\C-w" 'waffle-add-whippedcream)
	(define-key map "\C-c\C-y\C-b" 'waffle-add-yogurt-blueberry)
	(define-key map "\C-c\C-y\C-k" 'waffle-add-yogurt-keylime)
	(define-key map "\C-c\C-y\C-m" 'waffle-add-yogurt-mango)
	(define-key map "\C-c\C-y\C-s" 'waffle-add-yogurt-strawban)
	; Alts
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

(add-hook 'waffle-mode-hook (lambda () (interactive)
							  (visual-line-mode -1)
							  (setq-local truncate-lines t)
							  (setq-default waffle-iron-cooked1 0)
							  (setq-default waffle-iron-cooked2 0)
							  (if (timerp waffle-timer) (cancel-timer waffle-timer))))

;; Cooking variables
(defvar-local waffle-iron-view t
  "Current view. t for iron view, nil for plate view.")
(defvar-local waffle-iron-filled nil
  "Whether waffle iron is filled.")
(defvar-local waffle-iron-flipped nil
  "Whether waffle iron is flipped.")
(defvar-local waffle-iron-open nil
  "Whether waffle iron is open.")
(defvar-local waffle-plate-filled nil
  "Whether plate is filled.")

(defvar waffle-iron-cooked1 0
  "How cooked first side of waffle in iron is.")
(defvar waffle-iron-cooked2 0
  "How cooked second side of waffle in iron is.")
(defvar waffle-plate-cooked1 0
  "How cooked first side of waffle on plate is.")
(defvar waffle-plate-cooked2 0
  "How cooked second side of waffle on plate is.")
(defvar waffle-timer nil)

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

(defun waffle-view-iron ()
  "View waffle iron."
  (interactive)
  (setq-local waffle-iron-view t)
  (waffle-draw-iron-view))

(defun waffle-view-plate ()
  "View waffle plate."
  (interactive)
  (setq-local waffle-iron-view nil)
  (waffle-draw-plate-view))

(defun waffle-fill ()
  "Fill waffle iron with dough."
  (interactive)
  (if waffle-iron-filled
	  (message "Waffle iron already filled.")
	(if waffle-iron-open
		(progn (message "Waffle iron filled.")
			   (setq-local waffle-iron-filled t)
			   (setq waffle-timer (run-at-time "1 sec" 5 'waffle-cook))
			   (if waffle-iron-view
				   (waffle-draw-iron-view)))
	(message "Waffle iron is not open."))))

(defun waffle-close ()
  "Close waffle iron."
  (interactive)
  (if waffle-iron-open
	  (progn (message "Waffle iron closed.")
			 (setq-local waffle-iron-open nil)
			 (if waffle-iron-view
				 (waffle-draw-iron-view)))
	(message "Waffle iron already closed.")))

(defun waffle-flip ()
  "Flip waffle iron."
  (interactive)
  (if waffle-iron-filled
	  (if waffle-iron-open
		  (message "Waffle iron not closed.")
		(progn (message "Waffle flipped.")
			   (if waffle-iron-flipped
				   (setq-local waffle-iron-flipped nil)
				 (setq-local waffle-iron-flipped t))))
	(message "No waffle to flip.")))

(defun waffle-open ()
  "Open waffle iron."
  (interactive)
  (if waffle-iron-open
	  (message "Waffle iron already open.")
	(progn (message "Waffle iron opened.")
		   (setq-local waffle-iron-open t)
		   (if waffle-iron-view
			   (waffle-draw-iron-view)))))

(defun waffle-remove ()
  "Remove waffle from waffle iron."
  (interactive)
  (if waffle-iron-filled
	  (if waffle-iron-open
		  (if waffle-plate-filled
			  (message "No room on plate.")
			(progn (message "Waffle removed.")
				   (setq-local waffle-plate-cooked1 (if waffle-iron-flipped
														waffle-iron-cooked2
													  waffle-iron-cooked1))
				   (setq-local waffle-plate-cooked2 (if waffle-iron-flipped
														waffle-iron-cooked1
													  waffle-iron-cooked2))
				   (cancel-timer waffle-timer)
				   (setq-local waffle-plate-filled t)
				   (setq-local waffle-iron-filled nil)
				   (setq-local waffle-iron-flipped nil)
				   (setq-default waffle-iron-cooked1 0)
				   (setq-default waffle-iron-cooked2 0)
				   (if waffle-iron-view
					   (waffle-draw-iron-view)
					 (waffle-draw-plate-view))))
		(message "Waffle iron not open."))
	(message "No waffle to remove.")))

(defun waffle-eat ()
  "Eat waffle!"
  (interactive)
  (if waffle-plate-filled
	  (progn (if (< 40 (max waffle-plate-cooked1 waffle-plate-cooked2))
				 (if (> 20 (min waffle-plate-cooked1 waffle-plate-cooked2))
					 (message "Eewww. That one was really unevenly cooked!")
			   (message "*Crunch* ...Wow, that one sure was toasty!"))
			   (if (> 20 (min waffle-plate-cooked1 waffle-plate-cooked2))
				   (message "*Mush* ...mmmm... tastes like... salmonella...")
				 (message "Waffle eaten!")))
			 (setq-local waffle-plate-filled nil)
			 (setq-local waffle-plate-cooked1 0)
			 (setq-local waffle-plate-cooked2 0)
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
			 (setq-local waffle-yogurt-strawbana nil)
			 (if (not waffle-iron-view) (waffle-draw-plate-view)))
	(message "No waffle to eat ;(")))

(defun waffle-cook ()
  "Increment waffle cookedness."
  (if waffle-iron-flipped
	  (progn (setq-default waffle-iron-cooked2 (+ waffle-iron-cooked2 4))
			 (if (not waffle-iron-open)
				 (setq-default waffle-iron-cooked1 (+ waffle-iron-cooked1 1))))
	(progn (setq-default waffle-iron-cooked1 (+ waffle-iron-cooked1 4))
		   (if (not waffle-iron-open)
			   (setq-default waffle-iron-cooked2 (+ waffle-iron-cooked2 1)))))
  (if (and (eq major-mode 'waffle-mode) waffle-iron-view)
	  (waffle-draw-iron-view)))

; There may be a better way to do the toppings :/
(defun waffle-add-bananas ()
  "Add bananas to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Bananas added to waffle")
			 (setq-local waffle-bananas t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▇" "yellow2" nil 10)))
	(message "No waffle on plate.")))
(defun waffle-add-butter ()
  "Add butter to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Butter added to waffle")
			 (setq-local waffle-butter t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "█" "light goldenrod" nil 40)))
	(message "No waffle on plate.")))
(defun waffle-add-blueberries ()
  "Add blueberries to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Blueberries added to waffle")
			 (setq-local waffle-blueberries t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▅" "dark blue" "midnight blue" 10)))
	(message "No waffle on plate.")))
(defun waffle-add-maplesyrup ()
  "Add maple syrup to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Maple syrup added to waffle")
			 (setq-local waffle-maplesyrup t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "█" "saddle brown" nil 50)))
	(message "No waffle on plate.")))
(defun waffle-add-raspberries ()
  "Add raspberries to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Raspberries added to waffle")
			 (setq-local waffle-raspberries t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▅" "firebrick" "indian red" 10)))
	(message "No waffle on plate.")))
(defun waffle-add-strawberries ()
  "Add strawberries to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Strawberries added to waffle")
			 (setq-local waffle-strawberries t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▅" "dark red" "brown" 10)))
	(message "No waffle on plate.")))
(defun waffle-add-whippedcream ()
  "Add whipped cream to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Whipped cream added to waffle")
			 (setq-local waffle-whippedcream t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "█" "ivory" nil 50)))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-blueberry ()
  "Add blueberry yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Blueberry yogurt added to waffle")
			 (setq-local waffle-yogurt-blueberry t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▆" "slate blue" "light slate blue" 30)))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-keylime ()
  "Add key lime yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Key lime yogurt added to waffle")
			 (setq-local waffle-yogurt-keylime t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▆" "aquamarine" "mint cream" 30)))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-mango ()
  "Add mango yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Mango yogurt added to waffle")
			 (setq-local waffle-yogurt-mango t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▆" "dark orange" "orange" 30)))
	(message "No waffle on plate.")))
(defun waffle-add-yogurt-strawban ()
  "Add strawberry banana yogurt to waffle."
  (interactive)
  (if waffle-plate-filled
	  (progn (message "Strawberry banana yogurt added to waffle")
			 (setq-local waffle-yogurt-strawbana t)
			 (if (not waffle-iron-view)
				 (waffle-randtop "▆" "light coral" "hot pink" 30)))
	(message "No waffle on plate.")))

(defun waffle-insert (string fg bg)
  (insert (propertize string 'face `(:foreground ,fg :background ,bg))))

(defun waffle-replace (string fg bg len)
  "Replace len characters at point with string."
  (delete-char len)
  (waffle-insert string fg bg))

(defun waffle-randtop (string fg bg num)
  "Randomly put toppings on waffle."
  (save-excursion
	(dotimes (i num)
	  (goto-char (+ 34 (random 577)))
	  (if (not (eq (% (point) 34) 0))
		  (waffle-replace string fg bg 1)))))

(defun waffle-draw-iron-view ()
  "Draw waffle iron view."
  (save-excursion
	(erase-buffer)
	(waffle-insert
"               ███               \n" "#3a3a3a" nil)
	(waffle-draw-big-rect "#3a3a3a")
	(if waffle-iron-open
		(if waffle-iron-filled
			(waffle-draw-waffle)
		  (waffle-draw-pattern "#555555")))))

(defun waffle-draw-plate-view ()
  "Draw plate view."
  (save-excursion
	(erase-buffer)
	(insert "                                 \n")
	(waffle-draw-big-rect "bisque")
	(if waffle-plate-filled
		(waffle-draw-waffle))))

(defun waffle-draw-waffle ()
  (goto-char 70)
  (waffle-draw-small-rect "chocolate")
  (waffle-draw-pattern "peru"))

(defun waffle-draw-big-rect (color)
  (waffle-insert
"█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████
█████████████████████████████████" color nil)
  (goto-char 0))

(defun waffle-draw-small-rect (color)
  (dotimes (i 15)
	(waffle-replace
	 "███████████████████████████████" color nil 31)
	(forward-char 3))
  (goto-char 0))

(defun waffle-draw-pattern (color)
	(dotimes (i 4)
	  (forward-char 102)
	  (dotimes (j 6)
		(forward-char 3)
		(waffle-replace "██" color nil 2))
	  (forward-char 4)))

(provide 'waffle-mode)
