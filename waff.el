;;; waff.el --- Tasty Emacs waffles

(defvar waffle-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "f" 'waffle-fill)
	(define-key map "l" 'waffle-flip)
	(define-key map "r" 'waffle-remove)
	(define-key map "e" 'waffle-eat)
	map)
  "Keymap for `waffle-mode'.")

(define-derived-mode waffle-mode nil "waffle"
  "Major mode for making waffles."
  (use-local-map waffle-mode-map))

(add-hook 'waffle-mode-hook (lambda () (interactive) (visual-line-mode -1) (setq truncate-lines t)))
