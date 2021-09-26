waff.el - Add waffles to Emacs

Use with (require 'waffle-mode)

Call waffle-mode to enter the major mode in an empty buffer. It will erase anything in the buffer!

Make tasty waffles by filling the waffle iron with waffle dough, cooking it to perfection, and then putting toppings on your waffle.

Keymap (I'll make this look nicer in the future):
(define-key map "i" 'waffle-view-iron)
(define-key map "p" 'waffle-view-plate)
(define-key map "f" 'waffle-fill)
(define-key map "c" 'waffle-close)
(define-key map "l" 'waffle-flip)
(define-key map "o" 'waffle-open)
(define-key map "r" 'waffle-remove)
(define-key map "e" 'waffle-eat)
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
;; (define-key map "?" 'waffle-help) ; Will add this eventually