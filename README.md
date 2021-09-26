# waff.el - Add waffles to Emacs

Make tasty waffles by filling the waffle iron with waffle dough, cooking it to perfection, and then putting toppings on your waffle.

Use with (require 'waffle-mode)

Don't call waffle-mode to enter the major mode in a buffer. **It will erase your buffer!**
Call waffle-open instead. You can bind it like this: (global-set-key (kbd "C-c w") 'waffle-open)

Keymap:
waffle-view-iron                i
waffle-view-plate               p
waffle-fill                     f
waffle-close                    c
waffle-flip                     l
waffle-open                     o
waffle-remove                   r
waffle-eat                      e
waffle-add-bananas              C-c C-b
waffle-add-butter               C-c C-u
waffle-add-blueberries          C-c C-l
waffle-add-maplesyrup           C-c C-m
waffle-add-raspberries          C-c C-r
waffle-add-strawberries         C-c C-s
waffle-add-whippedcream         C-c C-w
waffle-add-yogurt-blueberry     C-c C-y C-b
waffle-add-yogurt-keylime       C-c C-y C-k
waffle-add-yogurt-mango         C-c C-y C-m
waffle-add-yogurt-strawban      C-c C-y C-s
waffle-help ? (will add later)

Some alternatives:
waffle-add-yogurt-blueberry     C-c C-y b
waffle-add-yogurt-keylime       C-c C-y k
waffle-add-yogurt-mango         C-c C-y m
waffle-add-yogurt-strawban      C-c C-y s