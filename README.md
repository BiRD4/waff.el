# waff.el - Add waffles to Emacs

Make tasty waffles by filling the waffle iron with waffle dough, cooking it to perfection, and then putting toppings on your waffle.

Use with (require 'waffle-mode)

Don't call waffle-mode to enter the major mode in a buffer. **It will erase your buffer!**
Call waffle-open instead. You can bind it like this: (global-set-key (kbd "C-c w") 'waffle-open)

Keymap:
- <kbd>i</kbd> - waffle-view-iron                
- <kbd>p</kbd> - waffle-view-plate               
- <kbd>f</kbd> - waffle-fill                     
- <kbd>c</kbd> - waffle-close                    
- <kbd>l</kbd> - waffle-flip                     
- <kbd>o</kbd> - waffle-open                     
- <kbd>r</kbd> - waffle-remove                   
- <kbd>e</kbd> - waffle-eat                      
- C-c C-b     - waffle-add-bananas              
- C-c C-u     - waffle-add-butter               
- C-c C-l     - waffle-add-blueberries          
- C-c C-m     - waffle-add-maplesyrup           
- C-c C-r     - waffle-add-raspberries          
- C-c C-s     - waffle-add-strawberries         
- C-c C-w     - waffle-add-whippedcream         
- C-c C-y C-b - waffle-add-yogurt-blueberry     
- C-c C-y C-k - waffle-add-yogurt-keylime       
- C-c C-y C-m - waffle-add-yogurt-mango         
- C-c C-y C-s - waffle-add-yogurt-strawban      
- ? - waffle-help (will add later)

Some alternatives:
waffle-add-yogurt-blueberry     C-c C-y b
waffle-add-yogurt-keylime       C-c C-y k
waffle-add-yogurt-mango         C-c C-y m
waffle-add-yogurt-strawban      C-c C-y s