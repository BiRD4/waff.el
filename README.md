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
- <kbd>C-c C-b</kbd> - waffle-add-bananas              
- <kbd>C-c C-u</kbd> - waffle-add-butter               
- <kbd>C-c C-l</kbd> - waffle-add-blueberries          
- <kbd>C-c C-m</kbd> - waffle-add-maplesyrup           
- <kbd>C-c C-r</kbd> - waffle-add-raspberries          
- <kbd>C-c C-s</kbd> - waffle-add-strawberries         
- <kbd>C-c C-w</kbd> - waffle-add-whippedcream         
- <kbd>C-c C-y C-b</kbd> - waffle-add-yogurt-blueberry     
- <kbd>C-c C-y C-k</kbd> - waffle-add-yogurt-keylime       
- <kbd>C-c C-y C-m</kbd> - waffle-add-yogurt-mango         
- <kbd>C-c C-y C-s</kbd> - waffle-add-yogurt-strawban      
- <kbd>?</kbd> - waffle-help (will add later)

Some alternatives:
- <kbd>C-c C-y b</kbd> - waffle-add-yogurt-blueberry
- <kbd>C-c C-y k</kbd> - waffle-add-yogurt-keylime
- <kbd>C-c C-y m</kbd> - waffle-add-yogurt-mango
- <kbd>C-c C-y s</kbd> - waffle-add-yogurt-strawban