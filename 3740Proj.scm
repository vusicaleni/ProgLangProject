
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CPSC 3470 
;Final Project
;Implement an interpreter for a stack based language called U of L which
;used reverse polish notation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Global variables:
(define stack '()) ;The stack, represented as a list
(define temp 0) ;The temporary variable for POP operation
(define t 0) ;Another temporary value
;Functions:
;1) Push operation
;  -Pushes an element onto the queue
;  -Implied in the comand line 
;  (ie: UofL> 40 pushes 40 onto the stack
;  -If we add the element x to the front of the list this will simplify POP and DROP
(define (PUSH x )
  (set! stack (cons x stack))) ;Add element x to the (front) stack 
   
;3) . message operation
;  -Prints "<message>" to the screen
;4) define operation
;  -We can use this to define a variable
;  -ie:UofL> define a 10 //Defines a variable a to be 10
;5)DROP operation
;  -Pops the top element off the stack and throws it away
(define (DROP x)
  (if (null? stack);Check if the stack is empty
      (display "The stack is empty") ;If the stack is empty, display
      ;Else
      (set! stack (cdr stack)))) ;Pop the front element off the stack
;6)POP operation
;  -Pops the top element off the stack but saves to a temporary location
(define (POP x)
  (if (null? stack);Check if the stack is empty
   (display "The stack is empty");If the stack is empty, display
   ;Else
   (begin
     (set! temp (car stack)) ;set the temporary value to the top element of the stack
     (set! stack (cdr stack))))) ;pop the first element of the stack.

;7)SAVE operation
;  -Pushes the element from the temporary location to the top of the stack
;  -This function will be used to perform intermediate operations
(define (SAVE x)
  (push temp)) ;Add the temp value back to the stack

;8)DUP
;  -Duplicates the top element and pushes it to the top of the stack
(define (DUP)
  (= t (car stack)) ;Set t to the top element of the stack
  (push t)) ;Push t onto the stack
  
;9)SWAP
;  -swap the top two elements on the top of the stack.

;10) REV
;  -reverse all the elements in the stack
;  -(the top becomes the bottom while the bottom becomes the top)
;11) STACK 
;  -print out the current content of the stack, from top to the bottom.
;12) CLEAR 
;  -clears the contents of the stack 


;;Random vars for testing:
(define p 1)
(define list '(1 2 3))

