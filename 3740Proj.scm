;Global variables:
(define stack '()) ;The stack, represented as a list
(define temp 0) ;The temporary variable for POP operation

;Temp vars
(define t 0) 
(define s1 0)
(define s2 0)
(define tempstack '())
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
(define (DEF a n) ;;This function needs to be chaged/fixed
  (set! a n))
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
(define (SWAP)
  (set! s1 (car stack)) ;s1 is the top of the stack
  (cdr stack) ;Take it off of the stack
  (set! s2(car stack)) ;s2 is the second element from the stack
  (cdr stack) ;Take the second element off the stack
  (push s1) ;Push s1 back onto the stack
  (push s2)) ;Push s2 back onto the stack
        
;10) REV
;  -reverse all the elements in the stack
;  -(the top becomes the bottom while the bottom becomes the top)
(define (REV s)
  (if (null? s) ;Base case for recursion, check if the stack is empty
      (set! s tempstack) ;If it is empty, set the stack to the temporary stack
      ;ELSE
      (begin
        (set! tempstack (cons (car s) tempstack))
        (REV (cdr s)))))

;11) STACK 
;  -print out the current content of the stack, from top to the bottom.
(define (STACK)
  (display stack))

;12) CLEAR 
;  -clears the contents of the stack 
(define (CLEAR s)
  (if (null? s) ;if the stack is empty
      0 ;do nothing
      (CLEAR (cdr s))))
;;This needs to be improved on, also can't be run unless we comment out the STACK function
      

;;Random vars for testing:
(define p 1)
(define list '(1 2 3))


