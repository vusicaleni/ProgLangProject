;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CPSC 3470 
;Final Project
;Implement an interpreter for a stack based language called U of L which
;used reverse polish notation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Declarations of variables for the concatenator function and
;Declarations for the command prompt.
(define promptCommand '())
(define inputList '())
;Global variables:
(define stack '()) ;The stack, represented as a list
(define temp 0) ;The temporary variable for POP operation
(define nums '()) ;A list of numbers that are pushed onto the stack using the PUSH operator
(define lastList '()) ;A list that keeps track of the last operation done
;Temp vars
(define t 0) 
;(define s1)
;(define s2)
(define var1 0)
(define var2 0)
(define tempstack '())

;************************************************************************************************
;Read User Input Function
(define (read-keyboard-as-list)
  (let ((char (read-char)))
    (if (char=? char #\newline)
        '()
        (let loop ((char char))
          (if (char=? char #\newline)
              '()
              (cons char (loop (read-char)))
              )
          )
        )
    )
  )

;************************************************************************************************
;The Program Loop
;General Idea:
;It takes read-input as a parameter and calls the parser, this happens inifnitely.  
(define (infiniteLoop) 
  (display "UOFL>")
  (parser (read-keyboard-as-list))
  (infiniteLoop))

;************************************************************************************************
;The Parser
;General Idea: 
;Parser gets a list as an input, the list will be read until a space.  This list will be the command call for the language.
;If there is no space, it keeps reading.  If there is, then it will return the FUNCTION which calls all of the other functions.
;functionGenerator will direct to all other function calls
(define parser (lambda (inputList)
                    
					;If everything in the input string has been eaten, then:
					;1. Call the functionCaller with the promptCommand
					;2. Set the promptCommand to null so that no more commands can be made.
                 (if (<= (length inputList) 0)
                     (begin
                       (functionCaller promptCommand)
                       (set! promptCommand '())
                       )
					;Else the input string is still being eaten
                     (begin
					;If the first inputList char ISN'T a space or zero then we begin
					;1. append the latest inputList to the promptCommand
					;2. recurse on the rest of the inputList
					;3. and set the promptCommand to null
                       (if (or (not(equal? (car (list inputList)) #\space)) 
                               (eq? (length inputList) 0))
			   (begin
			     (set! promptCommand (append promptCommand (list(car inputList))))
                             (parser (cdr  inputList))                           ;2
                             (set! promptCommand '()))                            ;3 
					;If the inputList char IS a space or zero then we begin
					;1. Call the functionCaller
					;2. Clear the promptCommand
					;3. Recurse, but skip the space, so that the operands can be used.
			   (begin 
			     (functionCaller promptCommand)  ;1
			     (set! promptCommand '())         ;2
			     (parser (cdr inputList))        ;3
			     )        
			   )
		       )
		     )
		 )
  )

(define (functionCaller promptCommand )
  (set! promptCommand (list->string promptCommand))
					; (if (= ((string-length promptCommand) 0))
					;   0)
					; (cond   ;Start of condition
					;   ((integer? (promptCommand)) ;If the promptCommand  has an integer,
					;   (set! nums(append num(list(string->number promptCommand)))) ;Add the integer in the command prompt to the list of nums
					;   (if (< 2 (length lastList))  ;If the length of this list is less than 2
					;	(set! lastList (cdr lastList)) ;set the list to the last operation in the list
					;	(values))  ;Return the values of the last operation done
  (cond					;   (PUSH (string->number promptCommand))
  ((equal? promptCommand "DROP")(DROP))
  ((equal? promptCommand "POP")(POP))
  ((equal? promptCommand "DUP")(DUP))
  ((equal? promptCommand "DEFINE")(DEF))
  ((equal? promptCommand "SAVE")(SAVE))
  ((equal? promptCommand "SWAP")(SWAP))
  ((equal? promptCommand "STACK")(STACK))
  ((equal? promptCommand "REV")(REV stack)) ;Call the reverse function on the stack
  ((equal? promptCommand "CLEAR")(CLEAR stack)) ;Call the clear function on the stack
  )
  )





;************************************************************************************************   
;Functions:
;1) Push operation
;  -Pushes an element onto the stack
;  -Implied in the comand line 
;  (ie: UofL> 40 pushes 40 onto the stack
;  -If we add the element x to the front of the list this will simplify POP and DROP
(define (PUSH x)
  (set! stack (cons x stack))) ;Add element x to the (front) stack 
   
;3) . message operation
;  -Prints "<message>" to the screen

;4) define operation
;  -We can use this to define a variable
;  -ie:UofL> define a 10 //Defines a variable a to be 10
(define (DEF a n) ;;This function needs to be chaged/fixed
  (set! a n))

;************************************************************************************************
;Basic stack operations

;5)DROP operation
;  -Pops the top element off the stack and throws it away
(define (DROP)
  (if (null? stack)
      (display "The stack is empty")
      (if (pair? stack)
	  (set! stack (cdr stack)))))


				;Pop the front element off the stack

;6)POP operation
;  -Pops the top element off the stack but saves to a temporary location
(define (POP)
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

;************************************************************************************************

;condIF
;   This is the basic if condition
; something like this maybe ?
; ( define (condIf x) // ex. given 0 
	;(begin 
		;(push x ) pushs 0 on to stack 
		;(pop x ) // next we need to pop the top 2 elements 
		;(set! temp s1) // since temp is rewritten after each pop set temp to s1 
		;(pop stack) // pop the second element 
		;(set! temp s2) // set temp so s2 
		;(LessThan s1 s2) // pass them to the lessthan defintion 
		;(push temp) // we need lessThan to save a temp variable
		;(if 1 = temp
			;(display " top of stack is greater ") // should we be callign a func here?
			;(display " stack is smaller " ))))// 
			
;(define (condIF condition)
;  (begin
;  (if(operator x y))))

;condTHEN
;   This is the basic then condition

;condELSE
;   This is the basic else condition

  
;************************************************************************************************  
  
;Comparison Operators (i.e. < > <= =>, etc...)

;Operator <
(define (LESSTHAN s1 s2)
  (if (< s1 s2)
      (begin
        DROP s1
        DROP s2
        1)
      0)) ;DOES THIS RETURN 1 OR PUSH 1 BACK ONTO THE STACK

;Operator >
(define (GREATERTHAN s1 s2)
  (if (> s1 s2)
      (begin
        DROP s1
        DROP s2
        1)
      0)) ;DOES THIS RETURN 1 OR PUSH 1 BACK ONTO THE STACK

;Operator =
(define (EQUAL s1 s2)
  (if (= s1 s2)
      (begin
        DROP s1
        DROP s2
        1)
      0)) ;DOES THIS RETURN 1 OR PUSH 1 BACK ONTO THE STACK

;Operator !=
(define (NOTEQUAL s1 s2)
  (if (not (eq? s1 s2))
      (begin
        DROP s1
        DROP s2
        1)
      0)) ;DOES THIS RETURN 1 OR PUSH 1 BACK ONTO THE STACK

;Operator <=
(define (LESSTHANOREQUAL s1 s2)
  (if (<= s1 s2)
      (begin
        DROP s1
        DROP s2
        1)
      0)) ;DOES THIS RETURN 1 OR PUSH 1 BACK ONTO THE STACK

;Operator >=
(define (GREATERTHANOREQUAL s1 s2)
  (if (>= s1 s2)
      (begin
        DROP s1
        DROP s2
        1)
      0)) ;DOES THIS RETURN 1 OR PUSH 1 BACK ONTO THE STACK


;Conditional statements
;(define (IF)
;  (cond (LESSTHAN s1 s2

         
         
         ;(begin
;(if (= condition GREATERTHANOREQUAL)
;  (if (>= s1 s2)
;      (begin
;        DROP s1
;        DROP s2
;        1)
;      0)))) ;DOES THIS RETURN 1 OR PUSH 1 BACK ONTO THE STACK


;************************************************************************************************
;LOOPS

;LOOP
;   This function allows us to loop.
;   It is boolean controlled
;   It starts with a conditional followed by a loop body

;ead-keyboard-as-list) 


;************************************************************************************************
;FUNCTION DEFINITIONS

;FUNC$ 
;  marks the start of a function definition that uses static scoping

;CNUF
;  marks the end of the definition





(infiniteLoop)

;;Random vars for testing:
(define p1 2)
(PUSH p1)
(define list '(1 2 3))
(define p2 2)
;(PUSH p1)
;(PUSH p2)


