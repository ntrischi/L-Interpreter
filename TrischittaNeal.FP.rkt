;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           FINAL_VERSION          ;
;                                  ;
; Neal Trischitta                  ;
; Programming Languages - CS 496   ; 
; May 08, 2013                     ; 
; Final Project      		   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#lang eopl
(require racket/string)

;;GIVEN - Stores Statments ;;
(define-datatype statement statement?
  (add1 (V symbol?))
  (sub1 (V symbol?))
  (skip (V symbol?))
  (if-goto (V symbol?)
           (l symbol?))
;;PART 3
  (goto-L (l symbol?))
  (zero-V (V symbol?))
  (assign (V1 symbol?)
          (V2 symbol?)))
  
;; GIVEN - Stores Instruction ;;
(define-datatype instruction instruction?
(labeled (l symbol?)
         (i statement?))
(unlabeled (i statement?)))

;; GIVEN - Stores entire Program ;;
(define-datatype program program?
(a-program (l (list-of instruction?))
           ))
;; GIVEN - Stores Statement 
(define-datatype state state?
  (empty-state)
  (extend-state (V symbol?) (m number?) (s state?)))

;; GIVEN - Creates a program counter
(define pc?
  (lambda (i) 
    (and (number? i) (> i 0))))

;; GIVEN - Creates a Snapshot
(define-datatype snapshot snapshot?
  (a-snapshot (i pc?)(s state?)))



;; QUESTION 1 - PART A: Pretty Print - Returns the Concrete Syntax of LANG
(define pretty-print
  (lambda (p)
    (if (null? p) ""
        (cases program p
          (a-program (instruct) (pretty-print-instructions instruct ""))))))

(define (pretty-print-instructions list string)
  (cond
    [(null? list) string]
    [else
     (cases instruction (car list)
       (labeled (l i) (string-append "(" (symbol->string l) ") " (pretty-print-statement i)  (pretty-print-instructions (cdr list) string)))
       (unlabeled (i) (string-append  (pretty-print-statement i)  (pretty-print-instructions (cdr list) string))))]))

(define pretty-print-statement
  (lambda (s)
         (cases statement s
           (add1 (V)  (string-append (symbol->string V) " <- " (symbol->string V) " + 1 "))
           (sub1 (V) (string-append (symbol->string V) " <- " (symbol->string V) " - 1 "))
           (skip (V)  (string-append (symbol->string V) " <- " (symbol->string V) " "))
           (if-goto (V L) (string-append "IF "(symbol->string V) " =/= " "0 GOTO "(symbol->string L) " "))
            ;;PART 3
           (goto-L (L)  (string-append "GOTO "(symbol->string L) " "))
           (zero-V (L)  (string-append (symbol->string L) " <- 0 "))
           (assign (V1 V2) (string-append (symbol->string V1) " <- " (symbol->string V2) " "))
           (else "Invalid Statement")))) 

              
;;QUESTION 2 - PART B: Parse - Returns the Abstract Syntax Tree of LANG

(define parse
  (lambda (p)
    (a-program (parse-instructions (parseString p)))))
;; Parses a string into a list.

(define parseString 
  (lambda (s)
    (if (not(list? s)) (parseString(string-split s))
    (cond [(null? s) '()]
          [(equal? (string-ref (car s) 0) #\() (cons(list (string->symbol (substring (car s) 1 (- (string-length (car s)) 1)))) (parseString(cdr s)))];; we have a string label
          [(char-numeric? (car(string->list  (car s)))) (cons (string->number (car s)) (parseString(cdr s)))]
          [else(cons (string->symbol (car s))(parseString(cdr s)))]))))
 
(define parse-instructions
  (lambda (i)
    (cond
      [(not (list? i)) (eopl:error "parse-instructions takes a list")]
      [(null? i) '()]
      [(list? (car i))
          (let ((label (caar i)) 
               (parse-states (parse-statement (cdr i))))
       
          ; Creates a datatype instruction labeled with symbol label and a statement.
          (if (and (statement? (car parse-states)) (symbol? label))
               (cons (labeled label (car parse-states))
                     (parse-instructions (cdr parse-states)))
               (eopl:error "error in concrete syntax")))]      
      ; No labels
      [else (cons (unlabeled (car (parse-statement i))) 
                    (parse-instructions (cdr (parse-statement i))))])))

(define parse-statement
  (lambda (s)
     (list? s)
        (cond
          [(if-goto? s) (cons (if-goto (cadr s) (caddr (cdddr s))) (cdddr (cdddr s)))]
          [(add1? s) (cons (add1 (car s)) (cdddr (cddr s)))]
          [(sub1? s) (cons (sub1 (car s)) (cdddr (cddr s)))]
          [(skip? s) (cons (skip (car s)) (cdddr s))]
;;QUESTION 3 - Added new constructors for the goto, zero and assignment.
          [(zero-V? s) (cons (zero-V (car s)) (cdddr s))];3
          [(assign? s) (cons (assign (car s) (caddr s)) (cdddr s))];3
          [(goto-L? s) (cons (goto-L (cadr s)) (cddr s))];2
          [eopl:error "invalid statement"])))

(define add1?
  (lambda (s)
    (if (>= (length s) 5)
        (let ((arg1 (car s))
              (arg2 (cadr s))
              (arg3 (caddr s))
              (arg4 (cadddr s))
              (arg5 (cadddr (cdr s))))
          (and (symbol? arg1)
               (symbol? arg3)
               (eqv? arg1 arg3)
               (eqv? arg2 '<-)
               (eqv? arg4 '+)
               (eqv? arg5 1)))
        #f)))

(define sub1?
  (lambda (s)
    (if (>= (length s) 5)
        (let ((arg1 (car s))
              (arg2 (cadr s))
              (arg3 (caddr s))
              (arg4 (cadddr s))
              (arg5 (cadddr (cdr s))))
          (and (symbol? arg1)
               (symbol? arg3)
               (eqv? arg1 arg3)
               (eqv? arg2 '<-)
               (eqv? arg4 '-)
               (eqv? arg5 1)))
        #f)))

(define if-goto?
  (lambda (s)
    (if (>= (length s) 6)
        (let ((arg1 (car s))
              (arg2 (cadr s))
              (arg3 (caddr s))
              (arg4 (cadddr s))
              (arg5 (cadddr (cdr s)))
              (arg6 (cadddr (cddr s))))
          (and (symbol? arg2)
               (symbol? arg6)
               (eqv? arg1 'IF)
               (eqv? arg3 '=/=)
               (eqv? arg4 '0)
               (eqv? arg5 'GOTO)))
        #f)))

(define skip?
  (lambda (s)
    (if (>= (length s) 3)
        (let ((arg1 (car s))
              (arg2 (cadr s))
              (arg3 (caddr s)))
          (and (symbol? arg1)
               (symbol? arg3)
               (eqv? arg1 arg3)
               (eqv? arg2 '<-)))
        #f)))
               
;;Question 3 - Implemenetion
(define zero-V?
  (lambda (s)
    (if (>= (length s) 3)
        (let ((arg1 (car s))
              (arg2 (cadr s))
              (arg3 (caddr s)))
          (and (symbol? arg1)
               (eqv? arg2 '<-)
               (eqv? arg3 0)))
        #f)))

(define assign?
  (lambda (s)
    (if (>= (length s) 3)
        (let ((arg1 (car s))
              (arg2 (cadr s))
              (arg3 (caddr s)))
          (and (symbol? arg1)
               (symbol? arg3)
               (eqv? arg2 '<-)))
        #f)))

(define goto-L?
  (lambda (s)
    (if (>= (length s) 2)
        (let ((arg1 (car s))
              (arg2 (cadr s)))
          (and (symbol? arg2)
               (eqv? arg1 'GOTO)))
        #f)))

;; Question 4 - max-index Returns the largest index used in a program.

(define max-index
  (lambda (prog)
    (cases program prog
      (a-program (p) (max-index-instructions p)))))

(define max-index-instructions
  (lambda (list)
    (cond
      ((null? list) 0)
      (else (max (max-index-instruction (car list))
                 (max-index-instructions (cdr list)))))))

(define max-index-instruction
  (lambda (instr)
    (cases instruction instr
      (labeled (label inst) (max (get-index label) (max-index-statement inst)))
      (unlabeled (inst) (max-index-statement inst)))))

(define max-index-statement
  (lambda (state)
    (cases statement state
      (add1 (arg1) (get-index arg1))
      (sub1 (arg1) (get-index arg1))
      (skip (arg1) (get-index arg1))
      (zero-V (arg1) (get-index  arg1))
      (if-goto (arg1 lab) (max (get-index arg1) (get-index lab)))
      (goto-L (lab) (get-index lab))
      (assign (arg1 arg2) (max (get-index arg1) (get-index arg2))))))

;; Helper Functions
(define get-index
  (lambda (sym)
    (or (string->number (substring (symbol->string sym) 1)) 1)))

(define MAX 0)

(define constr-var
  (lambda (L n)
    (string->symbol (string-append L (number->string n)))))

(define get-fresh-symbol
  (lambda ()
    (begin (set! MAX(+ 1 MAX))
           (constr-var "Z" MAX))))

(define get-fresh-label
  (lambda ()
    (begin (set! MAX (+ 1 MAX))
           (constr-var "A" MAX))))

(define label-first
  (lambda (lab ilist)
   ;; (display lab) DEBUGGING 
    (cases instruction (car ilist)
      (unlabeled (in) (cons (labeled lab in)(cdr ilist)))
      (else (eopl:error "Error: expected list of unlabeled")))))

;; QUESTION 5 - Expand-GOTOS: replaces all instances of GOTO L in a program by their expansions.

(define expand-GOTOS-program
  (lambda (ilist)
    (cond
      [(null? ilist) '()]
      [else (append (expand-GOTOS-instruction (car ilist)) (expand-GOTOS-program (cdr ilist)))])))

(define expand-GOTOS
  (lambda (prog)
    (begin (set! MAX (max-index prog))
           (cases program prog
             (a-program (P) (a-program (expand-GOTOS-program P)))))))


(define expand-GOTOS-instruction
  (lambda (instr)
    (cases instruction instr
      (labeled (lab in) (label-first lab (expand-GOTOS-statement in)))
      (unlabeled (in) (expand-GOTOS-statement in)))))

(define expand-GOTOS-statement
  (lambda (state)
    (cases statement state
      (goto-L (lab) (let ((fresh (get-fresh-symbol)))
                    (list (unlabeled (add1 fresh))
                          (unlabeled (if-goto fresh lab))) ))
      (else (cons (unlabeled state) '())))))

;;QUESTION 6 - ExpandV<-0: replaces all instances of V <- 0 in a program by their expansions.
;;NOTE SAME AS QUESTION 5

(define expandV<-0
  (lambda (prog)
    (begin (set! MAX (max-index prog)) ;; Gets the max index and set it to function MAX
           (cases program prog
             (a-program (p) (a-program (expandV<-0-program p)))))))

(define expandV<-0-program
  (lambda (list)
    (cond
      ((null? list) '())
      (else (append (expandV<-0-instruction (car list))
                    (expandV<-0-program (cdr list)))))))

(define expandV<-0-instruction
  (lambda (instr)
    (cases instruction instr
      (labeled (lab in) (label-first lab (expandV<-0-statement in)))
      (unlabeled (in) (expandV<-0-statement in)))))

(define expandV<-0-statement
  (lambda (state)
    (cases statement state
      (zero-V (V) (let ((fresh (get-fresh-label)))
                    (list (labeled fresh (sub1 V))
                          (unlabeled (if-goto V fresh)))))
      (else (cons (unlabeled state) '())))))

;;QUESTION 7 - ExpandV<-V': replaces all instances of V <- V' in a program by their expansions.
;;NOTE SAME AS QUESTION 5 and 6

(define expandV<-V 
  (lambda (prog)
    (begin (set! MAX (max-index prog))
           (cases program prog
             (a-program (p) (a-program (expandV<-V-program p)))))))

(define expandV<-V-program
  (lambda (list)
    (cond
      ((null? list) '())
      (else (append (expandV<-V-instruction (car list))
                    (expandV<-V-program (cdr list)))))))

(define expandV<-V-instruction
  (lambda (instr)
    (cases instruction instr
      (labeled (lab in) (label-first lab (expandV<-V-statement in)))
      (unlabeled (in) (expandV<-V-statement in)))))

(define expandV<-V-statement
  (lambda (state)
    (cases statement state
      (assign (V1 V2) (let ((L1 (get-fresh-label))   ;; Gets fresh-label assigns it to L1
                            (L2 (get-fresh-label))   ;; Gets fresh-label assigns it to L2
                            (L3 (get-fresh-label))   ;; Gets fresh-label assigns it to L3
                            (V  (get-fresh-symbol))) ;; Gets fresh-label symbol it to V
                        ;; Template for Assignment ;;
                        (append (expandV<-0-statement (zero-V V1))
                                (list (unlabeled (if-goto V2 L1))
                                      (unlabeled (goto-L L3))        
                                      (labeled L1 (sub1 V2))           
                                      (unlabeled (add1 V1))           
                                      (unlabeled (add1 V))            
                                      (unlabeled (if-goto V2 L1))    
                                      (labeled L2 (sub1 V))           
                                      (unlabeled (add1 V2))            
                                      (unlabeled (if-goto V L2))       
                                      (labeled L3 (skip V))))))        
      (else (cons (unlabeled state) '()))))) ;; If not assign statement returns empty list.

;;QUESTION 8 - clean-up: removes all shorthands of the program p and produces a program that behaves like p but does not contain any shorthands.

(define clean-up
  (lambda (p)
    (cond
      [(inState? p assign) (clean-up (expandV<-V p))]
      [(inState? p zero-V) (clean-up (expandV<-0 p) )]
      [(inState? p goto-L) (clean-up (expand-GOTOS p) )]
      [else p])))

; searches program for statement
(define inState?
  (lambda (prog statement)
    (cases program prog
      (a-program (list) (inState?-program list statement)))))

(define inState?-program
  (lambda (list state)
    (cond
      ((null? list) #f)
      ((inState?-instruct (car list) state) #t)
      (else (inState?-program (cdr list) state)))))

(define inState?-instruct
  (lambda (instr state)
    (cases instruction instr
      (labeled (lab in) (inState?-statement in state))
      (unlabeled (in) (inState?-statement in state)))))

(define inState?-statement
  (lambda (state target)
    (cases statement state
      (assign (V1 V2) (if (eq? target assign) #t #f))
      (zero-V (V) (if (eq? target zero-V) #t #f))
      (goto-L (L) (if (eq? target goto-L) #t #f))
      (else #f))))

;;QUESTION 9 - eval-program: given a program p and a snapshot s outputs the value of Y in the final state, if the program terminates, and loops indenitely otherwise.

(define eval-program
  (lambda (prog snap)
    (cases snapshot snap
      (a-snapshot (pc s) (cases program prog
                           (a-program (instructs) (if (null? instructs) (display "Y = 0")
                                      (eval-program-results (car (reverse (succ pc s instructs)))))))))))
   
(define eval-program-results
  (lambda (snap)
    (cases snapshot snap
      (a-snapshot (i s) (display (string-append "Y = " (number->string (valueof 'Y s))))))))

(define valueof
  (lambda (symb s)
    (cases state s
      (empty-state () 0)
      (extend-state (V n s) (if (eq? V symb) n (valueof symb s))))))

(define succ
  (lambda (i env instructs)
    (cond
      [(> i (length instructs)) '()]
      [else (cons (a-snapshot i env) 
                  (cases statement (lookup-instruct instructs i)
                    (add1 (V) (succ (+ i 1) (extend-state V (+ (valueof V env) 1) env) instructs))
                    (sub1 (V) (succ (+ i 1) (extend-state V (max (- (valueof V env) 1) 0) env) instructs))
                    (skip (V) (succ (+ i 1) env instructs))
                    (if-goto (V l) (if (eq? 0 (valueof V env))
                                       (succ (+ i 1) env instructs)
                                       (succ (lookup-label instructs l) env instructs)))
                    (else (eopl:error "Please call clean-up first before evaluating"))))])))

(define lookup-instruct
  (lambda (body n)
    (cond
      [(null? body) (eopl:error "invalid index")]
      [(= n 1) (cases instruction (car body)
                 (labeled (l i) i)
                 (unlabeled (i) i))]
      [else (lookup-instruct (cdr body) (- n 1))])))

; returns pc number of a given label
(define lookup-label
  (lambda (body lab)
    (cond
      ((null? body) 1)
      (else (cases instruction (car body)
              (labeled (l i) (if (eq? l lab) 1 (+ 1 (lookup-label (cdr body) lab))))
              (unlabeled (+ 1 (lookup-label (cdr body) lab))))))))


;--------------------------------------------------Test Cases ------------------------------------------------------;
(define myinstrs
           (list (labeled 'A (add1 'X))
                 (unlabeled (if-goto 'Z 'A))
                 (labeled 'C (skip 'Z2))
                 (unlabeled (sub1 'Y))
		   (unlabeled (goto-L 'D))
		   (labeled 'D (zero-V 'X))
		   ;;(unlabeled (assign ('C2 'Z2)))
  ))

(define test1
  (list (labeled 'A (add1 'X))
        (unlabeled (if-goto 'Z 'A))
        (labeled 'C (skip 'Z2))
        (unlabeled (sub1 'Y))
        (labeled 'A3 (add1 'Z4))
        (unlabeled (if-goto 'Z8 'A3))
        (unlabeled (goto-L 'A4))
        (unlabeled (add1 'z5))
        (labeled 'G20 (zero-V 'C1))
        (labeled 'E2 (skip 'S3))
                  ))
(define test2
  (list(labeled 'R (goto-L 'P))
        (labeled 'C (skip 'Z))
        (labeled 'T (zero-V 'Q))
        (unlabeled (zero-V 'W))
        (unlabeled (goto-L 'S))
        (unlabeled (assign 'SW 'L))))

; For testing
(define myprog (a-program test1))
(define myprog2 (a-program test2)) 

(define add2-body
(list (unlabeled (add1 'Y))
(unlabeled (add1 'Y))
(unlabeled (if-goto 'Y 'B))
(labeled 'A (add1 'Y))
(unlabeled (sub1 'X))
(labeled 'B (if-goto 'X 'A))))

(define mystate1 (extend-state 'X 3 (extend-state 'Y 0 (empty-state))))
(define mysnp1 (a-snapshot 1 mystate1))
  
(define add2-prog
(a-program add2-body))
