#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           FINAL_VERSION          ;
;                                  ;
; Neal Trischitta                  ;
; Programming Languages - CS 496   ; 
; May 08, 2013                     ; 
; Final Project                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;--------------------------------------TEST CASES--------------------------------------;          
; For testing
(define myinstrs
  (list (labeled 'A (add1 'X))
        (unlabeled (if-goto 'Z 'A))
        (labeled 'C (skip 'Z2))
        (unlabeled (sub1 'Y))))
; For testing
(define myprog (a-program myinstrs))

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
        (unlabeled (assign 'L 'L))))

; For testing
(define myprog (a-program test1))
(define myprog2 (a-program test2))               
  
;;;;;;;;;;;;;;;;Pretty Print Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pretty-print myprog)
;Should output:"(A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y - 1 "
 
(pretty-print (a-program '()))
;Should output: ""
 
(pretty-print '())
;Should output: ""


;;;;;;;;;;;;;;;;;;;;;Parsing Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parse "(A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y - 1 ")
; #(struct:a-program (#(struct:labeled A #(struct:add1 X)) #(struct:unlabeled #(struct:if-goto Z A)) #(struct:labeled C #(struct:skip Z2)) #(struct:unlabeled #(struct:sub1 Y))))
 
(parse "")
; #(struct:a-program ()

(parse "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F) IF Z =/= 0 GOTO A Y <- Y - 1")
;#(struct:a-program (#(struct:labeled A #(struct:add1 X)) #(struct:unlabeled #(struct:skip M3) #(struct:labeled D #(struct:if-goto Z A)) #(struct:labeled F #(struct:if-goto Z A) #(struct:unlabeled #(struct:sub1 Y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Additional Instruction;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(parseString "(A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y + 1 ")
;Should output: ((A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y + 1)

(parseString "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F)")
;Should output: ((A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F))


;;;;;;;;;;;;;;;;;;;;;;;;Additional Function Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pretty-print(parse "
(A3) Z4 <- Z4 + 1
IF Z6 =/= 0 GOTO A3
GOTO A4
C1 <- 0
Z5 <- Z5 + 1
(A4) Z3 <- Z3 - 1"))
;;Should output: "(A3) Z4 <- Z4 + 1 IF Z6 =/= 0 GOTO A3 GOTO A4 C1 <- 0 Z5 <- Z5 + 1 (A4) Z3 <- Z3 - 1 "

(pretty-print myprog)
"(A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y - 1 (A3) Z4 <- Z4 + 1 IF Z8 =/= 0 GOTO A3 GOTO A4 z5 <- z5 + 1 (G20) C1 <- 0 (E2) S3 <- S3 "

(parse (pretty-print myprog))
;;Should output:
;;#(struct:a-program
;;  (#(struct:labeled A #(struct:add1 X))
;; #(struct:unlabeled #(struct:if-goto Z A))
;; #(struct:labeled C #(struct:skip Z2))
;; #(struct:unlabeled #(struct:sub1 Y))
;; #(struct:labeled A3 #(struct:add1 Z4))
;; #(struct:unlabeled #(struct:if-goto Z8 A3))
;; #(struct:unlabeled #(struct:goto-L A4))
;; #(struct:unlabeled #(struct:add1 z5))
;; #(struct:labeled G20 #(struct:zero-V C1))
;; #(struct:labeled E2 #(struct:skip S3))))

(pretty-print myprog2)
;;Should output:"(R) GOTO P (C) Z <- Z (T) Q <- 0 W <- 0 GOTO S L <- L "

(parse (pretty-print myprog2))
;;Should output:
;;#(struct:a-program
;;  (#(struct:labeled R #(struct:goto-L P))
;;   #(struct:labeled C #(struct:skip Z))
;;   #(struct:labeled T #(struct:zero-V Q))
;;   #(struct:unlabeled #(struct:zero-V W))
;;   #(struct:unlabeled #(struct:goto-L S))
;;   #(struct:unlabeled #(struct:skip L))))



;;;;;;;;;;;;;;;;;;;;;;;;Max-Index Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(max-index(parse "
(A3) Z4 <- Z4 + 1
IF Z6 =/= 0 GOTO A3
GOTO A4
C1 <- 0
Z5 <- Z5 + 1
(A4) Z3 <- Z3 - 1"))
;Should output: 6

(max-index(parse ""))
;Should output: 0

(max-index myprog)
;Should output: 20

(max-index myprog2)
;Should output 1

(max-index(parse "
(A3) Z4 <- Z4 + 1
IF Z1 =/= 0 GOTO A3
GOTO A40
Z5 <- Z5 + 1
(A40) Z3 <- Z3 - 1"))
;Should output 40

;;;;;;;;;;;;;;;;;;;;;;;;Expand-GOTOS Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pretty-print(expand-GOTOS
(parse "GOTO A3 ")))
;Should output "Z4 <- Z4 + 1 IF Z4 =/= 0 GOTO A3 "

(pretty-print(expand-GOTOS
(parse "
(A3) Z4 <- Z4 + 1
IF Z6 =/= 0 GOTO A3
GOTO A4
Z5 <- Z5 + 1
(A4) Z3 <- Z3 - 1 ")))
;Should output "(A3) Z4 <- Z4 + 1 IF Z6 =/= 0 GOTO A3 Z7 <- Z7 + 1 IF Z7 =/= 0 GOTO A4 Z5 <- Z5 + 1 (A4) Z3 <- Z3 - 1 "

(pretty-print(expand-GOTOS
(parse "")))
;Should output ""

(pretty-print(expand-GOTOS ;; No GOTOs
(parse "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F) IF Z =/= 0 GOTO A Y <- Y - 1")))
;Should output "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F) IF Z =/= 0 GOTO A Y <- Y - 1 "

(pretty-print(expand-GOTOS myprog))
;Should output "(A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y - 1 (A3) Z4 <- Z4 + 1 IF Z8 =/= 0 GOTO A3 Z21 <- Z21 + 1 IF Z21 =/= 0 GOTO A4 z5 <- z5 + 1 (G20) C1 <- 0 (E2) S3 <- S3 "

(pretty-print(expand-GOTOS myprog2))
;Should output "(R) Z2 <- Z2 + 1 IF Z2 =/= 0 GOTO P (C) Z <- Z (T) Q <- 0 W <- 0 Z3 <- Z3 + 1 IF Z3 =/= 0 GOTO S SW <- L "


;;;;;;;;;;;;;;;;;;;;;;;;Expand-V<-0 Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pretty-print(expandV<-0
(parse "Z <- 0")))
;Should output "Z <- Z (A2) Z <- Z - 1 IF Z =/= 0 GOTO A2 "

(pretty-print(expandV<-0
(parse "")))
;Should output ""

(pretty-print(expand-V<-0 ;; No V<-0
(parse "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F) IF Z =/= 0 GOTO A Y <- Y - 1")))
;Should output "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F) IF Z =/= 0 GOTO A Y <- Y - 1 "

(pretty-print(expandV<-0 myprog))
; Should ouput "(A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y - 1 (A3) Z4 <- Z4 + 1 IF Z8 =/= 0 GOTO A3 GOTO A4 z5 <- z5 + 1 (G20) C1 <- C1 (A21) C1 <- C1 - 1 IF C1 =/= 0 GOTO A21 (E2) S3 <- S3 "

(pretty-print(expandV<-0 myprog2))
;Should output "(R) GOTO P (C) Z <- Z (T) Q <- Q (A2) Q <- Q - 1 IF Q =/= 0 GOTO A2 W <- W (A3) W <- W - 1 IF W =/= 0 GOTO A3 GOTO S SW <- L "

;;;;;;;;;;;;;;;;;;;;;;;;Expand-V<-V' Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pretty-print(expandV<-V
(parse "Z <- C")))
;Should output "Z <- Z (A6) Z <- Z - 1 IF Z =/= 0 GOTO A6 IF C =/= 0 GOTO A2 GOTO A4 (A2) C <- C - 1 Z <- Z + 1 Z5 <- Z5 + 1 IF C =/= 0 GOTO A2 (A3) Z5 <- Z5 - 1 C <- C + 1 IF Z5 =/= 0 GOTO A3 (A4) Z5 <- Z5 "

(pretty-print(expandV<-0
(parse "")))
;Should output ""

(pretty-print(expandV<-V ;; No V <- V'
(parse "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F) IF Z =/= 0 GOTO A Y <- Y - 1")))
;Should output "(A) X <- X + 1 M3 <- M3 (D) IF Z =/= 0 GOTO A (F) IF Z =/= 0 GOTO A Y <- Y - 1 "

(pretty-print(expandV<-V myprog))
;Should output "(A) X <- X + 1 IF Z =/= 0 GOTO A (C) Z2 <- Z2 Y <- Y - 1 (A3) Z4 <- Z4 + 1 IF Z8 =/= 0 GOTO A3 GOTO A4 z5 <- z5 + 1 (G20) C1 <- 0 (E2) S3 <- S3 "

(pretty-print(expandV<-V myprog2))
;Should output "(R) GOTO P (C) Z <- Z (T) Q <- 0 W <- 0 GOTO S SW <- SW (A6) SW <- SW - 1 IF SW =/= 0 GOTO A6 IF L =/= 0 GOTO A2 GOTO A4 (A2) L <- L - 1 SW <- SW + 1 Z5 <- Z5 + 1 IF L =/= 0 GOTO A2 (A3) Z5 <- Z5 - 1 L <- L + 1 IF Z5 =/= 0 GOTO A3 (A4) Z5 <- Z5 "

;;;;;;;;;;;;;;;;;;;;;;;;clean-up Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pretty-print (clean-up (parse "")))
;Should output ""

(pretty-print (clean-up (parse "A <- A + 1 V <- V - 1")) )
;Should output "A <- A + 1 V <- V - 1 "

(pretty-print (clean-up (parse "GOTO S2 X <- 0 Y <- A A <- A + 1")))
;Should output "Z9 <- Z9 + 1 IF Z9 =/= 0 GOTO S2 (A8) X <- X - 1 IF X =/= 0 GOTO A8 (A7) Y <- Y - 1 IF Y =/= 0 GOTO A7 IF A =/= 0 GOTO A3 Z10 <- Z10 + 1 IF Z10 =/= 0 GOTO A5 (A3) A <- A - 1 Y <- Y + 1 Z6 <- Z6 + 1 IF A =/= 0 GOTO A3 (A4) Z6 <- Z6 - 1 A <- A + 1 IF Z6 =/= 0 GOTO A4 (A5) Z6 <- Z6 A <- A + 1 "

(eq? (clean-up(clean-up add2-prog)) (clean-up add2-prog))
;Should output #t

;;;;;;;;;;;;;;;;;;;;;;;;clean-up Test Cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-program (parse "") mysnp1)
;Should output Y = 0

(eval-program add2-prog mysnp1)
;Should output Y = 5

(eval-program (clean-up (parse "X <- X + 1 Y <- X")) mysnp1)
;Should output Y = 4

(eval-program (clean-up (parse "Y <- X")) mysnp1)
;Should output Y = 3


