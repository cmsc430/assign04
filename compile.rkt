#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; This assignment should be completed individually.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I pledge on my honor that I have not given or received any
;; unauthorized assistance on this assignment.
;;
;; Name: TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp) ; push before calling
    (call error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Compiling expressions and their sub-parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You'll need to add the ability to compile `char-e` nodes

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(int-e i)
     `((mov rax ,(* i 4)))]
    [(bool-e b)
     `((mov rax ,(if b #b101 #b001)))]
    [(prim-e p e0)
     (let ((c0 (compile-e e0 c)))
          `(,@c0
            ,@(compile-prim p)))]
    [(if-e e0 e1 e2)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 c))
           (c2 (compile-e e2 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         (cmp rax #b001) ; compare to #f
         (je ,l0)        ; jump to c2 if #f
         ,@c1
         (jmp ,l1)       ; jump past c2
         ,l0
         ,@c2
         ,l1))]

    ;; TODO: make sure this works with your generalized let
    [(var-e v)
     (let ((i (lookup v c)))
       `((mov rax (offset rsp ,(- (add1 i))))))]

    ;; TODO: generalize
    [(let-e (list (binding x def)) bod)
     (let ((c0 (compile-e def c))
           (c1 (compile-e bod (cons x c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c1))]

    ;; TODO
    #;...))
    
(define (compile-prim p)
  (match p
    ['add1 `(,@assert-integer
             (add rax 4))]
    ['sub1 `(,@assert-integer
             (sub rax 4))]
    ['zero?
     (let ((l0 (gensym))
           (l1 (gensym)))
       `(,@assert-integer
         (cmp rax 0)
         (mov rax #b001) ; #f
         (jne ,l0)
         (mov rax #b101) ; #t
         ,l0))]

    ;; Primitives still left: char?, integer?, boolean?,
    ;; char->integer, integer->char, neg, abs.
    ;; You should be able to harvest a significant amount from your submission
    ;; to assignment#3
    ;; TODO
    #;...))



;; Variable CEnv -> Natural
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y cenv)
     (match (symbol=? x y)
       [#t (length cenv)]
       [#f (lookup x cenv)])]))

;; Asm
(define assert-integer
  `((mov rbx rax)
    (and rbx #b11)
    (cmp rbx 0)
    (jne err)))



