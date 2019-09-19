#lang racket
(provide (all-defined-out))

;; Expr -> Asm
(define (compile e)
  `(entry
    ,@(compile-e e '())
    ret
    err
    (push rbp) ; push before calling
    (call error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(? integer? i)
     `((mov rax ,(* i 4)))]
    [(? boolean? b)
     `((mov rax ,(if b #b101 #b001)))]
    [`(add1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (add rax 4)))]
    [`(sub1 ,e0)
     (let ((c0 (compile-e e0 c)))
       `(,@c0
         ,@assert-integer
         (sub rax 4)))]
    [`(zero? ,e0)
     (let ((c0 (compile-e e0 c))
           (l0 (gensym))
           (l1 (gensym)))
       `(,@c0
         ,@assert-integer
         (cmp rax 0)
         (mov rax #b001) ; #f         
         (jne ,l0)
         (mov rax #b101) ; #t
         ,l0))]    
    [`(if ,e0 ,e1 ,e2)
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
    [(? symbol? x)
     (let ((i (lookup x c)))
       `((mov rax (offset rsp ,(- (add1 i))))))]

    ;; TODO: generalize
    [`(let ((,x ,e0)) ,e1)
     (let ((c0 (compile-e e0 c))
           (c1 (compile-e e1 (cons x c))))
       `(,@c0
         (mov (offset rsp ,(- (add1 (length c)))) rax)
         ,@c1))]

    
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



