#lang racket
(provide (all-defined-out))

(require "ast.rkt")

;; Expr -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(int-e i)  #t]
    [(bool-e b) #t]
    [(char-e c) #t]
    [(if-e p t f)
     (and (expr? p)
          (expr? t)
          (expr? f))]
    [(prim-e p e) (and
                    (prim? p)
                    (expr? e))]
    [(cond-e cs f)
       (and (andmap expr? (get-preds cs))
            (andmap expr? (get-bods cs))
            (expr? f))]
    ;; TODO
    ;; ...
    [_ #f]))

; SExpr -> AST
; convert the s-expr into our AST
; This should be a one-to-one mapping for now.
(define (sexpr->ast s)
  (match s
    [(? integer? s)         (int-e s)]
    [(? boolean? s)         (bool-e s)]
    [(? char? s)            (char-e s)]
    [(? symbol? s)          (var-e s)]
    [`(add1  ,e)            (prim-e 'add1 (sexpr->ast e))]
    [`(sub1  ,e)            (prim-e 'sub1 (sexpr->ast e))]
    [`(zero? ,e)            (prim-e 'zero? (sexpr->ast e))]
    [`(char? ,e)            (prim-e 'char? (sexpr->ast e))]
    [`(integer? ,e)         (prim-e 'integer? (sexpr->ast e))]
    [`(boolean? ,e)         (prim-e 'boolean? (sexpr->ast e))]
    [`(integer->char ,e)    (prim-e 'integer->char (sexpr->ast e))]
    [`(char->integer ,e)    (prim-e 'char->integer (sexpr->ast e))]
    [`(abs ,x)              (prim-e 'abs (sexpr->ast x))]
    [`(- ,x)                (prim-e '-   (sexpr->ast x))]
    [`(if ,p ,t ,f)         (if-e (sexpr->ast p) (sexpr->ast t) (sexpr->ast f))]
    [`(cond ,@cs)
      ; collect the clauses in a pair where the car
      ; is the list of clauses the cdr is the 'else'
      (let ((p (clauses->ast '() cs))) 
        (cond-e (car p) (cdr p)))]
    [`(let ,bs ,b)          (let-e (map binding->ast bs) (sexpr->ast b))]
    [o              (error "operation not supported: " o)]))

(define (clauses->ast acc cs)
  (match cs
    [`((else ,f))
      (cons (reverse acc) (sexpr->ast f))]
    [(cons `(,e ,b) rest)
      (let ((c (clause (sexpr->ast e) (sexpr->ast b))))
           (clauses->ast (cons c acc) rest))]))

(define (binding->ast bs)
  (match bs
    [`(,v ,e) (binding v (sexpr->ast e))]))

;; Expr -> Boolean
;; Is e a closed expression?
(define (closed? e)
  ;; TODO
  #f)



; SOLN
(module+ test
  (require rackunit)
  (require "random-exprs.rkt")
  (for ([e (in-list exprs)])
    (check-true (and (closed? (sexpr->ast e)) #t) e)))

;; Any -> Boolean
;; Is x a primitive?
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      char? integer? boolean? zero?))))
