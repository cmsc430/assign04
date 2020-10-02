#lang racket
(provide (all-defined-out))
(require (only-in "syntax.rkt" prim?))

(require "ast.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character

;; type Answer = Value | 'err

;; type REnv = (Listof (List Variable Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr REnv -> Answer
(define (interp-env e r)
  (match e
    [(int-e i) i]
    [(bool-e b) b]
    [(char-e c) c]
    [(prim-e p e)
     (let ((a (interp-env e r)))
       (interp-prim p a))]
    [(if-e i t f)
     (match (interp-env i r)
       ['err 'err]
       [v
        (if v
            (interp-env t r)
            (interp-env f r))])]    
    [(var-e v)
     (lookup r v)]
    [(let-e (list (binding xs es) ...) b)
      (match (interp-envs es r)
        ['err 'err]
        [vs
          (interp-env b (append (zip xs vs) r))])]
    [(cond-e cs f)
      (interp-cond-env cs f r)]))

;; (Listof Expr) REnv -> (Listof Value) | 'err
(define (interp-envs es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (cons v (interp-envs es r))])]))

;; (Listof (clause i b)) Expr REnv -> Answer
(define (interp-cond-env cs f r)
  (match cs
    ['() (interp-env f r)]
    [(cons (clause i b) css)
     (match (interp-env i r)
       ['err 'err]
       [v
        (if v
            (interp-env b r)
            (interp-cond-env css f r))])]))

;; Prim Answer -> Answer
(define (interp-prim p a)
  (match (list p a)
    [(list _  'err) 'err]
    [(list '- (? integer? i0)) (- i0)]    
    [(list 'abs (? integer? i0)) (abs i0)]
    [(list 'add1 (? integer? i0)) (+ i0 1)]
    [(list 'sub1 (? integer? i0)) (- i0 1)]
    [(list 'zero? (? integer? i0)) (zero? i0)]
    [(list 'char? v0) (char? v0)]
    [(list 'integer? v0) (integer? v0)]
    [(list 'boolean? v0) (boolean? v0)]
    [(list 'integer->char (? codepoint? i0)) (integer->char i0)]
    [(list 'char->integer (? char? c)) (char->integer c)]
    [_ 'err]))

;; REnv Variable -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y v) env)
     (match (symbol=? x y)
       [#t v]
       [#f (lookup env x)])]))

;; REnv Variable Value -> Value
(define (ext r x v)
  (cons (list x v) r))

;; Any -> Boolean
(define (codepoint? x)
  (and (integer? x)
       (<= 0 x #x10FFFF)
       (not (<= #xD800 x #xDFFF))))

;; (Listof A) (Listof B) -> (Listof (List A B))
(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y) (zip xs ys))]))
