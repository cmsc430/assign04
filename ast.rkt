#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The grammar of our AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type Expr =
;; | Integer
;; | Boolean
;; | Character
;; | Variable
;; | Prim PrimOp Expr
;; | if Expr Expr Expr
;; | cond (Clause list) Expr
;; | let (Binding list) Expr

;; type PrimOp =
;; | add1 | sub1 | zero? | abs | - | char? | integer? | boolean?
;; | integer->char | char->integer

;; type Clause = Expr Expr
;; type Binding = Variable Expr

;; type Variable = Symbol (except 'let, 'add1, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; The AST data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The AST can be viewed as having 'kinds' of nodes.
;;
;; * The nodes that represnt an expression themselves
;;
;; * The nodes that are part of an expression, but no an expression themselves

;; The below are the former:

(struct int-e  (i) #:transparent)
(struct bool-e (b) #:transparent)
(struct char-e (c) #:transparent)
(struct var-e  (v) #:transparent)
(struct prim-e (p e) #:transparent)
(struct if-e   (e t f) #:transparent)
(struct cond-e (cs el) #:transparent)
(struct let-e  (bs b) #:transparent)

;; The next two are the latter:

;; a clause now takes an _arbitrary_ expression and a body.
;; This is different than assignment 3! If you want to understand
;; why, look at the lecture notes for Dupe.
(struct clause  (e body) #:transparent)

;; A binding holds a symbol representing the bound variable and
;; Expr that represents the value that will be bound to that variable
(struct binding (v e) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AST utility functions (getters)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It will sometimes be useful to get the list of all the variables that are
;; introduced by a `let`
;; [Binding] -> [Symbol]
(define (get-vars bs)
  (match bs
    ['() '()]
    [(cons (binding v _) bs) (cons v (get-vars bs))]))

;; Get all of the _definitions_ from a list of bindings
;; [Binding] -> [Expr]
(define (get-defs bs)
  (match bs
    ['() '()]
    [(cons (binding _ def) bs) (cons def (get-defs bs))]))


;; Get all of the predicate expressions from a list of clauses
;; [Binding] -> [Expr]
(define (get-preds cs)
  (match cs
    ['() '()]
    [(cons (clause p _) cs) (cons p (get-preds cs))]))

;; Get all of the bodies expressions from a list of clauses
;; [Binding] -> [Expr]
(define (get-bods cs)
  (match cs
    ['() '()]
    [(cons (clause _ b) cs) (cons b (get-preds cs))]))

;; Given an AST, construct an sexpr that has the same shape
(define (ast->sexpr a)
  (match a
    [(int-e i)     `(int-e ,i)]
    [(bool-e b)    `(bool-e ,b)]
    [(char-e c)    `(char-e ,c)]
    [(var-e v)     `(var-e ,v)]
    [(prim-e p e)  `(prim-e ,p ,e)]
    [(if-e e t f)  `(if-e ,(ast->sexpr e)
                          ,(ast->sexpr t)
                          ,(ast->sexpr f))]
    [(cond-e cs f) `(cond-e ,(clauses->sexpr cs) ,(ast->sexpr f))]
    [(let-e bs b)  `(let-e ,(binding->sexpr bs) ,(ast->sexpr b))]))

(define (binding->sexpr bnds)
  (match bnds
    ['() '()]
    [(cons (binding v e) bnds) `((,v ,(ast->sexpr e)) ,@(binding->sexpr bnds))]))

(define (clauses->sexpr cs)
  (match cs
    ['() '()]
    [(cons (clause e b) cs) `((,(ast->sexpr e) ,(ast->sexpr b)) ,@(clauses->sexpr cs))]))
