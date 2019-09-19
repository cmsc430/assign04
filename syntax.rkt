#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(,(? prim?) ,x) (expr? x)]
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
    ;; TODO
    ;; ...
    [_ #f]))

;; Expr -> Boolean
;; Is e a closed expression?
(define (closed? e)
  ;; TODO
  #f)



;; Any -> Boolean
;; Is x a primitive?
(define (prim? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      char? integer? boolean? zero?))))




