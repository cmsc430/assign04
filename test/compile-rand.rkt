#lang racket
(require (only-in "../interp.rkt" interp)
         (only-in "../compile.rkt" compile)
         "../asm/interp.rkt" 
         "../random-exprs.rkt" 
         "../syntax.rkt" 
         rackunit)

(define (check-compiler e)
  (let ((e (sexpr->ast e)))
  (check-equal? (asm-interp (compile e))
                (interp e)
                e)))

(for ([e exprs])
  (check-compiler e))

