#lang racket
(provide parse)
(require "ast.rkt")

(define (well-formed? e)
  ;; TODO: Replace with your definition
  #t

  )




;; We move parsing to a helper function to allow for calling well-formed?
;; S-Expr -> Expr
(define (parse s)
  (let ((e (parse-aux s)))
    (if (well-formed? e)
        e
        (IllFormedError))))

(define (parse-aux s)
    (cond
      [(integer? s) (Int s)]
      [(boolean? s) (Bool s)]
      [(char? s)    (Char s)]
      [(symbol? s)  (match s
                      ['eof (Eof)]
                      [_ (Var s)])]
      [else
       (match s
         ['eof                    (Eof)]
         [(list 'read-byte)       (Prim0 'read-byte)]
         [(list 'peek-byte)       (Prim0 'peek-byte)]
         [(list 'void)            (Prim0 'void)]
         [(list 'add1 e)          (Prim1 'add1 (parse-aux e))]
         [(list 'sub1 e)          (Prim1 'sub1 (parse-aux e))]
         [(list 'zero? e)         (Prim1 'zero? (parse-aux e))]
         ;; Implemented for you: abs, -, not
         [(list 'abs e)  (Prim1 'abs (parse-aux e))]
         [(list '- e)    (Prim1 '- (parse-aux e))]
         [(list 'not e)  (Prim1 'not (parse-aux e))]
         [(list 'char? e)         (Prim1 'char? (parse-aux e))]
         ;; TODO: integer? / boolean?
         [(list 'write-byte e)    (Prim1 'write-byte (parse-aux e))]
         [(list 'eof-object? e)   (Prim1 'eof-object? (parse-aux e))]
         [(list 'integer->char e) (Prim1 'integer->char (parse-aux e))]
         [(list 'char->integer e) (Prim1 'char->integer (parse-aux e))]
         [(list 'begin e1 e2)     (Begin (parse-aux e1) (parse-aux e2))]
         [(list '+ e1 e2)         (Prim2 '+ (parse-aux e1) (parse-aux e2))]
         ;; TODO: Variadic +
         [(cons '+ es)            (PrimVar '+ (map parse-aux es))]
         [(list '- e1 e2)         (Prim2 '- (parse-aux e1) (parse-aux e2))]       
         [(list 'if e1 e2 e3)
          (If (parse-aux e1) (parse-aux e2) (parse-aux e3))]
         ;; TODO: Let and Let* with multiple bindings
         ;; Implemented for you: cond
         [(cons 'cond clauses)
          (let ((res (parse-cond clauses)))
            (Cond (car res) (cdr res)))]
         [_ (error "Parse error" s)])]
      ))


;; Implemented for you: cond
(define (parse-cond cs)
  (match cs
    [(list (list 'else e)) (cons '() (parse-aux e))]
    [(cons (list p e) css)
     (let ((res (parse-cond css)))
       (cons (cons (Clause (parse-aux p) (parse-aux e)) (car res)) (cdr res)))]
    ))
