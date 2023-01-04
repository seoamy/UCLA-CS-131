#lang racket
(provide expr-compare)

(define LAMBDA (string->symbol "\u03BB"))

; check if x is lambda
(define (lambda? x)
  (or (equal? x LAMBDA) (equal? x 'lambda))
)

; check if x is a quote
(define (quote? x)
  (equal? (car x) 'quote)
)

; check if x or y is an if statement
(define (if? x y)
  (or (equal? (car x) 'if) (equal? (car y) 'if))
)

; check if x and y are the same length
(define (eqlen? x y) 
  (equal? (length x) (length y))
)

(define (create-if x y) 
  ; if both x and y start with an if, cons if with the comparison done on the cdr of both
  ; otherwise, create a single term
  (if (and (equal? (car x) 'if) (equal? (car y) 'if))
    (cons 'if (expr-compare (cdr x) (cdr y)))
    (create-term x y)
  )
)

; create a single term by rerunning expr-compare or 
; creating the term itself
(define (create-single x y)
  (if (and (list? x) (list? y))
    (expr-compare x y)
    (create-term x y)
  )
)

; does simple comparison between x and y to generate a single term
(define (create-term x y)
  (cond 
    [(equal? x y) x]
    [(and (equal? x #t) (not y) '%)]
    [(and (equal? y #t) (not x) (list 'not '%))]
    [(list 'if '% x y)]
  )
)

(define (create-expr x y)
  (if (eqlen? x y) 
    (cons (create-single (car x) (car y)) (expr-compare (cdr x) (cdr y)))
    (create-term x y)
  )
)

(define (create-bind x y)
  (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)

; creates a hash for x and y
(define (create-hash x y h)
  (if (or (null? x) (null? y)) h
    (if (equal? (car x) (car y))
      (create-hash (cdr x) (cdr y) h)
      (let ([new-h (hash-set h (car x) (create-bind (car x) (car y)))])
        (let ([new-h2 (hash-set new-h (car y) (create-bind (car x) (car y)))])
          (create-hash (cdr x) (cdr y) new-h2)
        )
      )
    )
  )
)

; binds result based on hash
(define (bind-result x h a)
  (if (list? x)
    (if (null? x) a
      (if (equal? (hash-ref h (car x) "hash not found") "hash not found")
        (if (null? a) ; if the hash h value is found, and a is null, then bind first element of x
          (bind-result (cdr x) h (list (car x)))
          (bind-result (cdr x) h (append a (list (car x)))) ; otherwise bind with the accumulator a
        )
        (if (null? a)
          (bind-result (cdr x) h (cons (hash-ref h (car x) "hash not found") a)) ; if the hash h value is NOT found and a is null, cons h with a
          (bind-result (cdr x) h (append a (list (hash-ref h (car x) "hash not found")))) ; otherwise append the hash value with the accumulator
        )
      )
    )
    (if (equal? (hash-ref h x "hash not found") "hash not found") x
      (hash-ref h x "hash not found")
    )
  )
)

(define (create-lambda-body x y h)
  (let ([bound-x (bind-result x h empty)] [bound-y (bind-result y h empty)])
    (expr-compare bound-x bound-y)
  )
)

(define (lambda-helper x y)
  (let ([empty-hash (hash-set (hash) "null" 0)])
    (let ([h (create-hash (cadr x) (cadr y) empty-hash)])
      (list (car x) (bind-result (cadr x) h empty) (create-lambda-body (caddr x) (caddr y) h))
    )
  )
)

(define (create-lambda x y)
  (if (or (< (length x) 2) (< (length y) 2)) (create-expr x y)
    (if (not (equal? (length (cadr x)) (length (cadr y)))) (create-term x y)
      (if (not (equal? (length x) 1))
        (if (and (equal? 'lambda (car x)) (equal? 'lambda (car y)))
          (lambda-helper x y)
          (let ([new-x (append (list 'λ) (cdr x))] [new-y (append (list 'λ) (cdr y))])
            (lambda-helper new-x new-y))
        )
        (list (create-term (car x) (car y)))
      )
    )
  )
)

(define (expr-compare x y)
  (cond [(equal? x y) x]
    [(or (null? x) (null? y)) empty]
    [(and (equal? x #t) (not y) '%)]
    [(and (equal? y #t) (not x) (list 'not '%))]
    [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
    ; if one of them is not list - which means that not function
    [(or (not (list? x)) (not (list? y))) (list 'if '% x y)]
    [(if? x y) (create-if x y)]
    [(quote? x) (list 'if '% x y)]
    [(and (lambda? (car x)) (lambda? (car y))) (create-lambda x y)] ; both are lambda
    [(or (lambda? (car x)) (lambda? (car y))) (list 'if '% x y)] ; only one of them is lambda
    [(create-expr x y)]
    [(create-term x y)]
  )
)

; from TA helper code
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval '(let ((% #t)) (expr-compare x y))))
       (equal? (eval y)
               (eval '(let ((% #f)) (expr-compare x y)))))
)

; ; WARNING: IT MUST BE A SINGLE TEST CASE
; ; You need to cover all grammars including:
; ;     constant literals, variables, procedure calls, quote, lambda, if
(define test-expr-x
  '(list
    12
    'a
    '(cons a b)
    '(cons a lambda)
    '(cons (cons a b) (cons b c))
    '(cons a b)
    '(list)
    ''(a b)
    '(quoth (a b))
    '(if x y z)
    '((lambda (a) (f a)) 1)
    '((lambda (a) (f a)) 1)
    '(+ #f ((λ (a b) (f a b)) 1 2))
    '((λ (if) (+ if 1)) 3)
   )
)

(define test-expr-y
  '(list
    20
    '(cons a b)
    '(cons a b)
    '(cons a λ)
    '(cons (cons a c) (cons a c))
    '(list a b)
    '(list a)
    ''(a c)
    '(quoth (a c))
    '(if x z z)
    '((lambda (a) (g a)) 2)
    '((λ (a) (g a)) 2)
    '(+ #t ((lambda (a c) (f a c)) 1 2))
    '((lambda (fi) (+ fi 1)) 3)
  )
)