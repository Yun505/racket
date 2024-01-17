#lang racket
;these two functions are exactly the same  
(define (f x) [+ (* x x) 21])
(define f (lambda (x) [+ (* x x) 21]))
;ctrl backslash gives you lambda or just put lambda in scheme
((lambda (x) (+ x x)) 3) ; this gives 6, can only be done with lambda

(define makerepeat (λ (f) (λ (x) (f x x))))

(define double (makerepeat +))
;double is now λ -> (+ x x)

(define f (λ (f) (λ x (f (f x)))))
(define func(λ (n) (λ (* n 2))*))

(define twice_variable (λ (x) (λ (f) (f (f x)))))

;(map number)


