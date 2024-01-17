#lang racket
#| Hayun Jung
Kevin Liu helped me
I helped Devon Lai|#

(define (func1 x) [+ (* x x) 2])
(define func2 [λ (x) [+ (* x x) 2]])
(define dec (λ (x) [- x 1]))

(define operate (λ (f) [λ (a b) (cons [f a] [f b])]))
(define times3 [operate (λ (x) [* 3 x])])

(define [evens list (newlist '())]
  [cond
    ((empty? list)
     [reverse newlist])
    [else
     [cond
     ((equal? [modulo (car list) 2] 0)
      (evens [cdr list] [cons #t newlist]))
     (else
      (evens [cdr list] [cons #f newlist]))]]])

(define [listTimes4 list (newlist '())]
  [cond
    ((empty? list)
     [reverse newlist])
    [else
     (listTimes4  (cdr list) [cons [* (car list) 4] newlist])]])

(define [rpn list (stack '())]
  [cond
    ((empty? list)
     [cond
       ((empty? stack)
        0)
       (else
        (car stack))])
    [else
     [cond
       ((number? (car list))
        [rpn (cdr list) [cons (car list) stack]])
       ([or [equal? (car list) *] [equal? (car list) -] [equal? (car list) /] [equal? (car list) +]]
        (define result [(car list) [car (cdr stack)] (car stack)])
        [rpn (cdr list) [cons result [cdr(remove (car stack) stack)]]])]]])


(define [evaluate list (operation '()) (operand '()) (shift #f)]
  [cond
    ;inside Parenthesis
    [shift
     [cond
       ((equal? (car operation) "(")
        (evaluate list (cdr operation) operand #f))
       (else
        (evaluate list (cdr operation) (append operand (cons (car operation) null)) shift))]]

    ;checking for empty list/operation stack
    [(empty? list)
     [cond
       [(empty? operation)
        operand]
       [else
        (evaluate list (cdr operation) (append operand (cons (car operation)null)) shift)]]]
    
    ;check if number or operand
    [else
     [cond
       [(number? (car list))
        (evaluate (cdr list) operation (append operand (cons (car list) null)) shift)]
       [(not shift)
        [cond
          ;check parenthesis
          [(equal? (car list) "(")
           (evaluate (cdr list) (cons (car list) operation) operand shift)]
          [(equal? (car list) ")")
           (evaluate (cdr list) operation operand #t)]

          ;null check
          [(not (empty? operation))
           [cond
             ((equal? (car operation) "(")
              (evaluate (cdr list) (cons (car list) operation) operand shift))
             
              ;precedence
             ([or (equal? (car list) *) (equal? (car list) /)]
              [cond
                ;when car list is * / and car operation is * /
                ([or (equal? (car operation) *) (equal? (car operation) /)]
                 (evaluate (cdr list) (append (cons (car list) null) (cdr operation)) (append operand (cons (car operation) null)) shift))
                 ;when car list is * / and car operation is + -
                (else
                 (evaluate (cdr list) (cons (car list) operation) operand shift))])
             (else
              [cond
                ;when car list is + - and operation is * /
                ([or (equal? (car operation) *) (equal? (car operation) /)]
                 (evaluate (cdr list) (append (cons (car list) null)(cdr operation)) (append operand (cons (car operation) null)) shift))
                 ;when car list is + - and operation is + -
                (else
                 (evaluate (cdr list) (append (cons (car list) null) (cdr operation)) (append operand (cons (car operation) null)) shift))])]]

          ; when operation is null
          [else
           (evaluate (cdr list) (append operation (cons (car list) null)) operand shift)]]]]]])
       
(define (infix list)
  (rpn (evaluate list)))                   
  
;tests
(displayln "(func1 5) -> 27")
(func1 5)
(displayln "(func1 0) -> 2")
(func1 0)
(displayln "(func1 -1) -> 3")
(func1 -1)
(displayln "")

(displayln "(func2 5) -> 27")
(func2 5)
(displayln "(func2 0) -> 2")
(func2 0)
(displayln "(func2 -1) -> 3")
(func2 -1)
(displayln "")

(displayln "(dec 0) --> -1")
(dec 0)
(displayln "(dec 5) --> 4)")
(dec 5)
(displayln "")

(displayln "(times3 7 15) -> '(21 . 45)")
(times3 7 15)
(displayln "(times3 0 4) -> '(0 . 12)")
(times3 0 4)
(displayln "(times3 -1 5) -> '(-3 . 15)")
(times3 -1 5)
(displayln "")

(displayln "(evens '(4 6 7)) -> '(#t #t #f)")
(evens '(4 6 7))
(displayln "(evens '(-1 0 -6)) -> '(#f #t #t)")
(evens '(-1 0 -6))
(displayln "(evens '()) -> '()")
(evens '())
(displayln "")

(displayln "(listTimes4 '(1 2 3)) -> '(4 8 12)")
(listTimes4 '(1 2 3))
(displayln "(listTimes4 '(-1 0 -3)) -> '(-4 0 -12)")
(listTimes4 '(-1 0 -3))
(displayln "")

(displayln "(rpn (list 3 4 5 + /) '()) -> 1/3")
(rpn (list 3 4 5 + /) '())
(displayln "(rpn (list 10 5 0 1 2 + * - /) '()) -> 2")
(rpn (list 10 5 0 1 2 + * - /) '())
(displayln "(rpn (list 25 0 5 - *)'()) -> -125")
(rpn (list 25 0 5 - *)'())
(displayln "")

(displayln "(evaluate (list 2 * \"(\" 3 + 4 \")\" )) --> '(2 3 4 + *)")
(evaluate (list 2 * "(" 3 + 4 ")"))
(displayln "(evaluate (list -1 + 5 * 0 / 3)) --> '(-1 5 0 * 3 / +)" )
(evaluate (list -1 + 5 * 0 / 3))
(displayln "((evaluate (list ))) --> '()")
(evaluate (list ))
(displayln "")

(displayln "(infix (list 2 * \"(\" 3 + 4 \")\")) --> 14") 
(infix (list 2 * "(" 3 + 4 ")"))
(displayln "(infix (list 2 * \"(\" \"(\" 20 / 5 \")\"  + 4 \")\")) --> 16")
(infix (list 2 * "(" "(" 20 / 5 ")"  + 4 ")"))
(displayln "(infix (list ) --> 0 ")
(infix (list ))