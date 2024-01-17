#lang racket
#|Hayun Jung
 Kevin Liu helped me
 I helped Devon Lai|#

(define (get-element list n)
  [cond
    ((equal? n 0)
     (car list))
    [else
     (get-element (cdr list) (- n 1))]])
 

(define (append-element list y)
  (cond
    ((null? list)
     (cons y null))
    ((null? (cdr list))
     (cons (car list) (cons y null)))
    (else
     (cons (car list) (append-element (cdr list)  y)))))

(define (append-list first second)
  (cond
    ((null? second)
     first)
    (else
     (append-list (append-element first (car second)) (cdr second)))))

(define (backwards list)
  (cond
    ((null? list)
     list)
    (else
     (append-element (backwards (cdr list)) (car list)))))

(define (paren-balanced? string (stack '()))
  (define (opening? element)
    [cond
      [(or (equal? element #\() (equal? element #\[) (equal? element #\{))
       #t]
      [else
       #f]])
  (define (closing? element)
    [cond
       [(or (equal? element #\)) (equal? element #\]) (equal? element #\}))
       #t]
       [else
        #f]])
  (define (corresponding element)
    [cond
      ((equal? element #\()
       ")")
      ((equal? element #\{)
       "}")
      ((equal? element #\[)
       "]")
      ((equal? element #\))
       ")")
      ((equal? element #\})
       "}")
      ((equal? element #\])
       "]")])
  
  (define (element-match element on-list)
    [cond
      ((equal? element on-list)
       #t)
      (else
       #f)])
    
  (define (check? list stack)
    (cond
      ((empty? list)
       (cond
         ((empty? stack)#t)
         (else #f)))
      (else
       (cond
         ((opening? (car list))
          (check? (cdr list) (cons (corresponding (car list)) stack)))
    
         ((closing? (car list))
          (cond
            ((element-match (corresponding (car list)) (car stack))
             (check? (cdr list) (cdr stack)))
            (else
             #f)))
         (else
          (check? (cdr list) stack))))))

  (check? (string->list string) stack))

;test cases
(displayln "Test for get-element:")
(displayln "(get-element '(1 2 3) 1) -> 2")
(get-element '(1 2 3) 1)
(displayln "(get-element '(() 2) 0) -> '()")
(get-element '(() 2) 0)

(displayln "Test for append-element:")
(displayln "(append-element '(1 2 4) 0) -> '(1 2 4 0)")
(append-element '(1 2 4) 0)
(displayln "(append-element '() null) -> '(())")
(append-element '() null)

(displayln "Test for append-list:")
(displayln "(append-list '(1 2 3) '(4 5 6)) -> '(1 2 3 4 5 6)")
(append-list '(1 2 3) '(4 5 6))
(displayln "(append-list '(1 2 3) '()) -> '(1 2 3)")
(append-list '(1 2 3) '())
(displayln "(append-list '() '()) -> '()")
(append-list '() '())

(displayln "Test for backwards:")
(displayln "(backwards '(1 2 3)) -> '(3 2 1)")
(backwards '(1 2 3))
(displayln "(backwards '()) -> '()")
(backwards '())

(displayln "Test for paren-balanced:")
(displayln "(paren-balanced? \"potatoes are cool but []{} no\") -> #t")
(paren-balanced? "potatoes are cool but []{} no")
(displayln "(paren-balanced? \"\") -> #t")
(paren-balanced? "")
(displayln "(paren-balanced? \"[[[\") -> #f")
(paren-balanced? "[[[")
(displayln "(paren-balanced? \"askldjflks;fjsd[[[]]]\") -> #t")
(paren-balanced? "askldjflks;fjsd[[[]]]")
(displayln "(paren-balanced? \"123(([[\") -> #f")
(paren-balanced? "123(([[")