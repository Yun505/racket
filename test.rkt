#lang racket
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