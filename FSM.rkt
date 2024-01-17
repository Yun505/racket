#lang racket
#|Hayun Jung
Kevin Liu and Benen Sullivan helped me|#
(define max_depth 5)

(define (DFA input Sigma S s0 Delta F)
  (define (DFA_runner input Sigma S s0 Delta F)
    (define (final_checker s0 F)
      [cond
        ((> (length (filter (λ(x) (equal? s0 x)) F)) 0)
         #t)
        (else
         #f)])
    
    (define (transition_checker element s0 Delta)
      [cond
        ((null? Delta)
         s0)
        [(equal? (caar Delta) s0)
         [cond
           ((equal? (cadar Delta) (string element))
            (caddar Delta))
           (else
            (transition_checker element s0 (cdr Delta)))]]
        (else
         (transition_checker element s0 (cdr Delta)))])
    
    [cond
      [(null? input)
         (final_checker s0 F)]
      [else
       (DFA_runner (cdr input) Sigma S (transition_checker (car input) s0 Delta) Delta F)]])
  
   (DFA_runner (string->list input) Sigma S s0 Delta F))

;NFA
(define (NFA input Sigma Q q0 Delta F)
  (define (final_checker q0 F)
    [cond
      ((null? q0)
       #f)
      ([> (length (filter (λ (x) [equal? (car q0) x]) F)) 0]
       #t)
      (else
       (final_checker (cdr q0) F))])

  (define (epsilon_checker q0 Delta depth)
    [cond
      ((equal? depth max_depth)
       '())
      (else
       (transition_checker ""  q0 Delta (+ depth 1)))])

  (define (transition_checker element q0 Delta depth)
    [cond
      ((null? q0)
       null)
      (else
       (define q0_matches (filter (λ (x) [equal? (car q0) (car x)]) Delta))
       [cond
         ((null? q0_matches)
          null)
         (else
          (define element_matches (filter (λ (x) [equal? element (cadr x)]) q0_matches))
          [cond
            ((null? element_matches)
             (epsilon_checker q0 Delta depth))
            ((equal? element "")
             (append (transition_checker element (cdr q0) Delta depth) (cddar element_matches)))
            [(list? (caddar element_matches))
             [append (append (caddar element_matches) (transition_checker element (cdr q0) Delta depth)) (epsilon_checker (caddar element_matches) Delta depth)]]
            [else
             [append (cons (caddar element_matches) (transition_checker element (cdr q0) Delta depth)) (epsilon_checker (list (caddar element_matches)) Delta depth)]]])])])
  
  (define (NFA_runner input Sigma Q q0 Delta F depth)
    [cond
      [(or (null? q0) (equal? depth max_depth))
       #f]
      [(null? input) 
       (final_checker (append q0 (epsilon_checker q0 Delta 0)) F)]
      [else
       (NFA_runner (cdr input) Sigma Q (transition_checker (string (car input)) q0 Delta 0) Delta F (+ depth 1))]])

  (NFA_runner (string->list input) Sigma Q (append (cons q0 null) (epsilon_checker (cons q0 null) Delta 0)) Delta F 0))

       

;Test Cases
(displayln "DFA CASES:")
;no input
(displayln "(DFA \"\" '(\"1\" \"0\") '(q0 q1) 'q0 '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '(q1)) -> #f")
(DFA "" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1))
;no delta
(displayln "(DFA \"1000\" '(\"1\" \"0\") '(q0 q1) 'q0 '() '(q1)) -> #f")
(DFA "1000" '("1" "0") '(q0 q1) 'q0 '() '(q1))
;no final
(displayln "(DFA \"1000\" '(\"1\" \"0\") '(q0 q1) 'q0 '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '()) -> #f")
(DFA "1000" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '())
;no start
(displayln "(DFA \"1000\" '(\"1\" \"0\") '(q0 q1) ' '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '(q1)) -> #f")
(DFA "1000" '("1" "0") '(q0 q1) '() '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1))
;no input + accept state is first one
(displayln "(DFA \"\" '(\"1\" \"0\") '(q0 q1) 'q0 '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '(q0)) -> #t")
(DFA "" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q0))
;Normal Case(just testing)
(displayln "(DFA \"1010\" '(\"1\" \"0\") '(q0 q1) 'q0 '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '(q1)) -> #t")
(DFA "1010" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1))

(displayln "")
(displayln "NFA CASES:")
;no input
(displayln "(NFA \"\" '(\"1\" \"0\") '(q0 q1) 'q0 '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '(q1)) -> #f")
(NFA "" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1))
;no delta
(displayln "(NFA \"1000\" '(\"1\" \"0\") '(q0 q1) 'q0 '() '(q1)) -> #f")
(NFA "1000" '("1" "0") '(q0 q1) 'q0 '() '(q1))
;no final
(displayln "(NFA \"1000\" '(\"1\" \"0\") '(q0 q1) 'q0 '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '()) -> #f")
(NFA "1000" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '())
;no start
(displayln "(NFA \"1000\" '(\"1\" \"0\") '(q0 q1) ' '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '(q1)) -> #f")
(NFA "1000" '("1" "0") '(q0 q1) '() '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q1))
;no input + accept state is first one
(displayln "(NFA \"\" '(\"1\" \"0\") '(q0 q1) 'q0 '((q0 \"0\" q0) (q0 \"1\" q1) (q1 \"0\" q1) (q1 \"1\" q1)) '(q0)) -> #t")
(NFA "" '("1" "0") '(q0 q1) 'q0 '((q0 "0" q0) (q0 "1" q1) (q1 "0" q1) (q1 "1" q1)) '(q0))
;Multiple states
(displayln "(NFA \"aba\" '(\"a\" \"b\") '(t1 t2 t3 t4 NA) 't1
'(
(t1 \"b\" (t1)) (t1 \"a\" (t1 t2)) (t2 \"b\" (t3)) (t2 \"a\" (NA)) (t3 \"b\" (NA))
(t3 \"a\" (t4)) (t4 \"a\" (t4)) (t4 \"b\" (t4)) (NA \"a\" (NA)) (NA \"b\" (NA))
)
'(t4)) -> #t")
(NFA "aba" '("a" "b") '(t1 t2 t3 t4 NA) 't1
'(
(t1 "b" (t1)) (t1 "a" (t1 t2)) (t2 "b" (t3)) (t2 "a" (NA)) (t3 "b" (NA)) (t3 "a" (t4)) (t4 "a" (t4)) (t4 "b" (t4)) (NA "a" (NA)) (NA "b" (NA)))
'(t4))

;EPSILON
(displayln "(NFA \"b\" '(\"a\" \"b\") '(t1 t2 t3 NA) 't1 '(
  (t1 \"\" t2) (t1 \"a\" (NA)) (t1 \"b\" (NA)) (t2 \"b\" t3) (t2 \"a\" (NA)) (t3 \"a\" (NA)) (t3 \"b\" (NA))) '(t3)) -> #t")
(NFA "b" '("a" "b") '(t1 t2 t3 NA) 't1 '(
  (t1 "" t2) (t1 "a" (NA)) (t1 "b" (NA)) (t2 "b" t3) (t2 "a" (NA)) (t3 "a" (NA)) (t3 "b" (NA))) '(t3))
;2 SHIFT EPSILON
(displayln "(NFA \"a\" '(\"a\" \"b\") '(t1 t2 t3 t4 NA) 't1 '(
  (t1 \"b\" t1) (t1 \"a\" t2) (t2 \"\" t3) (t2 \"a\" (NA)) (t2 \"b\" (NA)) (t3 \"\" t4) (t3 \"a\" (NA)) (t3 \"b\" (NA)) (t4 \"a\" (NA)) (t4 \"b\" (NA))) '(t4))-> #t")
(NFA "a" '("a" "b") '(t1 t2 t3 t4 NA) 't1 '(
  (t1 "b" t1) (t1 "a" t2) (t2 "" t3) (t2 "a" (NA)) (t2 "b" (NA)) (t3 "" t4) (t3 "a" (NA)) (t3 "b" (NA)) (t4 "" (NA)) (t4 "b" (NA))) '(t4))
;Infinite Epsilon
(displayln "(NFA \"a\" '(\"a\") '(t1 t2) 't1 '((t1 \"a\" t2) (t2 \"a\" t1)) '(t2)) -> #t")
(NFA "a" '("a") '(t1 t2) 't1 '((t1 "a" t2) (t2 "a" t1)) '(t2))
;Infinite Epsilon that doesn't work
(displayln "(NFA \"a\" '(\"a\") '(t1 t2 t3) 't1 '((t1 \"a\" t2) (t2 \"a\" t1)) '(t3)) -> #f")
(NFA "a" '("a") '(t1 t2 t3) 't1 '((t1 "a" t2) (t2 "a" t1)) '(t3))