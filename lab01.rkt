#lang racket
(define (y-intercept m b)
  b)

(define (x-intercept m b)
  (/ (* -1 b) m))

(define (triangle-area m b)
  (abs (/ (*(y-intercept m b) (x-intercept m b)) 2)))

(define (modadd low high current addnum)
   (+ (modulo (+ current addnum (- low)) (- high low -1)) low))

(define (modsub low high current subnum)
  (modadd low high current (* subnum -1))) 

(define (addtime time mins)
  (define hours (floor (/ time 100)))
  (define minutes (modulo time 100))
  (define MinutesToHours (floor (/ (+ minutes mins) 60)))  
  (define addhours (modulo (+ hours MinutesToHours) 12))
  (define addmins (modulo (+ minutes mins) 60))
  
  (+ (* (modadd 1 12 12 (modulo addhours 12)) 100) addmins))

; Tests for imaginary - function:
(display "(y-intercept 0 3) -> 3):")
(y-intercept 0 3)

(display "(x-intercept -1 1) -> 1):")
(x-intercept -1 1)
(display "(x-intercept 1 0) -> 0):")
(x-intercept 1 0)

(display "(triangle-area 3 4) -> 2+2/3):")
(triangle-area 3 4)
(display "(triangle-area -1 0) -> 0):")
(triangle-area -1 0)
(display "(triangle-area -1 -2) -> 2):")
(triangle-area -1 -2)
(display "(triangle-area 5 -6) -> 3 +3/5 ):")
(triangle-area 5 -6)

(display "(modadd 0 0 0 0) -> 0):")
(modadd 0 0 0 0)
(display "(modadd 5 8 7 0) -> 7):")
(modadd 5 8 7 0)
(display "(modadd 5 8 7 2) -> 5):")
(modadd 5 8 7 2)
(display "(modadd 5 8 7 49) -> 8):")
(modadd 5 8 7 49)
(display "(modadd -6 3 -3 1) -> -2:")
(modadd -6 3 -3 1)

(display "(modsub 1 4 4 3) -> 1):")
(modsub 1 4 4 3)
(display "(modsub 1 4 3 -1) -> 4):")
(modsub 1 4 3 -1)

(display "(addtime 1230 240) -> 430):")
(addtime 1230 240)
(display "(addtime 1230 0) -> 1230):")
(addtime 1230 0)
