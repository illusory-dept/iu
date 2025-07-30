iu (c) tttiw (l) mit
iu is inspired by "u-incu".
https://github.com/nekoarch/u-incunabulum
====================================
rlwrap scala run .
====================================
; QUICK START
(load "lib/list.iu")(load "lib/math.iu")    ; order matters
(map (lambda (x) (* x x)) (quote (1 2 3)))  ; (1 4 9)
(unique (quote (1 2 2 3 3 3 4)))            ; (1 2 3 4)
(load "exa/mat.iu")
====================================
#t #nil + - * / % ^ < > = <= >= sqrt
list define lambda if cond load quote
atom eq car cdr cons and or not xor
str-append join strlen display println
:time :quit
=====================================
(define let1 (lambda (f a b) (f a b)))
(let1 (lambda (a b) (/ (+ a b) 2)) 10 14)
(define let-size (lambda (f a b) (f a b)))
(let-size (lambda (len sum) (/ sum len))
          (length '(1 2 3 4)) (sum '(1 2 3 4)))
; let*
((lambda (a)
   ((lambda (b)
      (+ a b))
    (* a 2)))
 5)
