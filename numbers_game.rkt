#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda(n)
    (+ n 1)
    ))
(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 0)

(define ++
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (add1 (++ x (sub1 y)))))))

(++ 4 2)


(define --
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (-- x (sub1 y)))))))

(-- 8 5)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(addtup '(2 3 4 50))

(define xx
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (+ a (xx a (sub1 b)))))))

(xx 3 13)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2 3 4 5) '(10 10 10))

(define >>
  (lambda (a b)
    (cond
      ((zero? a) false)
      ((zero? b) true)
      (else (>> (sub1 a) (sub1 b))))))

(>> 12 21)

(define ==
  (lambda (a b)
    (cond
      ((and (zero? a) (zero? b) true))
      ((zero? a) false)
      (else (== (sub1 a) (sub1 b))))))

(== '6 '4)

(define ^^
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (xx a (^^ a (sub1 b)))))))

(^^ '2 '3)
      
       
(define //
  (lambda (a b)
    (cond
      ((< a b) 0)
      (else (add1 (// (- a b) b))))))

(// '15 '5)


(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(length '(hey how are you jerry))


(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick '3 '(rafeh qazi matt tenzin bob))

76

  

