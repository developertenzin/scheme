#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(display "(rember* a l) removes all 'a'(s) from a nested list\n")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      ((eq? (car l) a) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l)))))))

(rember* 'cat '((dog cat) (sheep (whale (more cat and more cat) tiger) lion cat) (crazy (cat and (and more cat) more cat) cat))) 

(display "(insertR* new old l) inserts 'new' to the right of every occurence of 'old' in the nested (or not nested) list.\n")



#| my version using "list?"
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
      ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
      (else (cons (car l) (insertR* new old (cdr l)))))))
|#

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
        ((eq? (car l) old)
         (cons old (cons new (insertR* new old (cdr l)))))
        (else
         (cons (car l) (insertR* new old (cdr l))))
       )
      )
      (else
       (cons (insertR* new old (car l)) (insertR* new old (cdr l)))
      ))))
   
(insertR* 'qazi 'rafeh '(rafeh is pretty (cool (but rafeh is a little ****) and rafeh is a *****)))

(display "(define (occur* a l)) checks how many times 'a' occurs in l\n")
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)))))
      (else
       (+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'apple '(apple ball cat apple apple more apple tenzin apple))
       
(display "(define (subst* new old l)) substitutes all the occurences of 'old' in the 'l' with 'new'\n")
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
          (cons (car l) (subst* new old (cdr l))))
         ))
      (else
       (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'god 'blank '(qazi is a blank (he was born (a blank) he is a blank among men)))
       


(display "(define (insertL* new old l)) inserts 'new in front of every occurence of 'old in the nested(or not nested) list.\n")
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old (insertL* new old (cdr l)))))
         (else
          (cons (car l) (insertL* new old (cdr l))))))
       (else
        (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'tenzin 'phuljung '(phuljung was born (in 1993 (and phuljung was born a god as well (phuljung was meant to be a god)))))
              

(display "(member* a l) returns true if 'a exists in the nested (or not nested) list l and false otherwise.\n")
(define member*
  (lambda (a l)
    (cond
      ((null? l) false)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) true)
         (else (member* a (cdr l)))))
      (else
       (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'tenzin '(tenzin old (gold henry tenzin (more tenzin) (and more (tenzin))) and more tenzin))
          

(display "(leftmost l) returns the left most atom of an not empty list that does not contain any empty list.\n")
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l))
       (car l))
      (else
       (leftmost (car l))))))

(leftmost '((((tango) tenzin phuljung) rafeh) qazi))


(display "(eqlist? l1 l2) will return true if both lists are equal and false otherwise.\n")
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) true)
      ((and (null? l1) (atom? (car l2))) false)
      ((and (null? l2) (atom? (car l1))) false)
      ((and (atom? (car l1)) (list? (car l2))) false)
      ((and (atom? (car l2)) (list? (car l1))) false)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      (else
       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(tenzin (hey hey)qazi matt) '(tenzin (hey hey) qazi matt))

      


