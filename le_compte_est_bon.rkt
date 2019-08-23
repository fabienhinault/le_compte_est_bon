#lang racket

(define-syntax-rule (let1 a b body ...)
  (let ((a b)) body ...))

(require data/enumerate/lib)
(require rackunit)

(define nbs '(2 3 7 8 9 10))
(define ops '(+ - * /))

; make an accumulator for fonction h
; 2 -> '((2) (2 ()))
; The first list is the working space where all the number are stored.
; The following ones will contain a number and the list of all other numbers.
; Hence, for the first time: x, and the empty list.
(define (make-acc x)
  `((,x) (,x ())))

(check-equal? (make-acc 2) '((2) (2 ())))

; add a new number x to the accumulator
; add it to the first list = (car acc) = working space
; add the new list of x and the list of other numbers
; add x to the previous lists, as other number
(define (add-to-acc x acc)
  (list* (cons x (car acc))
         (list x (car acc))
         (map (lambda (_) (list (car _) (cons x (cadr _))))
              (cdr acc))))

(define (g l acc)
  (if (null? l)
      acc
      (g (cdr l) (add-to-acc (car l) acc))))

(define (h xs)
  (cdr (g (cdr xs) (make-acc (car xs)))))

(define lceb (h nbs))

(map (lambda (_)
       (list (list 10 (car _))
             (cadr _)))
     (h (cadar lceb)))

(length (permutations nbs))

(fin/e + - *)

(listof-n/e (fin/e + - *) 5)

(define (compute-result nbs ops)
  (if (null? ops)
      (car nbs)
      ((car ops) (car nbs) (compute-result (cdr nbs) (cdr ops)))))

(define nbss (permutations nbs))
(define opss (apply cartesian-product (make-list 5 (list + - *))))
;> (length A)
;174960
(define A (cartesian-product nbss opss))
(define V (make-vector (+ 1 (compute-result nbs (make-list 5 *)))))
(define sub-A-10000 (take A 10000))