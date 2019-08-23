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

(define (list-compute-result nbs ops)
  (if (null? ops)
      (car nbs)
      ((car ops) (car nbs) (compute-result (cdr nbs) (cdr ops)))))

; (2 3 7 8 9 10) = (2 . (3 . (7 . (8 . (9 . (10 . '()))))))
; (+ . ( () . (+ . ( () . (+ . ( () . (+ . ( () .  (+ ()))))))))) = '(+ () + () + () + () + ())
;
; (10) = (10 . ())
; ()
;
; (9 10) = (9 . (10 . ()))
; (+ . ( () . ())) = (+ ())
(define (compute-result nbs ops)
  (cond ((number? nbs) nbs)
        ((null? (cdr nbs)) (car nbs))
        (#t ((car ops) (compute-result (car nbs) (cadr ops)) (compute-result (cdr nbs) (cddr ops))))))

(define nbss (permutations nbs))
(define opss (apply cartesian-product (make-list 5 (list + - *))))
;> (length A)
;174960
(define A (cartesian-product nbss opss))
;(define V (make-vector (+ 1 (compute-result nbs (make-list 5 *)))))
(define sub-A-10000 (take A 10000))

(define (trees2  l2)
  (list (cons (car l2) (cadr l2)) (cons (cadr l2) (car l2))))

(define (tree2-1 l2)
  (cons (car l2) (cadr l2)))

; build all trees keeping content in order
;> (trees3-1 '(1 2 3))
;'((1 2 . 3) ((1 . 2) . 3))
(define (trees3-1 l3)
  (list (cons (car l3) (tree2-1 (cdr l3))) (cons (tree2-1 (take l3 2)) (caddr l3))))

;> (trees3 '( 1 2 3))
;'((1 2 . 3)
;  ((1 . 2) . 3)
;  (2 1 . 3)
;  ((2 . 1) . 3)
;  (1 3 . 2)
;  ((1 . 3) . 2)
;  (3 1 . 2)
;  ((3 . 1) . 2)
;  (2 3 . 1)
;  ((2 . 3) . 1)
;  (3 2 . 1)
;  ((3 . 2) . 1))
(define (trees3 l3)
  (apply append (map trees3-1 (permutations l3))))


;> (trees-n-1 '(1 2 3))
;'((1 2 . 3) ((1 . 2) . 3))
;> (trees-n-1 '(1 2 3 4))
;'((1 2 3 . 4) (1 (2 . 3) . 4) ((1 . 2) 3 . 4) ((1 2 . 3) . 4) (((1 . 2) . 3) . 4))
(define (trees-n-1 l)
  (cond ((equal? (length l) 2) (list(tree2-1 l)))
        ((equal? (length l) 1) l)
        (#t
         (apply append
           (map
            (lambda (i)
              (map (lambda (_) (apply cons _))
                   (cartesian-product (trees-n-1 (take l i)) (trees-n-1 (drop l i)))))
            (range 1 (length l)))))))

(define (trees l)
  (apply append (map trees-n-1 (permutations l))))
