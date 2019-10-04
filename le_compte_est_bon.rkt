#lang racket

(define-syntax-rule (let1 a b body ...)
  (let ((a b)) body ...))

(require data/enumerate/lib)
(require rackunit)
(require racket/format)

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

(define (handled-list-compute-result nbs ops)
  (with-handlers ([exn:fail:contract:divide-by-zero? void]
                  [(lambda (e) (equal? e 'arithmetic)) void])
    (list-compute-result nbs ops)))

;exn:fail:contract:divide-by-zero
(define (list-compute-result nbs ops)
  (if (null? ops)
      (car nbs)
      ((car ops) (car nbs) (list-compute-result (cdr nbs) (cdr ops)))))

(check-equal?
 (list-compute-result '(1) '())
 1
 "list-compute-result 1")
(check-equal?
 (list-compute-result '(1 2) (list +))
 3
 "list-compute-result 2")

(define (strict-quotient n m)
  (if (not (equal? (remainder n m) 0))
      (raise 'arithmetic)
      (quotient n m)))

(define (strict-minus n m)
  (if (< n m)
      (raise 'arithmetic)
      (- n m)))

(define (op->string op)
  ; not using case, because quoted clause
  (cond
    ((equal? op +) "+")
    ((equal? op strict-minus) "-")
    ((equal? op -) "-")
    ((equal? op *) "*")
    ((equal? op strict-quotient) "/")
    [#t (~a op)]))

(check-equal?
 (op->string +)
 "+"
 "op->string")

(define (op-list->string nbs ops)
    (if (null? ops)
        (number->string (car nbs))
        (string-append "(" (number->string (car nbs)) " " (op->string (car ops)) " " (op-list->string (cdr nbs) (cdr ops)) ")")))

(check-equal?
 (op-list->string '(1 2 3 4) (list + - *))
 "(1 + (2 - (3 * 4)))"
 "op-list->string")

(define nbss (permutations nbs))
(define opss (apply cartesian-product (make-list 5 (list + strict-minus * strict-quotient))))
;> (length A)
;737280
(define A (cartesian-product nbss opss))
(define V (make-vector (+ 1 (list-compute-result nbs (make-list 5 *)))))
(define sub-A-10 (take A 10))
(define sub-A-10000 (take A 10000))

(define (V-set-line! A-line)
  (let1 res (apply handled-list-compute-result A-line)
        (when (exact-nonnegative-integer? res)
          (vector-set! V res (op-list->string (car A-line) (cadr A-line))))))

(define (V-set! n)
  (map V-set-line!
       (take A n))
  (vector-count (lambda (_) (equal? _ 0)) (vector-take V 1000)))


(define (V-set-all!)
  (map V-set-line! A)
  (vector-count (lambda (_) (equal? _ 0)) (vector-take V 1000)))

;> (time (V-set! 500000))
;cpu time: 18709 real time: 18685 gc time: 4406
;211

;> (time (V-set-all!))
;cpu time: 53747 real time: 53684 gc time: 11456
;132

; build all trees keeping content in order
; all trees for 1 permutation of a list of n elements
(define (trees-n-1 l)
  (cond ((equal? (length l) 1) l)
        (#t
         (apply append
           (map
            (lambda (i)
              ; cartesian-product returns the list of all 2-lists containing a tree of
              ; i first elements and a tree of all but i last elements. We cons the 2
              ; elements of each 2-list.
              (map (lambda (_) (apply cons _))
                   (cartesian-product (trees-n-1 (take l i)) (trees-n-1 (list-tail l i)))))
            (range 1 (length l)))))))

(check-equal?
 (trees-n-1 '(1))
 '(1)
 "trees-n-1 1")
(check-equal?
 (trees-n-1 '(1 2))
 '((1 . 2))
 "trees-n-1 2")
(check-equal?
 (trees-n-1 '(1 2 3))
 '((1 2 . 3) ((1 . 2) . 3))
 "trees-n-1 3")
(check-equal?
 (trees-n-1 '(1 2 3 4))
 '((1 2 3 . 4) (1 (2 . 3) . 4) ((1 . 2) 3 . 4) ((1 2 . 3) . 4) (((1 . 2) . 3) . 4))
 "trees-n-1 4")


(define (trees l)
  (apply append (map trees-n-1 (permutations l))))

(check-equal?
 (trees '(1 2))
 '((1 . 2) (2 . 1))
 "trees 2")
(check-equal?
 (trees '(1 2 3))
 '((1 2 . 3)
  ((1 . 2) . 3)
  (2 1 . 3)
  ((2 . 1) . 3)
  (1 3 . 2)
  ((1 . 3) . 2)
  (3 1 . 2)
  ((3 . 1) . 2)
  (2 3 . 1)
  ((2 . 3) . 1)
  (3 2 . 1)
  ((3 . 2) . 1))
 "tree 1 2 3")

; build a one-op op-tree inserting the same operator op at each non-leaf node of the initial number-only tree.
; a real op-tree has different operators at its nodes.
(define (op-tree op tree)
  (if (not (pair? tree))
      tree
      (cons op (cons (op-tree op (car tree)) (op-tree op (cdr tree))))))

(check-equal?
 (op-tree + '(1 2 3 . 4))
 (list* + 1 + 2 + '(3 . 4)) ;'(#<procedure:+> 1 #<procedure:+> 2 #<procedure:+> 3 . 4)
 "op-tree")

(define (ops-tree ops tree)
  (if (not (pair? tree))
      (cons tree ops)
      (let* ((right-op-tree-remaining-ops (op-tree (cdr ops) (car tree)))
             (right-op-tree (car right-op-tree-remaining-ops))
             (right-remaining-ops (cdr right-op-tree-remaining-ops))
             (left-op-tree-remaining-ops (op-tree right-remaining-ops (cdr tree)))
             (left-op-tree (car left-op-tree-remaining-ops))
             (remaining-ops (cdr left-op-tree-remaining-ops))
             (op-tree (cons (car ops) (cons right-op-tree left-op-tree))))
        (cons op-tree remaining-ops))))


(check-equal?
 (car (ops-tree '() 1))
 1 
 "ops-tree 1")

;(check-equal?
; (ops-tree (list + - *) '(1 2 4 . 3))
; (list* + 1 * 2 - '(4 . 3)) ;'(#<procedure:+> 1 #<procedure:+> 2 #<procedure:+> 3 . 4)
; "ops-tree")

; compute the result of a op-tree ot
(define (compute-op-tree ot)
  (if (not (pair? ot))
      ot
      ((car ot) (compute-op-tree (cadr ot)) (compute-op-tree (cddr ot)))))

(check-equal?
 (compute-op-tree (op-tree + '(1 2 3 . 4)))
 10
 compute-op-tree)

(define (op-tree->string ot)
  (if (not (pair? ot))
      (number->string ot)
      (string-append "(" (op-tree->string (cadr ot)) " " (op->string (car ot)) " " (op-tree->string (cddr ot)) ")")))

(check-equal?
 (op-tree->string(op-tree + '(1 2 3 . 4)))
 "(1 + (2 + (3 + 4)))"
 "op-tree->string")

