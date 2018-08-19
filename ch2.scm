(define (numer x)
  (car x))

(define (denom x)
  (cdr x))
; (define (make-rat n d)
;   (cons n d))

(define (print-rat x)
  (newline)
  (display (numer x))
           (display "/")
           (display (denom x)))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (remainder a b)
  (modulo a b))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (numer x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x)(denom y))
                (* (numer y) (denom x)))
            (* (numer x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x)(numer y))
               (* (denom x)(denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y)(denom x))))




(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))


(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1)(append (cdr list1) list2))))

(define (last-pair list1)
  (if (> 2 (length list1))
         list1
         (last-pair (cdr list1))))

(define (reverse list1)
  (if (null? list1)
    list1
    (append (reverse (cdr list1))(list (car list1)))))

(define (map proc list1)
  (if (null? list1)
    '()
    (cons (proc (car list1)) (map proc (cdr list1)))))

; (define (for-each proc items)
;   (cond ((null? items) true)
;         (else (proc (car items))
;               (for-each proc (cdr items)))))
; there is a problem with this

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(define (deep-reverse x)
  (cond ((null? x) x)
        ((pair? (car x))
         (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))
        (else
          (append (deep-reverse (cdr x)) (list (car x))))))

(define (fringe x)
  (cond ((null? x) x)
        ((pair? (car x))
         (append (fringe (car x))(fringe (cdr x))))
        (else
           (cons (car x)(fringe (cdr x))))))
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (total-weight mobile)
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

(define (total-weight-branch branch)
  (if (not (pair?  (car (cdr branch))))
    (car (cdr branch))
    (total-weight (car (cdr branch)))))

(define (torque branch)
  (if (not (pair? branch))
    0
  (* (car branch)(total-weight-branch branch))))


(define (balanced? mobile)
  (cond ((not (pair? mobile)) true)
   ((and (balanced? (left-branch mobile))(balanced? (right-branch mobile)))
        (= (torque (left-branch mobile))(torque (right-branch mobile))))
   (else false)))

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree))
         (* tree tree))
        (else
          (cons (square-tree (car tree))(square-tree (cdr tree))))))

(define (tree-map fn tree)
  (cond ((null? tree) tree)
        ((not (pair? tree))
            (fn tree))
        (else
          (cons (tree-map fn (car tree))(tree-map fn (cdr tree))))))

(define (square-tree-map tree)
  (tree-map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree-map sub-tree)
           (* sub-tree sub-tree))) tree))
(define nil '())

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))


            ;Ch 2.2.3 Conventional interfaces
            ; Extra fib function from chapter 1
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))



(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
          (cons (car sequence)(filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate operation initial sequence)
  (if (null? sequence)
        initial
        (operation (car sequence)
                  (accumulate operation initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
                      ;Fibonnaci evens using map filter accumulate

(define (even-fibs n)
    (accumulate cons nil
                (filter even?
                    (map fib
                      (enumerate-interval 0 n)))))
                      ; Alterate map exercise 2.33
(define (map fn sequence)
        (accumulate (lambda (x y)(cons (fn x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y)(+ y 1)) 0 sequence))

(define (accumulate-n op init sequence)
    (if (null? (car sequence))
            nil
          (cons (accumulate op init (map car sequence))
            (accumulate-n op init (map cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op init seq)
    (define (iter result rest)
      (if (null? rest)
          result
          (iter (op result (car rest)) (cdr rest))))
          (iter init seq))


(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

;2.3 Symbolic Data

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? a b)
 (cond ((not (and (pair? a) (pair? b)))(eq? a b))
       ((not (pair? a)) false)
       ((not (pair? b)) false)
       (else (and (equal? (car a)(car b))(equal? (cdr a)(cdr b))))))

       ;Symbolic differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp) (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product (exponent exp)(make-exponentiation (base exp)(- (exponent exp) 1)) (deriv base var)))
        (else
              (error "unknown expression type --DERIV" exp))))

(define (variable? x)
        (symbol? x))

(define (same-variable? v1 v2)
      (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
(cond ((=))
    (else    (list '+ a1 a2))))

(define (make-product a1 a2)
        (list '* a1 a2))

(define (sum? a1)
        (and (pair? a1)(eq? (car a1) '+)))

(define (addend s)
        (cadr s))

(define (augend s)
        (caddr s))

(define (product? exp)
        (and (pair? exp) (eq? (car exp) '*)))

(define (multiplier p)
        (cadr p))

(define (multiplicand p)
        (caddr p))
; (define (exponentiation? exp)
;     (and (pair? exp)(eq? (car exp) '**)))
;
; (define (base exp)
;         (cadr exp))
;
; (define (exponent exp)
;         (caddr exp))
;
; (define (make-exponentiation base exponent)
;         (list '** base exponent))

; Not sure if exponentiation works fully

; Representing sets;
;Sets as unordered lists

(define (element-of-set? x set)
    (cond ((null? set) false)
            ((equal? x (car set)) true)
            (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
        (if (element-of-set? x set)
                set
              (cons x set)))

(define (intersection-set set1 set2)
      (cond ((or (null? set1) (null? set2)) '())
            ((element-of-set? (car set1)) set2)
              (cons (car set1)(interection-set (cdr set1) set2))
              (else (intersection-set (cdr set1) set2))))

;Excercise 2.59 Implement Union set for unordered Sets

(define (union-set set1 set2)
      (cond ((null? set1) set2)
            ((null? set2) set1)
            ((element-of-set? (car set1) set2)
              (union-set (cdr set1) set2))
            (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))
; Sets as ordered lists 
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
        (if (or (null? set1)(null? set2))
                '()
            (let ((x1 (car set1))(x2 (car set2)))
                 (cond ((= x1 x2) 
                        (cons x1 (intersection-set (cdr set1) (cdr set2))))
                        ((> x1 x2) (intersection-set set1 (cdr set2)))
                        ((< x1 x2) (intersection-set (cdr set1) set2))))))
; Binary Trees!!

(define (entry tree)
        (car tree))

(define (left-branch tree)
        (cadr tree))

(define (right-branch tree)
        (caddr tree))

(define (make-tree entry left right)
        (list entry left right))

(define (element-of-tree? x tree)
        (cond ((null? tree?) false)
              ((= x (entry tree)) true)
              ((< x (entry tree)) 
                (element-of-tree? x (left-branch tree))
              ((> x (entry tree))
               (element-of-tree? x (right-branch tree))))))


(define (insert-tree x tree)
      (cond ((null? tree) (make-tree x '() '()))
            ((= x (entry tree) tree))
            ((< x (entry tree)) 
              (make-tree (entry tree) (insert-tree x (left-branch tree)) (right-branch tree)))
            ((> x (entry tree))
              (make-tree (entry tree) (left-branch tree) (insert-tree x (right-branch tree))))))

(define (list->tree elements)
        (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
        (if (= n 0)
            (cons '() elts)
            (let ((left-size (quotient (- n 1) 2)))
                  (let ((left-result (partial-tree elts left-size)))
                    (let ((left-tree (car left-result))
                          (non-left-elts (cdr left-result))
                          (right-size (- n (+ left-size 1))))
                          (let ((this-entry (car non-left-elts))
                                (right-result (partial-tree (cdr non-left-elts) right-size)))
                                (let ((right-tree (car right-result))
                                       (remaining-elts (cdr right-result)))
                                      (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))




;Practice 

(define (list-length list)
        (list-length-iter list 0))

(define (list-length-iter list accum)
        (if (null? list)
              accum
              (list-length-iter (cdr list) (+ 1 accum))))

; Implement vector add

(define (vector-add v1 v2)
  (cond ((not (= (list-length v1)(list-length v2))) (error "lengths do not match"))
          ((null? v1) 
              '())
          (else (cons (+ (car v1)(car v2)) (vector-add (cdr v1)(cdr v2))))))

(define (dot-product-vector v1 v2)
  (cond ((not (= (list-length v1)(list-length v2))) (error "lengths do not match"))
        ((null? (cdr v1))
                (* (car v1)(car v2)))
        (else (+ (* (car v1)(car v2) (dot-product-vector (cdr v1) (cdr v2)))))) )

; Return to 
