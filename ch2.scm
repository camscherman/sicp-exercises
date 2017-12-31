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

(define (for-each proc items)
  (cond ((null? items) true)
        (else (proc (car items))
              (for-each proc (cdr items)))))
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
          (cons (tree-map (car tree))(tree-map (cdr tree))))))

(define (square-tree-map tree)
  (tree-map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree-map sub-tree)
           (* sub-tree sub-tree))) tree))
