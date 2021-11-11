; Lab 4
; Roman Soldatov B19-SD-01

#lang slideshow

; Exercise 4.1.
(define (attacks? q1 q2)
  (let ([x1 (car q1)]
        [y1 (car (cdr q1))]
        [x2 (car q2)]
        [y2 (car (cdr q2))])
    (cond
    [(= x1 x2) #t]
    [(= y1 y2) #t]
    [(= (abs (- x1 x2)) (abs (- y1 y2))) #t]
    [else #f])))

; Exercise 4.2.
(define (attacks-any? q queens)
  (ormap (lambda(q2)(attacks? q q2)) queens))

; Exercise 4.3.
(define (no-attacks? queens)
  (define (helper q queens)
    (cond
      [(empty? queens) #t]
      [(attacks-any? q queens) #f]
      [else (helper (car queens) (cdr queens))]))
  (helper (car queens) (cdr queens)))

; Exercise 4.4.
(define (for-range start end f)
  (map f (range start end)))

; Exercise 4.5.
(define (naive-four-queens)
  (apply append (for-range 1 5 (lambda (x1)
    (apply append (for-range 1 5 (lambda (x2)
      (apply append (for-range 1 5 (lambda (x3)
        (apply append (for-range 1 5 (lambda (x4)
          (let [(queens-lst
            (list (list x1 1)
                  (list x2 2)
                  (list x3 3)
                  (list x4 4)))]
            (cond
              [(no-attacks? queens-lst) (list queens-lst)]
              [else empty])))))))))))))))


; Exercise 4.6.
; Don't brute-force all possible cells.

; Exercise 4.7.
(define (add-queen-at queens-arrangements n column)
   (define (helper n column)
    (lambda (queens-lst)
      (filter
        (lambda (queen) (no-attacks? queen))
        (for-range 1 (+ n 1) (lambda (row) (cons (cons row (cons column empty)) queens-lst))))))
  (cond
    [(= (length queens-arrangements) 1)
     (filter
        (lambda (queen) (no-attacks? queen))
        (for-range 1 (+ n 1) (lambda (row) (cons (cons row (cons column empty)) (car queens-arrangements)))))]
    [else (apply append (map (helper n column) queens-arrangements))]))

; Exercise 4.8.
(define (eight-queens)
  (define (helper solutions column)
    (cond
      [(= column 9) solutions]
      [else (helper (add-queen-at solutions 8 column) (+ column 1))]))
  (helper '(()) 1))

; Exercise 4.9.
(define (n-queens n)
  (define (helper solutions column n)
    (cond
      [(= column (+ n 1)) solutions]
      [else (helper (add-queen-at solutions n column) (+ column 1) n)]))
  (helper '(()) 1 n))

; Exercise 4.10.
(require racket/generator)

(define (for-range-gen lst f)
          (if (empty? lst)
              "The end."
              (begin
               (yield (f (first lst)))
               (for-range-gen (rest lst) f))))

(define for-range-gen-1-5
  (generator () (for-range-gen (range 1 5) (lambda (x) (* x x)))))

(for-range-gen-1-5)
(for-range-gen-1-5)
(for-range-gen-1-5)
(for-range-gen-1-5)
(for-range-gen-1-5)

; Exercise 4.11.
(define eight-queens-gen
  (generator () (for-range-gen (eight-queens) (lambda (x)(cons 'Solution: x )))))

(eight-queens-gen)
(eight-queens-gen)
(eight-queens-gen)
(eight-queens-gen)
