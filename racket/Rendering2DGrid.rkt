; Lab 3
; Roman Soldatov B19-SD-01

#lang slideshow

(define list-example '(10 0 1 2 3 4 1 2 5 9 2))

; Exercise 3.1.
(define (len-via-foldl lst)
  (foldl (lambda (el len) (+ len 1)) 0 lst))

(len-via-foldl list-example)

; Exercise 3.2.
(define (len-via-map lst)
  (apply + (map (lambda (el) 1) lst)))

(len-via-map list-example)

; Exercise 3.3.
(define (average lst)
  (/ (apply + lst) (len-via-foldl lst)))

(average list-example)

; Exercise 3.4.
(define (my-contains? element lst) (ormap (lambda (x) (equal? x element)) lst))

(define (my-remove-duplicates lst)
  (define (helper lst current)
    (cond
      [(empty? lst) (reverse current)]
      [(my-contains? (first lst) current) (helper (rest lst) current)]
      [else (helper (rest lst) (cons (first lst) current))]))
  (helper lst empty))

(my-remove-duplicates list-example)

; Exercise 3.5.
(define (my-contains-with? element lst my-predicate) (ormap (lambda (x) (my-predicate x element)) lst))

(define (my-remove-duplicates-with my-predicate lst)
  (define (helper lst current)
    (cond
      [(empty? lst) (reverse current)]
      [(my-contains-with? (first lst) current my-predicate) (helper (rest lst) current)]
      [else (helper (rest lst) (cons (first lst) current))]))
  (helper lst empty))

(my-remove-duplicates-with (lambda (a b) (= a b)) list-example)

; Exercise 3.6.
; Associativity, transitivity, reflexivity

; Exercise 3.7.
(define (range-list start end)
  (define (helper start end current)
    (cond
      [(< start end) (helper (+ 1 start) end (cons start current))]
      [else (reverse current)]))
  (helper start end empty))

(define (for-range start end f)
  (map f (range-list start end)))

(for-range 1 10 (lambda (x) (* x 2)))

; Exercise 3.8.

(define (stitch-element-to-list elem lst)
  (map (lambda (x) (cons x elem)) lst))

(define (range-list-2D x-start y-start x-end y-end)
  (define (helper x-start y-start x-end y-end current)
    (cond
      [(< y-start y-end)
       (helper x-start (+ 1 y-start) x-end y-end
               (append (stitch-element-to-list y-start (range-list x-start x-end)) current))]
      [else (sort current <= #:key car)]))
  (helper x-start y-start x-end y-end empty))

(define increment-each-pair-coordinates
  (lambda (coordinates)
    (cons (+ (car coordinates) 1) (+ (cdr coordinates) 1))))

(define (for-range-2D x-start y-start x-end y-end f)
  (map f (range-list-2D x-start y-start x-end y-end)))

(for-range-2D 0 0 5 5 increment-each-pair-coordinates)

; Exercise 3.9.
(require 2htdp/image)

(define sum-each-pair-coordinates
  (lambda (coordinates)
    (+ (car coordinates) (cdr coordinates))))

(define cell-size 20)

(define (render-cell value)
    (cond
      [(<= value 1) (rectangle cell-size cell-size "solid" "black")]
      [(<= value 2) (rectangle cell-size cell-size "solid" "indigo")]
      [(<= value 3) (rectangle cell-size cell-size "solid" "blue")]
      [(<= value 4) (rectangle cell-size cell-size "solid" "Medium Cyan")]
      [(<= value 5) (rectangle cell-size cell-size "solid" "green")]
      [(<= value 6) (rectangle cell-size cell-size "solid" "yellow")]
      [(<= value 7) (rectangle cell-size cell-size "solid" "orange")]
      [(<= value 8) (rectangle cell-size cell-size "solid" "red")]
      [else (rectangle cell-size cell-size "solid" "white")]))

(define (render-row row)
  (define (printer row current)
    (cond
      [(empty? row) current]
      [else (printer (rest row) (hb-append current (render-cell (first row))))]))
  (printer (rest row) (render-cell (first row))))

(define (get-row row-number lst)
  (filter (lambda(cords)(equal? (car cords) row-number)) lst))

(define (render-2D x-start y-start x-end y-end f)
  (define (helper lst step current)
    (cond
      [(empty? lst) current]
      [else (helper (drop lst step) step (vc-append (render-row (take lst step)) current))]))
  (let ([lst (for-range-2D x-start y-start x-end y-end f)]
        [step (abs (- x-end x-start))])
    (helper (drop lst step) step (render-row (take lst step)))))

(render-2D 0 0 5 5 sum-each-pair-coordinates)


; Exercise 3.10.
(define (my-map f lst)
  (reverse (foldl (lambda (elem current)
            (cons (f elem) current)) '() lst)))

(my-map (lambda (x) (+ x 1)) list-example)

; Exercise 3.11.
(define (my-reverse lst)
  (foldl (lambda (elem current)
           (cons elem current)) '() lst))

(my-reverse list-example)

; Exercise 3.12.
(define (render-vector-field x-start y-start x-end y-end f)
  (render-2D x-start y-start x-end y-end f))

(define vector-field-function
 (lambda (coordinates)
   (+ (* ( * (sin (car coordinates)) (cos (cdr coordinates))) 10) 5)))

(render-vector-field 0 0 5 5 vector-field-function)
