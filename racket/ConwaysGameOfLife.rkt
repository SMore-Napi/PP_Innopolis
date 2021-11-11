; Lab 2
; Roman Soldatov B19-SD-01

#lang slideshow

;Plot data series as a histogram
(require 2htdp/image)

(define (ex-sin x) (abs(+ 100 (* 50 (sin x)))))

(define (plot-bars lst) 
                        (define (helper lst current)
                           (cond
                             [(empty? lst) current]
                             [else (helper (rest lst)
                                           (hb-append 1 current (rectangle 10 (first lst) "solid" "black")))]))
                        (helper (rest lst) (hb-append (rectangle 10 (first lst) "solid" "black"))))

(plot-bars (map ex-sin (range 0 10 0.2)))

; Conway’s Game of Life
(define alive 1)
(define dead 0)
(define maze (list -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6))
(define (neighbours-list i j) (list (list (- i 1) (- j 1)) (list i (- j 1)) (list (- i 1) j) (list (+ i 1) (- j 1))
                                    (list (- i 1) (+ j 1)) (list (+ i 1) (+ j 1)) (list i (+ j 1)) (list (+ i 1) j)))

(define (render-cell value)
  (cond
    [(= value 1) (rectangle 10 10 "solid" "black")]
    [else (rectangle 10 10 "solid" "white")]))

(define (render-row r conw)
  (define (printer con lst current)
    (cond
      [(empty? lst) current]
      [else (printer con (rest lst) (hb-append current (render-cell (con  (first lst) r))))]
    ))
  (printer conw (rest maze) (hb-append (render-cell (conw  (first maze) r)))))

(define (render-conway conw)
  (define (printer con lst current)
    (cond
      [(empty? lst) current]
      [else (printer con (rest lst) (vc-append current (render-row  (first lst) conw)))]
    ))
  (printer conw (rest maze) (vc-append (render-row (first maze) conw))))

(define (neighbours-number i j conw)
  (define (helper con i j lst sum)
    (cond
      [(empty? lst) sum]
      [else (helper con i j (rest lst) (+ sum (con (first (first lst)) (first (rest (first lst))))))]
    ))
  (helper conw i j (neighbours-list i j) 0))

(define (update-cell i j conw)
  (cond
    [(and (= (conw i j) alive) (<= 2 (neighbours-number i j conw) 3)) alive]
    [(and (= (conw i j) dead) (= (neighbours-number i j conw) 3)) alive]
    [else dead]))

(define (update-row r conw)
  (define (helper con lst current)
    (cond
      [(empty? lst) current]
      [else (helper con (rest lst) (cons (update-cell (first lst) r con) current))]
    ))
  (helper conw (rest maze) (cons (update-cell (first maze) r conw) empty)))

(define (update-conway conw)
  (define (helper con lst current)
    (cond
      [(empty? lst) current]
      [else (helper con (rest lst) (cons (update-row (first lst) con) current))]
    ))
  (helper conw (rest maze) (cons (update-row (first maze) conw) empty)))

(define (conway-lst lst)
  (lambda (i j)
   (cond
     [(> -6 i) dead]
     [(> -6 j) dead]
     [(< 6 i) dead]
     [(< 6 j) dead]
     [(= (list-ref (list-ref lst (+ i 6)) (+ j 6)) 1) alive]
     [else dead])))

(define (conway-steps conw steps)
  (define (helper con steps current)
    (cond
      [(= steps 0) current]
      [else (helper (conway-lst (update-conway con)) (- steps 1) (cons con current))]
    ))
  (helper conw steps empty))

(define (render-conway-steps conw steps)
  (for/list ([step (reverse (conway-steps conw steps))])
    (render-conway step)))

(define conway-example
  (lambda (i j)
    (cond
      [(and (< (abs i) 3)
            (< (abs j) 3)
            (odd? (+ i j)))
       alive]
      [else dead])))

(render-conway-steps conway-example 5)
