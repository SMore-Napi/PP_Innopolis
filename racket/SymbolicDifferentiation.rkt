; Homework Assignment 1
; Roman Soldatov B19-SD-01
; r.soldatov@innopolis.university

#lang racket
(require racket/match)

; 1.1 Polynomial expressions

; Exercise 1.1

; Check whether a given expression is a variable
(define (variable? expr)
  (cond
    [(procedure? expr) #f]
    [else #t]))

; Check whether a given expression is a sum
(define (sum? expr)
  (match expr
    [(list '+ _ _) #t]
    [_ #f]))

; Extract first summand from a sum
(define (summand-1 expr)
  (match expr
    [(list '+ x _) x]))

; Extract second summand from a sum
(define (summand-2 expr)
  (match expr
    [(list '+ _ x) x]))

; Check whether a given expression is a product
(define (product? expr)
  (match expr
    [(list '* _ _) #t]
    [_ #f]))

; Extract first multiplier from a product
(define (multiplier-1 expr)
  (match expr
    [(list '* x _) x]))

; Extract second multipler from a product
(define (multiplier-2 expr)
  (match expr
    [(list '* _ x) x]))

; For Exercises 1.6-1.7

; Check whether a given expression is a polyvariadic sum
(define (polyvariadic-sum? expr)
  (if (and (list? expr) (> (length expr) 3) (equal? (first expr) '+)) #t #f))

; Check whether a given expression is a polyvariadic product
(define (polyvariadic-product? expr)
  (if (and (list? expr) (> (length expr) 3) (equal? (first expr) '*)) #t #f))

; Check whether a given expression is a exponentiation
(define (exponentiation? expr)
  (match expr
    [(list '^ _ _) #t]
    [_ #f]))

; Extract a base from a exponentiation
(define (exponentiation-base expr)
  (match expr
    [(list '^ x _) x]))

; Extract a power from a exponentiation
(define (exponentiation-power expr)
  (match expr
    [(list '^ _ x) x]))

; Check whether a given expression is a sin
(define (sin? expr)
  (match expr
    [(list 'sin _) #t]
    [_ #f]))

; Extract an argument from a sin
(define (sin-arg expr)
  (match expr
    [(list 'sin x) x]))

; Check whether a given expression is a cos
(define (cos? expr)
  (match expr
    [(list 'cos _) #t]
    [_ #f]))

; Extract an argument from a cos
(define (cos-arg expr)
  (match expr
    [(list cos x) x]))

; Check whether a given expression is a tan
(define (tan? expr)
  (match expr
    [(list 'tan _) #t]
    [_ #f]))

; Extract an argument from a tan
(define (tan-arg expr)
  (match expr
    [(list tan x) x]))

; Check whether a given expression is a natural logarithm
(define (log? expr)
  (match expr
    [(list 'log _) #t]
    [_ #f]))

; Extract an argument from a natural logarithm
(define (log-arg expr)
  (match expr
    [(list log x) x]))

; Exercises 1.2, 1.6-1.7

; Recursive function derivative that computes a symbolic derivative of a given expression
; with respect to a given variable
(define (derivative expr v)
  (cond
    ; find a derivative for a sum
    [(sum? expr)
     (let ([s1 (summand-1 expr)]
           [s2 (summand-2 expr)])
       (list '+
             (derivative s1 v)
             (derivative s2 v)))]
    
    ; find a derivative for a polyvariadic sum
    [(polyvariadic-sum? expr)
     (let ([args (rest expr)])
       (cons '+
             (map (lambda (x) (derivative x v)) args)))]
    
    ; find a derivative for a product
    [(product? expr)
     (let ([m1 (multiplier-1 expr)]
           [m2 (multiplier-2 expr)])
       (list '+
             (list '*
                   (derivative m1 v)
                   m2)
             (list '*
                   m1
                   (derivative m2 v))))]

    ; find a derivative for a polyvariadic product
    [(polyvariadic-product? expr)
     (let ([args (rest expr)])
       (cons '+
             (map (lambda (x)
                    (cons '*
                          (cons (derivative x v)
                                (remove x args))))
                  args)))]
    
    ; find a derivative for a exponentiation
    [(exponentiation? expr)
     (let ([base (exponentiation-base expr)]
           [power (exponentiation-power expr)])
       (list '*
             (derivative power v)
             (list '*
                   expr
                   (list 'log base))))]
    ; find a derivative for a sin
    [(sin? expr)
     (let ([arg (sin-arg expr)])
       (list '*
             (derivative arg v)
             (list 'cos arg)))]
    
    ; find a derivative for a cos
    [(cos? expr)
     (let ([arg (cos-arg expr)])
       (list '*
             -1
             (list '*
                   (derivative arg v)
                   (list 'sin arg))))]
    
    ; find a derivative for a tan
    [(tan? expr)
     (let ([arg (tan-arg expr)])
       (list '/
             (derivative arg v)
             (list '*
                   (list 'cos arg)
                   (list 'cos arg))))]
    
    ; find a derivative for a natural logarithm
    [(log? expr)
     (let ([arg (log-arg expr)])
       (list '/
             (derivative arg v)
             arg))]
    
    ; find a derivative for a constant
    [(number? expr) 0]
    
    ; find a derivative for a variable
    [(variable? expr) (if (equal? expr v) 1 0)]))

'(Exercise 1.2)
(derivative '(+ 1 x) 'x) ; '(+ 0 1)
(derivative '(* 2 y) 'y) ; '(+ (* 0 y) (* 2 1))
(derivative '(* (+ x y) (+ x (+ x x))) 'x) ; '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))

; Exercises 1.3, 1.6-1.7

; Simplify an expression
(define (simplify expr)
  (define (simplify-at-root expr)
    (match expr
      [(list '+ 0 e) e]
      [(list '+ e 0) e]
      [(list '* 1 e) e]
      [(list '* e 1) e]
      [(list '* 0 e) 0]
      [(list '* e 0) 0]
      [(list '/ 0 e) 0]
      [(list '+ s1 s2)
       #:when (and (number? s1) (number? s2))
       (+ s1 s2)]
      [(list '* m1 m2)
       #:when (and (number? m1) (number? m2))
       (* m1 m2)]
      [(list '^ e 1) e]
      [(list '^ 1 e) 1]
      [(list 'sin 0) 0]
      [(list 'cos 0) 1]
      [(list 'tan 0) 0]
      [(list 'log 'e) 1]
      [_
       (cond
         [(polyvariadic-sum? expr)
          ; Sum all numbers and place the result on the first place
          (let ([simplified-expr
                 (cons '+                          
                       (cons 
                        (apply + 
                               (filter (lambda (x) (number? x)) (rest expr)))
                        (filter (lambda (x) (not (number? x))) (rest expr)))
                       )])
            (cond
              ; If it is a simple sum with two arguments
              [(= (length simplified-expr) 3) (simplify-at-root simplified-expr)]
              ; Remove the number if it is 0
              [(equal? (second simplified-expr) 0)
               (remove 0 simplified-expr)]
              [else simplified-expr]))]
         [(polyvariadic-product? expr)
          ; Multiply all numbers and place the result on the first place
          (let ([simplified-expr
                 (cons '*                          
                       (cons 
                        (apply * (filter (lambda (x) (number? x)) (rest expr)))
                        (filter (lambda (x) (not (number? x))) (rest expr))))])
            (cond
              ; If it is a simple prodcut with two arguments
              [(= (length simplified-expr) 3) (simplify-at-root simplified-expr)]
              ; If the number is 0, then the whole expression will be equal to zero
              [(equal? (second simplified-expr) 0) 0]
              ; Remove the number if it is 1
              [(equal? (second simplified-expr) 1)
               (remove 1 simplified-expr)]
              [else simplified-expr]))]
         [else expr])]))
  (match expr
    [(list '+ s1 s2)
     (simplify-at-root (list '+
                             (simplify s1)
                             (simplify s2)))]
    [(list '* m1 m2)
     (simplify-at-root (list '*
                             (simplify m1)
                             (simplify m2)))]
    [(list '/ d1 d2)
     (simplify-at-root (list '/
                             (simplify d1)
                             (simplify d2)))]
    [(list '^ e1 e2)
     (simplify-at-root (list '^
                             (simplify e1)
                             (simplify e2)))]
    [(list 'sin e)
     (simplify-at-root (list 'sin
                             (simplify e)))]
    [(list 'cos e)
     (simplify-at-root (list 'cos
                             (simplify e)))]
    [(list 'tan e)
     (simplify-at-root (list 'tan
                             (simplify e)))]
    [(list 'log e)
     (simplify-at-root (list 'log
                             (simplify e)))]
    [_
     (cond
       [(polyvariadic-sum? expr)
        (simplify-at-root (cons '+
                                (map (lambda (x) (simplify x)) (rest expr))))]
       [(polyvariadic-product? expr)
        (simplify-at-root (cons '*
                                (map (lambda (x) (simplify x)) (rest expr))))]
       [else expr])]
    ))

'(Exercise 1.3)
(simplify '(+ 0 1)) ;1
(simplify '(+ (* 0 y) (* 2 1))) ;2
(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))) ; '(+ (+ x (+ x x)) (* (+ x y) 3))


; Exercise 1.4

; Simplify any expression down to a polynomial of its variables
(define (normalize expr)

  ; convert a list of variables into expression with given sign
  ; E.g. * (x y z) -> (* (* x y) z)
  (define (list-to-expression operation-sign lst)
    (define (helper lst)
      (cond
        [(= (length lst) 1)
         (first lst)]
        [else
         (list operation-sign (helper (rest lst)) (first lst))]))
    (helper lst))

  ; Sum coefficients of equal terms.
  ; The argument is the list of terms which are supposed to be sumed up
  ; E.g. ( (2 x y) (3 a b) (6 y x) ) -> ( (8 x y) (3 a b) )
  (define (sum-similar-terms lst)

    ; Compare two summation terms. Return #t if theay are equal.
    ; A term consists of variables multiplied with each other
    ; On the first position there must be a coefficient, so its value won't be compared
    ; The order of variables in a term does not matter
    (define (similar-terms? term1 term2)
      ; Check if a list contains an element
      (define (contains? elem lst)
        (ormap (lambda (x) (equal? elem x)) lst))
      
      (and (= (length term1) (length term2))
           (andmap (lambda (x) (contains? x (rest term2)))
                   (rest term1))))

    ; Check if a term is in a given list
    (define (contains-term? term terms-lst)
      (ormap (lambda (x) (similar-terms? term x)) terms-lst))

    ; Add a new term to a list if it is new.
    ; Otherwise sum this term to the one that is already in the list
    (define (combine-similar-terms term terms-lst)

      ; Return a term from a list with it's coefficient value
      ; E.g. (2xy) (ab x 3xy) -> 3xy
      (define (find-term term terms-lst)
        (define (helper term terms-lst)
          (foldl
           (lambda (t current)
             (cond
               [(similar-terms? term t)
                (cons t current)]
               [else current]))
           empty
           terms-lst))  
        (first (helper term terms-lst)))

      ; Remove a term from a list
      (define (remove-term term terms-lst)
        (foldl
         (lambda (t current)
           (cond
             [(similar-terms? term t)
              current]
             [else (cons t current)]))
         empty
         terms-lst))

      ; Sum coefficients of two equal terms and return a result as a new term
      (define (sum-two-similar-terms t1 t2)
        (let ([coef1 (first t1)]
              [coef2 (first t2)]
              [variables (rest t1)])
          (cons (+ coef1 coef2) variables)))

      ; Pop a term from a list, sum it with a given one, add the result to the list
      (let ([t (find-term term terms-lst)]
            [lst-without-term (remove-term term terms-lst)])
        (cons (sum-two-similar-terms t term)
              lst-without-term)))

    ; Get only unique terms from a list. Similar terms will be sumed up
    (define (helper lst unique-terms)
      (cond
        [(empty? lst)
         unique-terms]
        [else
         (let ([term (first lst)]
               [rest-lst (rest lst)])
           (cond
             [(contains-term? term unique-terms)
              (helper rest-lst
                      (combine-similar-terms term unique-terms))]
             [else
              (helper rest-lst
                      (cons term unique-terms))]))]))
    
    (helper lst empty))

  ; Multiply coefficients in a term and place a result to the first place of a list.
  ; Other elements are just variables.
  (define (multiply-coefficients-in-term lst)
    (define (helper lst coef variables)
      (cond
        [(empty? lst)
         (cons coef variables)]
        [else
         (let ([elem (first lst)]
               [rest-lst (rest lst)])
           (cond
             [(number? elem)
              (helper rest-lst
                      (* elem coef)
                      variables)]
             [else
              (helper rest-lst
                      coef
                      (cons elem variables))]))]))
    (helper lst 1 empty))

  ; Convert an expression of products to the list of terms
  ; E.g. (* (* x y) z) -> (x y z)
  (define (multiplications-to-list expr)
    (define (helper expr current)
      (cond
        [(not (product? expr))
         (cons expr current)]
        [else
         (helper (multiplier-1 expr)
                 (cons (multiplier-2 expr)
                       current))]))
    (helper expr empty))

  ; Convert an expression of sums to the list of terms
  ; E.g. (+ (+ x y) z) -> (x y z)
  (define (sums-to-list expr)
    (define (helper expr current)
      (cond
        [(not (sum? expr))
         (cons expr current)]
        [else
         (helper (summand-1 expr)
                 (cons (summand-2 expr) current))]))
    (helper expr empty))

  ; Move paranthesis of terms multiplication, so for each product the second term is always a variable
  ; E.g. (* (* a b) (* c (* d e))) -> (* (* (* (* a b) c) d) e)
  (define (rearrange-multiplications expr)
    (match expr
      [(list '* t1 t2)
       #:when (not (product? t2))
       (list '* (rearrange-multiplications t1) t2)]
      [(list '* t1 t2)
       #:when (product? t2)
       (rearrange-multiplications (list '*
                                        (rearrange-multiplications (list '* t1 (multiplier-1 t2)))
                                        (rearrange-multiplications (multiplier-2 t2))))]
      [_ expr]))

  ; Move paranthesis of terms summation, so for each sum the second term is always a variable
  ; E.g. (+ (+ a b) (+ c (+ d e))) -> (+ (+ (+ (+ a b) c) d) e)
  (define (rearrange-sums expr)
    (match expr
      [(list '+ t1 t2)
       #:when (not (sum? t2))
       (list '+ (rearrange-sums t1) t2)]
      [(list '+ t1 t2)
       #:when (sum? t2)
       (rearrange-sums (list '+
                             (rearrange-sums (list '+ t1 (summand-1 t2)))
                             (rearrange-sums (summand-2 t2))))]
      [_ expr]))

  ; Open parentheses using distributive property of multiplication over addition
  (define (open-parentheses expr)
    (match expr
      [(list '* m1 m2)
       #:when (sum? m1)
       (list '+
             (open-parentheses (list '*
                                     (summand-1 m1)
                                     m2))
             (open-parentheses (list '*
                                     (summand-2 m1)
                                     m2)))]
      [(list '* m1 m2)
       #:when (sum? m2)
       (list '+
             (open-parentheses (list '*
                                     m1
                                     (summand-1 m2)))
             (open-parentheses (list '*
                                     m1
                                     (summand-2 m2))))]
      [(list '+ s1 s2)
       (list '+
             (open-parentheses s1)
             (open-parentheses s2))]
      
      [_ expr]))

  ; Get a polynomial of exprression's variables
  (simplify
   (list-to-expression '+
                       (map
                        (lambda (term) (list-to-expression '* term))
                        (sum-similar-terms
                         (map
                          (lambda (term) (multiply-coefficients-in-term term))    
                          (map
                           (lambda (term) (multiplications-to-list term))
                           (map
                            (lambda (term) (rearrange-multiplications term))     
                            (sums-to-list (rearrange-sums (open-parentheses expr)))))))))))


; Example: for (x + 2z)y + 3xy we should get 4xy + 2yz
'(Exercise 1.4)
(normalize '(+ (* (+ x (* 2 z)) y) (* 3 (* x y)))) ; '(+ (* (* z y) 2) (* (* x y) 4))


; Exercise 1.5

; Convert an expression into an infix form
(define (to-infix expr)
  (cond
    [(sum? expr)
     (list (to-infix (summand-1 expr))
           '+
           (to-infix (summand-2 expr)))]
    [(product? expr)
     (list (to-infix (multiplier-1 expr))
           '*
           (to-infix (multiplier-2 expr)))]
    [else expr]))

'(Exercise 1.5)
(to-infix '(+ (+ x (+ x x)) (* (+ x y) 3))) ; '((x + (x + x)) + ((x + y) * 3))


; Exercise 1.7

'(Exercise 1.7)
(derivative '(+ 1 x y (* x y z)) 'x) ; '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0)))
(simplify '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0)))) ; '(+ 1 (* y z))

; 1.3 Gradient

; Exercise 1.8

; Check the operation sign for an expression with one argument
(define (single-arg-op? sign)
  (ormap (lambda (x) (equal? sign x)) '(sin cos tan log)))

; Check the operation sign for an expression with two arguments
(define (binary-op? sign)
  (ormap (lambda (x) (equal? sign x)) '(+ - * / ^)))

; Returns a list of distinct variables used in a given expression
(define (variables-of expr)
  (define (helper expr current)
    (match expr
      [(list sign t1 t2)
       #:when (binary-op? sign)
       (remove-duplicates (flatten
                           (cons (helper t1 current)
                                 (cons (helper t2 current)
                                       current))))]
      [(list sign e)
       #:when (single-arg-op? sign)
       (remove-duplicates (flatten
                           (cons (helper e current)
                                 current)))]
      [_
       (cond
         [(or (polyvariadic-product? expr) (polyvariadic-sum? expr))
          (remove-duplicates (flatten
                              (foldl
                               (lambda (arg c)
                                 (cons (helper arg c) c))
                               current
                               expr)))]
         [(or (number? expr) (single-arg-op? expr) (binary-op? expr))
          current]
         [else
          (remove-duplicates (flatten(cons expr current)))]
         )]))
  (remove-duplicates (flatten (helper expr empty))))

'(Exercise 1.8)
(variables-of '(+ 1 x y (* x y z))) ; '(x y z)


; Exercise 1.9

; Returns a gradient of a multivariable expression (given explicitly the list of variables)
(define (gradient expr variables)
  (map
   (lambda (v) (simplify (derivative expr v)))
   variables))

'(Exercise 1.9)
(gradient '(+ 1 x y (* x y z)) '(x y z)) ; '((+ 1 (* y z)) (+ 1 (* x z)) (* x y))
