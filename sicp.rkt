;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.1 The Elements of Programming ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Excercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))

;; Excercise 1.3
(lambda (a b c)
  (cond
   ((or (and (> a b) (> a c) (> c b))
	(and (> c a) (> c b) (> a b)))
    (sum-of-squares a c))
   ((or (and (> b a) (> b c) (> a c))
	(and (> a b) (> a c) (> b c)))
    (sum-of-squares a b))
   ((or (and (> b a) (> b c) (> c a))
	(and (> c a) (> c b) (> b a)))
    (sum-of-squares b c))))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (square a)
  (* a a))

;; Excercise 1.6
;; Loopea infinitamente porque el "if" primitivo es una special-form que usa normal-order evaluation. En cambio, el "new-if", como todo procedimiento normal de lisp, applicative-order evaluation, por lo que evalua sus argumentos antes de aplicar el new-if, entonces evalua infinitamente el (sqrt-iter (improve guess x) x)
(define (new-if predicate then-clause else-clause)
  (cond
   (predicate then-clause)
   (else else-clause)))


;; Excercise 1.7
(define (sqrt-iter guess x prev-guess)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x)
		 x
		 guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
  (< (/ (abs (- guess prev-guess))
	(abs guess))
     tolerance))

(define tolerance 0.001)

(define (sqrt x)
  (sqrt-iter 1.0 x 0.0))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(sqrt 36)

;; Ejercicio 1.8
(define (cubert x)
  (cubert-iter x 1.0 0))

(define (cubert-iter x guess prev-guess)
  (define (improve guess)
    (/ (+ (/ x (square guess))
	  (* 2 guess))
       3))
  (if (good-enough? guess prev-guess)
      guess
      (cubert-iter x
		   (improve guess)
		   guess)))

(cubert 27)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section  1.2 Procedures and the Processes They Generate ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Excercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

;; f(n)=2*n
(define (f n) (A 0 n))

;; g(n)=2^n
(define (g n) (A 1 n))

;; h(n)=
(define (h n) (A 2 n))


;; Excercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(define (f-iterativa n)
  (define (f-iter a b c n)
    (if (= n 0)
	a
	(f-iter (+ a (* 2 b) (* 3 c))
		a
		b
		(- n 1))))
  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 2))))

(f 5)
(f-iterativa 5) 

;; Excercise 1.12
(define (pascal-elem row col))
