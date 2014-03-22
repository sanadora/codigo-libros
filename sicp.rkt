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
(define (new-if predicate then-clause else-clause)
  (cond
   (predicate then-clause)
   (else else-clause)))

