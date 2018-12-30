#lang racket/base

;;;;;;;;;;;;;;;;;;
;; Exercise 2.1 ;;
;;;;;;;;;;;;;;;;;;

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g)
	  (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
  
;;;;;;;;;;;;;;;;;;
;; Exercise 2.2 ;;
;;;;;;;;;;;;;;;;;;

(define (make-segment point-a point-b)
  (cons point-a point-b))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (average (x-point end)
			 (x-point start))
		(average (y-point end)
			 (x-point start)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.3 ;;
;;;;;;;;;;;;;;;;;;

(define (make-rect base height)
  (cons base height))

(define (base-rect rect)
  (car rect))

(define (height-rect rect)
  (cdr rect))

(define (perimeter-rect rect)
  (+ (* 2 (base-rect rect))
     (* 2 (height-rect rect))))

(define (area-rect rect)
  (* (base-rect rect)
     (height-rect rect)))


;; second definition of make-rect, base-rect & height-rect

;; (define (square x)
;;   (* x x))

;; (define (make-rect seg-a seg-b seg-c seg-d)
;;   (cons (cons seg-a seg-b)
;; 	(cons seg-c seg-d)))

;; (define (module-segment segment)
;;   (let ((distance-x (x-point (start-segment segment)))
;; 	(distance-y (y-point (start-segment segment)))
;; 	(end-seg (end-segment segment)))
;;     (let ((translated (make-segment (make-point 0 0)
;; 				    (make-point (- (x-point end-seg) distance-x)
;; 						(- (y-point end-seg) distance-y)))))
;;       (sqrt (+ (square (y-point (end-segment translated)))
;; 	       (square (x-point (end-segment translated))))))))

;; (define (base-rect rect)
;;   (let ((base-seg (car (car rect))))
;;     (module-segment base-seg)))

;; (define (height-rect rect)
;;   (let ((height-seg (cdr (car rect))))
;;     (module-segment height-seg)))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.4 ;;
;;;;;;;;;;;;;;;;;;

;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car x)
;;   (x (lambda (n d) n)))

;; (define (cdr x)
;;   (x (lambda (n d) d)))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.5 ;;
;;;;;;;;;;;;;;;;;;

;; (define (cons a b)
;;   (* (expt 2 a)
;;      (expt 3 b)))

;; (define (logn n x)
;;   (/ (log x)
;;      (log n)))

;; (define (car x)
;;   (if (= 0 (remainder x 3))
;;       (car (/ x 3))
;;       (logn 2 x)))

;; (define (cdr x)
;;   (if (= 0 (remainder x 2))
;;       (cdr (/ x 2))
;;       (logn 3 x)))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.6 ;;
;;;;;;;;;;;;;;;;;;

(define (inc x)
  (+ x 1))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; (define (add n m)
;;   (lambda (f) (lambda (x) ((n f) ((m f) x)))))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.7 ;;
;;;;;;;;;;;;;;;;;;
  
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (make-interval x y)
  (cons x y))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

;;;;;;;;;;;;;;;;;;
;; Exercise 2.8 ;;
;;;;;;;;;;;;;;;;;;

;; (define (sub-interval x y)
;;   (make-interval (- (lower-bound x) (upper-bound y))
;; 		 (- (upper-bound x) (lower-bound y))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
				 (- (lower-bound y)))))
;;;;;;;;;;;;;;;;;;
;; Exercise 2.9 ;;
;;;;;;;;;;;;;;;;;;

(define (width-interval x)
  (/ (abs (- (lower-bound x) (upper-bound x)))
     2))


;; racket@sicp2.rkt> (define a (make-interval 3 5))
;; racket@sicp2.rkt> (define b (make-interval -5 7))
;; racket@sicp2.rkt> (= (width-interval (add-interval a b)) (+ (width-interval a) (width-interval b)))
;; #t
;; racket@sicp2.rkt> (= (width-interval (sub-interval a b)) (+ (width-interval a) (width-interval b)))
;; #t

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.10 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (div-interval x y)
;;   (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
;;       (error "Cant divide by an interval that spans zero.")
;;       (mul-interval x 
;; 		    (make-interval (/ 1.0 (upper-bound y))
;; 				   (/ 1.0 (lower-bound y))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.11 ;;
;;;;;;;;;;;;;;;;;;;


;; racket@sicp2.rkt> (define both-pos-int (make-interval 2 4))
;; racket@sicp2.rkt> (define pos-pos-int (make-interval 2 4))
;; racket@sicp2.rkt> (define neg-pos-int (make-interval -2 4))
;; racket@sicp2.rkt> (define neg-neg-int (make-interval -4 -2))
;; racket@sicp2.rkt> (mul-interval pos-pos-int pos-pos-int)
;; '(4 . 16)
;; racket@sicp2.rkt> (mul-interval pos-pos-int neg-pos-int)
;; '(-8 . 16)
;; racket@sicp2.rkt> (mul-interval pos-pos-int neg-neg-int) 
;; '(-16 . -4)
;; racket@sicp2.rkt> (mul-interval neg-pos-int pos-pos-int)
;; '(-8 . 16)
;; racket@sicp2.rkt> (mul-interval neg-pos-int neg-pos-int)
;; '(-8 . 16)
;; racket@sicp2.rkt> (mul-interval neg-pos-int neg-neg-int)
;; '(-16 . 8)
;; racket@sicp2.rkt> (mul-interval neg-neg-int pos-pos-int)
;; '(-16 . -4)
;; racket@sicp2.rkt> (mul-interval neg-neg-int neg-pos-int)
;; '(-16 . 8)
;; racket@sicp2.rkt> (mul-interval neg-neg-int neg-neg-int)
;; '(4 . 16)

;; (define (mul-interval x y)
;;   (let ((a (lower-bound x))
;; 	(b (upper-bound x))
;; 	(c (lower-bound y))
;; 	(d (upper-bound y)))
;;     (cond
;;      ((and (and (positive? a) (positive? b)) (and (positive? c) (positive? d)))
;;       (make-interval (* a c) (* b d)))
;;      ((and (and (positive? a) (positive? b)) (and (negative? c) (positive? d)))
;;       (make-interval (* b c) (* b d)))
;;      ((and (and (positive? a) (positive? b)) (and (negative? c) (negative? d)))
;;       (make-interval (* b c) (* a d)))
;;      ((and (and (negative? a) (positive? b)) (and (positive? c) (positive? d)))
;;       (make-interval (* a d) (* b d)))
;;      ((and (and (negative? a) (positive? b)) (and (negative? c) (positive? d)))
;;       (let ((p (* a c))
;; 	    (q (* a d))
;; 	    (r (* b c))
;; 	    (s (* b d)))
;; 	(make-interval (min p q r s) (max p q r s))))
;;      ((and (and (negative? a) (positive? b)) (and (negative? c) (negative? d)))
;;       (make-interval (* b c) (* a c)))
;;      ((and (and (negative? a) (negative? b)) (and (positive? c) (positive? d)))
;;       (make-interval (* a d) (* b c)))
;;      ((and (and (negative? a) (negative? b)) (and (negative? c) (positive? d)))
;;       (make-interval (* a d) (* a c)))
;;      ((and (and (negative? a) (negative? b)) (and (negative? c) (negative? d)))
;;       (make-interval (* b d) (* a c))))))


;; some extra definitions from the book (page 95)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.12 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-center-percent c p)
  (let ((width (abs (* c (/ p 100)))))
    (make-center-width c width)))

(define (percent i)
  (abs (/ (* 100 (width i))
	  (center i))))


;; some extra definitions from the book (page 96)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FALTAN 13, 14, 15 & 16 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.17 ;;
;;;;;;;;;;;;;;;;;;;

(define (last-pair l)
  (if (= 1 (length l))
      l
      (last-pair (cdr l))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.18 ;;
;;;;;;;;;;;;;;;;;;;

(define (reverse l)
  (cond ((null? l) l)
	((= 1 (length l)) l)
	(else (append (reverse (cdr l)) (list (car l))))))

(define (reverse-iter l)
  (define (iter list res)
    (if (null? list)
	res
	(iter (cdr list) (cons (car list) res))))
  (iter l null))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.19 ;;
;;;;;;;;;;;;;;;;;;;

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount
		     (except-first-denomination coin-values))
		 (cc (- amount
			(fist-denomination coin-values))
		     coin-values)))))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coins)
  (= 0 (length coins)))

(define (fist-denomination kinds-of-coins)
  (car kinds-of-coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;;(define us-coins (list 1 5 10 25 50))
(define (count-change amount)
  (cc amount us-coins))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.20 ;;
;;;;;;;;;;;;;;;;;;;

(define (same-parity first . rest)
  (define (iter items res)
    (if (= 0 (length items))
	res
	(let ((head (car items)))
	  (if (even? (+ head first))
	      (iter (cdr items) (append res (list (car items))))
	      (iter (cdr items) res)))))
  (iter (cons first rest) null))
	  

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.21 ;;
;;;;;;;;;;;;;;;;;;;

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
	    (square-list (cdr items)))))

;; (define (square-list items)
;;   (map square items))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.22 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;; 	answer
;; 	(iter (cdr things)
;; 	      (append answer (list (square (car things)))))))
;;   (iter items null))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.23 ;;
;;;;;;;;;;;;;;;;;;;

(define (for-each f items)
  (map f items)
  #t)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.24 ;;
;;;;;;;;;;;;;;;;;;;

;; hecho en papel :)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.25 ;;
;;;;;;;;;;;;;;;;;;;

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

(car (car (list (list 7))))

(cadr (cadr (cadr (cadr (cadr (cadr (list 1
					  (list 2
						(list 3
						      (list 4
							    (list 5
								  (list 6 7))))))))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.26 ;;
;;;;;;;;;;;;;;;;;;;

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;; '(1 2 3 4 5 6)
(cons x y) ;; '((1 2 3) 4 5 6)
(list x y) ;; '((1 2 3) (4 5 6))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.27 ;;
;;;;;;;;;;;;;;;;;;;

(define (deep-reverse l)
  (cond ((null? l) l)
	((pair? l) (append (deep-reverse (cdr l))
			   (list (deep-reverse (car l)))))
	(else l)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.28 ;;
;;;;;;;;;;;;;;;;;;;

(define (fringe tree)
  (cond ((null? tree) tree)
	((pair? (car tree))
	 (append (fringe (car tree)) (fringe (cdr tree))))
	(else (cons (car tree) (fringe (cdr tree))))))

(define (fring tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (list tree))
	(else (append (fring (car tree))
		      (fring (cdr tree))))))
	 
	      

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.29 ;;
;;;;;;;;;;;;;;;;;;;

;; Part 1

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; (define (left-branch mobile)
;;   (car mobile))

;; (define (right-branch mobile)
;;   (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))
 
;; Part 2

(define (total-weight mobile)
  (define (structure-weight structure)
    (if (not (pair? structure))
	structure
	(+ (structure-weight (branch-structure (left-branch structure)))
	   (structure-weight (branch-structure (right-branch structure))))))
  (structure-weight mobile))

;; Part 3

(define (balanced? mobile)
  (define (torque branch)
    (* (total-weight (branch-structure branch))
       (branch-length branch)))
  (define (structure-balanced? structure)
    (if (not (pair? structure))
	#t
	(let ((left (left-branch structure))
	      (right (right-branch structure)))
	  (and (structure-balanced? (branch-structure left))
	       (structure-balanced? (branch-structure right))
	       (= (torque left) (torque right))))))
  (structure-balanced? mobile))

;; Part 4

;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch leng structure)
;;   (cons length structure))

;; (define (left-branch mobile)
;;   (car mobile))
;; (define (right-branch mobile)
;;   (cdr mobile))

;; (define (branch-length branch)
;;   (car branch))
;; (define (branch-structure branch)
;;   (cdr branch))
  

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.30 ;;
;;;;;;;;;;;;;;;;;;;

(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (square subtree)))
       tree))

;; (define (square-tree tree)
;;   (cond ((null? tree) null)
;; 	((pair? tree) (cons (square-tree (car tree))
;; 			    (square-tree (cdr tree))))
;; 	(else (square tree))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.31 ;;
;;;;;;;;;;;;;;;;;;;
	 
(define (tree-map f tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map f subtree)
	     (f subtree)))
       tree))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.32 ;;
;;;;;;;;;;;;;;;;;;;
	 
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subset)
			    (append subset (list (car s))))
			  rest)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.33 ;;
;;;;;;;;;;;;;;;;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;; (define (map p sequence)
;;   (accumulate (lambda (x y)
;; 		(cons (p x) y))
;; 	      null
;; 	      sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons seq2 seq1))

;; (define (length sequence)
;;   (accumulate (lambda (x y)
;; 		(+ 1 y))
;; 	      0
;; 	      sequence))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.34 ;;
;;;;;;;;;;;;;;;;;;;

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* higher-terms x)
		   this-coeff))
	      0
	      coefficient-sequence))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.35 ;;
;;;;;;;;;;;;;;;;;;;

(define (count-leaves t)
  (accumulate + 0 (map (lambda (subtree)
			 (if (pair? subtree)
			     (count-leaves subtree)
			     1))
		       t)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.36 ;;
;;;;;;;;;;;;;;;;;;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.37 ;;
;;;;;;;;;;;;;;;;;;;

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))


(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.38 ;;
;;;;;;;;;;;;;;;;;;;

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;; op should be commutative

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.39 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (reverse sequence)
;;   (fold-right (lambda (x y)
;; 		(append y (list x)))
;; 	      null
;; 	      sequence))

;; (define (reverse sequence)
;;   (fold-left (lambda (x y)
;; 	       (cons y x))
;; 	     null
;; 	     sequence))
	      
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.40 ;;
;;;;;;;;;;;;;;;;;;;

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;; generates the sequence of pairs (i,j) with 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair)
	(cadr pair)
	(+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

(define (smallest-divisor-next n)
  (define (next n)
    (if (= n 2)
	3
	(+ n 2)))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))  
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor-next n) n))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.41 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-triplets n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k)
			       (list i j k))
			     (enumerate-interval 1 n)))
		      (enumerate-interval 1 n)))
	   (enumerate-interval 1 n)))

(define (filter-dif-numbers triplets)
  (filter (lambda (t)
	    (not (or (= (car t) (cadr t))
		     (= (car t) (caddr t))
		     (= (cadr t) (caddr t)))))
	  triplets))

(define (triplet-sum triplet)
  (accumulate + 0 triplet))
		    
(define (triplets-sum n s)
  (filter (lambda (t)
	    (= (triplet-sum t) s))
	  (filter-dif-numbers (make-triplets n))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.42 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (flatmap mapper seq)
;;   (accumulate append null (map mapper seq)))

;; (define (enumerate-interval start end)
;;   (if (> start end)
;;       null
;;       (cons start (enumerate-interval (+ start 1) end))))

(define empty-board null)

(define (adjoin-position row col positions)
  (append positions (list (list row col))))

(define (pos-row position)
  (car position))

(define (pos-col position)
  (cadr position))

(define (upper-left-diagonal pos)
  (if (or (= (pos-row pos) 1) (= (pos-col pos) 1))
      null
      (let ((diagonal (list (- (pos-row pos) 1) (- (pos-col pos) 1))))
	(append (upper-left-diagonal diagonal) (list diagonal)))))

(define (lower-left-diagonal pos)
  (if (= (pos-col pos) 1)
      null
      (let ((diagonal (list (+ (pos-row pos) 1) (- (pos-col pos) 1))))
	(append (lower-left-diagonal diagonal) (list diagonal)))))
;; determines, for a set of positions, wether the queen
;; in the kth column is safe with respect to the others
(define (safe? col queens)
  (let ((last-queen-pos (car (filter (lambda (pos) (= (pos-col pos) col))
				     queens))))
    (and (= (length (filter (lambda (pos)
			     (= (pos-row pos) (pos-row last-queen-pos)))
			   queens))
	   1)
	(= (length (filter (lambda (pos)
			     (= (pos-col pos) (pos-col last-queen-pos)))
			   queens))
	   1)
	(let ((diagonals (append (upper-left-diagonal last-queen-pos)
				 (lower-left-diagonal last-queen-pos))))
	  (let ((diag-queens-intersection (flatmap (lambda (diagonal)
						     (filter (lambda (previous-queen)
							       (and (= (pos-row diagonal) (pos-row previous-queen))
								    (= (pos-col diagonal) (pos-col previous-queen))))
							     queens))
						   diagonals)))
	    (= (length diag-queens-intersection) 0))))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (positions) (safe? k positions))
		(flatmap (lambda (rest-of-queens)
			   (map (lambda (new-row)
				  (adjoin-position new-row k rest-of-queens))
				(enumerate-interval 1 board-size)))
			 (queen-cols (- k 1))))))
  (queen-cols board-size))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.43 ;;
;;;;;;;;;;;;;;;;;;;
			   
;; If the order of mappings is inverted, (queen-cols (- k 1)) is re-evaluated
;; for (length (enumerate-interval 1 board-size)) times. If the puzzle is solved
;; in time T (4.42), then with this change it should take approx T * (board-size ^ board-size)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.2.4 Example: A Picture Language ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define wave2 (beside wave (flip-vert wave)))
;; (define wave4 (below wave2 wave2))

(define beside '())
(define flip-vert '())
(define draw-line '())
(define frame-coord-map '())
(define transform-map '())
(define transform-painter '())

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;; (define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter
		(below smaller smaller)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.44 ;;
;;;;;;;;;;;;;;;;;;;

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter
	       (beside smaller smaller)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.45 ;;
;;;;;;;;;;;;;;;;;;;

(define (split main second)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split main second) painter (- n 1))))
	  (main painter
		(second smaller smaller))))))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.46 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect va vb)
  (make-vect (+ (xcor-vect va)
		(xcor-vect vb))
	     (+ (ycor-vect va)
		(ycor-vect vb))))

(define (sub-vect va vb)
  (add-vect va (make-vect (- (xcor-vect vb))
			    (- (ycor-vect vb)))))

(define (scale-vect v factor)
  (make-vect (* (xcor-vect v) factor)
	     (* (ycor-vect v) factor)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.47 ;;
;;;;;;;;;;;;;;;;;;;
  
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))
;; (define (origin-frame f)
;;   (car f))
;; (define (edge1-frame f)
;;   (cadr f))
;; (define (edge2-frame f)
;;   (cddr f))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.48 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (make-segment start end)
;;   (cons start end))

;; (define (start-segment seg)
;;   (car seg))
;; (define (end-segment seg)
;;   (cdr seg))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.49 ;;
;;;;;;;;;;;;;;;;;;;

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
		(draw-line
		 ((frame-coord-map frame) (start-segment segment))
		 ((frame-coord-map frame) (end-segment segment))))
	      segment-list)))

(segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
			 (make-segment (make-vect 1 0) (make-vect 1 1))
			 (make-segment (make-vect 1 1) (make-vect 0 1))
			 (make-segment (make-vect 0 1) (make-vect 0 0))))

(segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
			 (make-segment (make-vect 0 1) (make-vect 1 0))))

(segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
			 (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
			 (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
			 (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.50 ;;
;;;;;;;;;;;;;;;;;;;

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0)
		     (make-vect 0 0)
		     (make-vect 1.0 1.0)))

(define (rot-counter-180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0 1.0)
		     (make-vect 1.0 0)))

(define (rot-counter-270 painter)
  (transform-painter painter
		     (make-vect 0 1.0)
		     (make-vect 0 0)
		     (make-vect 1.0 1.0)))
				
		     
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.51 ;;
;;;;;;;;;;;;;;;;;;;

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below (transform-painter painter1
					  (make-vect 0.0 0.0)
					  (make-vect 1.0 0.0)
					  split-point))
	  (paint-above (transform-painter painter2
					  split-point
					  (add-vect split-point (make-vect 0.5 0.0))
					  (add-vect split-point (make-vect 0.0 0.5)))))
      (lambda (frame)
	(paint-below frame)
	(paint-above frame)))))

;; (define (below painter1 painter2)
;;   (rot-counter-270
;;    (rot-counter-180
;;     (beside (rot-counter-270 painter1)
;; 	    (rot-counter-270 painter2)))))


;;;;;;;;;;;;;;;;;;;
;; Exercise 2.53 ;;
;;;;;;;;;;;;;;;;;;;

(list 'a 'b 'c) ;; (a b c)
(list (list 'george)) ;; ((george))
(cdr '((x1 x2) (y1 y2))) ;; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;; (y1 y2)
(pair? (car '(a short lst))) ;; false
(memq 'red '((red shoes) (blue socks))) ;; false
(memq 'red '(red shoes blue socks)) ;; (red shoes blue socks)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.54 ;;
;;;;;;;;;;;;;;;;;;;

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (pair? a) (pair? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	(else #f)))
  

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.55 ;;
;;;;;;;;;;;;;;;;;;;

(car ''abracadabra) ;; (car (quote (quote abracadabra)))
;; quote

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.56 ;;
;;;;;;;;;;;;;;;;;;;

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (multiplicand exp)
				 (deriv (multiplier exp) var))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else (error "unknown expression type: DERIV" exp))))
	     
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
	((=number? m2 1) m1)
	((or (=number? m1 0) (=number? m2 0)) 0)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))
(define (addend s) (cadr s))
(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? '* (car x))))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (caddr p))

(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (car exp))))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	(else (list '** base exponent))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.57 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (augend s)
;;   (cond ((= 2 (length (cdr s))) (caddr s))
;; 	(else (cons '+ (cddr s)))))

;; (define (multiplicand p)
;;   (cond ((= 2 (length (cdr p))) (caddr p))
;; 	(else (cons '* (cddr p)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.58 ;;
;;;;;;;;;;;;;;;;;;;

;; part a)
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;; 	((=number? a2 0) a1)
;; 	((and (number? a1) (number? a2)) (+ a1 a2))
;; 	(else (list a1 '+ a2))))

;; (define (make-product m1 m2)
;;   (cond ((=number? m1 1) m2)
;; 	((=number? m2 1) m1)
;; 	((or (=number? m1 0) (=number? m2 0)) 0)
;; 	((and (number? m1) (number? m2)) (* m1 m2))
;; 	(else (list m1 '* m2))))

;; (define (sum? x)
;;   (and (pair? x) (eq? '+ (cadr x))))
;; (define (addend s) (car s))
;; (define (augend s) (caddr s))

;; (define (product? x)
;;   (and (pair? x) (eq? '* (cadr x))))
;; (define (multiplier p) (car p))
;; (define (multiplicand p) (caddr p))

;; part b)

;; (x + 3 * (x + y + 2))
;; (define (sum? x)
;;   (and (pair? x)
;;        (> (length (filter (lambda (e) (eq? e '+)) x))
;; 	  0)))
;; (define (addend s)
;;   (define (iter addend rest)
;;     (if (eq? (car rest) '+)
;; 	(if (= (length addend) 1)
;; 	    (car addend)
;; 	    addend)
;; 	(iter (append addend (list (car rest)))
;; 	      (cdr rest))))
;;   (iter '() s))
;; (define (augend s)
;;   (let ((aug (cdr (memq '+ s))))
;;     (if (= 1 (length aug))
;; 	(car aug)
;; 	aug)))
  
;; (define (product? x)
;;   (and (pair? x)
;;        (let ((operators (filter symbol? x)))
;; 	 (= (length operators)
;; 	    (length (filter (lambda (e)
;; 			      (eq? e '*))
;; 			    x))))))
;; (define (multiplier p)
;;   (define (iter multip rest)
;;     (if (eq? (car rest) '*)
;; 	(if (= (length multip) 1)
;; 	    (car multip)
;; 	    multip)
;; 	(iter (append multip (list (car rest)))
;; 	      (cdr rest))))
;;   (iter '() p))
;; (define (multiplicand p)
;;   (let ((multip (cdr (memq '* p))))
;;     (if (= 1 (length multip))
;; 	(car multip)
;; 	multip)))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets as unordered lists ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

;;O(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; O(n^2)
(define (intersection-set set1 set2)
  (cond ((null? set1) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;;;;;;;;;;;;;;;;;;;;
;; Excercise 2.59 ;;
;;;;;;;;;;;;;;;;;;;;

;; O(n^2)
;; (define (union-set set1 set2)
;;   (cond ((null? set1) set2)
;; 	(else (adjoin-set (car set1)
;; 			  (union-set (cdr set1) set2)))))

;; (define (union-set set1 set2)
;;   (accumulate adjoin-set set2 set1))
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.60 ;;
;;;;;;;;;;;;;;;;;;;

;; O(1)
;; (define (adjoin-set x set)
;;   (cons x set))

;; (define (union-set set1 set2)
;;   (append set1 set2))

;; el resto son iguales

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets as ordered lists ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; S={1,2,3,4} => '(1 2 3 4)
;; O(n)
;; (define (element-of-set? x set)
;;   (cond ((null? set) #t)
;; 	((< x (car set)) #f)
;; 	((= x (car set)) #t)
;; 	(else (element-of-set? x (cdr set)))))

;; O(n)
;; each step removes one element of set1, set2 or both
;; at most, if n1=(size set1) && n2=(size set2)
;; n1+n2 steps are needed ( O(n) ), instead of n1*n2 ( O(n^2) )
;; as with unordered lists
;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2)) '())
;; 	(else (let ((x1 (car set1))
;; 		    (x2 (car set2)))
;; 		(cond ((< x1 x2)
;; 		       (intersection-set (cdr set1) set2))
;; 		      ((< x2 x1)
;; 		       (intersection-set set1 (cdr set2)))
;; 		      (else
;; 		       (cons x1 (intersection-set (cdr set1)
;; 						  (cdr set2)))))))))
  
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.61 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (adjoin-set x set)
;;   (cond ((null? set) (list x))
;; 	((< x (car set)) (cons x set))
;; 	((= x (car set)) set)
;; 	(else (cons (car set) (adjoin-set x (cdr set))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.62 ;;
;;;;;;;;;;;;;;;;;;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      ((= x1 x2)
		       (cons x1 (union-set (cdr set1) (cdr set2))))
		      (else ;; (> x1 x2)
		       (cons x2 (union-set set1 (cdr set2)))))))))
					   
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets as binary trees ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (entry tree) (car tree))

;; (define (left-branch tree) (cadr tree))

;; (define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

;; abstraction barrier :)

;; (define (element-of-set? x set)
;;   (cond ((null? set) #f)
;; 	((= x (entry set)) #t)
;; 	((< x (entry set)) (element-of-set? x (left-branch set)))
;; 	(else (element-of-set? x (right-branch set)))))

;; (define (adjoin-set x set)
;;   (cond ((null? set) (make-tree x '() '()))
;; 	((= x (entry set)) set)
;; 	((< x (entry set))
;; 	 (make-tree (entry set)
;; 		    (adjoin-set x (left-branch set))
;; 		    (right-branch set)))
;; 	(else
;; 	 (make-tree (entry set)
;; 		    (left-branch set)
;; 		    (adjoin-set x (right-branch set))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.63 ;;
;;;;;;;;;;;;;;;;;;;

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (list (entry tree))
	      (tree->list-1 (right-branch tree)))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;; a) yes, both procedures generate the same answer
;; b) tree->list-1: T(n) = 2*T(n/2) + O(n/2) (por append)
;;                  T(n) = O(n * log n)
;;    tree->list-2: T(n) = 2*T(n/2) + O(1) (por cons)
;;                  T(n) = O(n)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.64 ;;
;;;;;;;;;;;;;;;;;;;

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-branch (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-branch (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-branch right-branch)
		      remaining-elts))))))))
		  
;; a)
;; partial-tree takes a number N and a list of elements of at
;; least N elements ELTS.
;; it computes the number of elements corresponding to each halve
;; of the tree (each branch), and recursively calls itself to
;; produce the partial trees corresponding to those elements.
;; each time a subtree is made, the first item of the list is taken

;; b) T(n) = 2*T(n/2) + O(1) (por cons) = O(n)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.65 ;;
;;;;;;;;;;;;;;;;;;;

(define (union-tree tree-1 tree-2)
  (let ((list-1 (tree->list-2 tree-1))
	(list-2 (tree->list-2 tree-2)))
    (list->tree (union-set list-1 list-2))))
;;            tree->list-2   union-set (ordered list)  list->tree
;; T(n) = 2 *    O(n)      +     O(n)                +   O(n)
;; T(n) = O(n)

(define (intersection-tree tree-1 tree-2)
  (let ((list-1 (tree->list-2 tree-1))
	(list-2 (tree->list-2 tree-2)))
    (list->tree (intersection-set list-1 list-2))))
	
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.66 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (lookup given-key set-of-records)
;;   (cond ((null? set-of-records) #f)
;; 	((= given-key (key (entry set-of-records)))
;; 	 (entry set-of-records))
;; 	((< given-key (key (entry set-of-records)))
;; 	 (lookup given-key (left-branch set-of-records)))
;; 	(else
;; 	 (lookup given-key (right-branch set-of-records)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.4 Example: Huffman Encoding Trees ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bat bit: CHOOSE-BRANCH" bit))))

(define (huffman-adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (huffman-adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(huffman-adjoin-set (make-leaf (car pair)  ;; symbol 
				       (cadr pair)) ;; frequency
			    (make-leaf-set (cdr pairs))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.67 ;;
;;;;;;;;;;;;;;;;;;;

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
  
;; '(A D A B B C A)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.68 ;;
;;;;;;;;;;;;;;;;;;;

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;; inter. len.
;; O(n) + O(n) = O(n) (inter. es O(n) porque uno de los sets tiene solo el simbolo)
(define (symbol-of-branch? symbol branch)
  (= 1 (length (intersection-set (list symbol)
				 (symbols branch)))))

(define (encode-symbol s tree)
  (cond ((null? tree) (error "null tree"))
	((leaf? tree) '())
	((symbol-of-branch? s (left-branch tree))
	 (cons 0 (encode-symbol s (left-branch tree))))
	((symbol-of-branch? s (right-branch tree))
	 (cons 1 (encode-symbol s (right-branch tree))))
	(else (error "symbol not in any branch"))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.69 ;;
;;;;;;;;;;;;;;;;;;;
  
(define (generate-huffman-tree pairs)
  (succesive-merge (make-leaf-set pairs)))

(define (succesive-merge branches)
  (if (= (length branches) 1)
      (car branches)
      (succesive-merge (huffman-adjoin-set
			(make-code-tree (car branches)
					(cadr branches))
			(cddr branches)))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.70 ;;
;;;;;;;;;;;;;;;;;;;

(let ((song-tree (generate-huffman-tree
		  '((A 2) (NA 16)
		    (BOOM 1) (SHA 3)
		    (GET 2) (YIP 9)
		    (JOB 2) (WAH 1))))
      (song '((GET A JOB SHA)
	      (SHA NA NA NA NA NA NA NA NA)
   	      (GET A JOB SHA)
	      (SHA NA NA NA NA NA NA NA NA)
	      (WAH YIP YIP YIP YIP)
	      (YIP YIP YIP YIP YIP)
	      (SHA BOOM))))
  (length (flatmap (lambda (phrase)
	 (encode phrase song-tree))
       song)))

;; 92 bits with the huffman encoding
(* 3 36) ;; 108 with a fixed length encoding

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.71 ;;
;;;;;;;;;;;;;;;;;;;

;; in such a tree there is only needed 1 bit to encode the
;; most frequent symbol, but n-1 to encode the least frequent.

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.72 ;;
;;;;;;;;;;;;;;;;;;;

;; T(n) = O(n) + T(n-1)
;; best case: O(1) (porque va a buscar un simbolo en un set de 1 simbolo)
;; worst case: O(n^2) (n veces va a buscar un simbolo en un set de n-1,2,... elementos

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.73 ;;
;;;;;;;;;;;;;;;;;;;

;; 1) We can't assimilate the predicates number? and variable? into the
;; data-directed dispatch because it operates on non-tagged data

;; 2) & 3)
;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;; 	((variable? exp) (if (same-variable? exp var) 1 0))
;; 	(else ((get 'deriv (operator exp)) (operands exp) var))))

;; los defino para poder entrar al modulo y evaluar en racket
(define put '())
(define get '())

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  ;; internal procedures
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  (define (deriv-prod exp var)
    (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
	      (make-product (deriv (multiplier exp) var) (multiplicand exp))))
  (define (deriv-exp exp var)
    (make-product
     (make-product (exponent exp)
		   (make-exponentiation (base exp)
					(- (exponent exp) 1)))
     (deriv (base exp) var)))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod)
  (put 'deriv '** deriv-prod)
  'done)

;; 4) The only changes needed would be to change the order of the arguments to "put"
;; when we install the handlers.

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.74 ;;
;;;;;;;;;;;;;;;;;;;

(define division 1)
(define contents 1)

(define (get-record employee-key personnel-file)
  ((get 'get-record (division personnel-file)) employee-key (contents personnel-file)))
;; each division's file should be tagged with a division's descriptor, like 'TEXAS,
;; that will be retrieved by (division file)

(define (get-salary employee-key personnel-file)
  (let ((record (get-record employee-key personnel-file)))
    ((get 'employee-salary (division personnel-file)) (contents record))))
;; the record should also be tagged with a division's descriptor

(define (find-employee-record employee-key files)
  (if (null? files)
      #f
      (or (get-record employee-key (car files))
	  (find-employee-record employee-key (cdr files)))))

;; when insatiable takes over a new company, the new personnel file and
;; the way to read that must be installed on the system.

;; | operation / company | TEXAS                 | MIAMI                 | NYC                 |
;; |---------------------+-----------------------+-----------------------+---------------------|
;; | get-record          | get-record-texas      | get-record-miami      | get-record-nyc      |
;; | employee-salary     | employee-salary-texas | employee-salary-miami | employee-salary-nyc |

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.75 ;;
;;;;;;;;;;;;;;;;;;;

(define (make-from-mag-ang r a)
  (lambda (message)
    (cond ((eq? message 'magnitude) r)
	  ((eq? message 'angle) a)
	  ((eq? message 'real-part)
	   (* r (cos a)))
	  ((eq? message 'imag-part)
	   (* r (sin a)))
	  (else (error "Unknown message -- MAKE-FROM-MAG-ANG" message)))))
	   
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.76 ;;
;;;;;;;;;;;;;;;;;;;

;; generic operations with explicit dispatch:
;; to add new types we must install the functions keeping in mind to don't generate name-colissions.
;; then, for each dispatcher function we must add a clause to the new type.
;; to add a new operation we must create a new dispatcher function to call the new functions that will
;; implement the operation on each type.

;; data-directed style:
;; new type: install the package corresponding to the new type (no name collision risk)
;; by adding the operations to the dispatch table
;; new operation: re-install every package with the new operation.

;; message-passing:
;; new type: generate a new function that responds all the operations for that type.
;; new operation: for each type-function, add the new operation and re-install it.

;; best for a system with frequent new types: message-passing or data-directed
;; best for a system with frequent new operations: message-passing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Systems with generic operations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (negative x) (apply-generic 'negative x))

;; (define (attach-tag type-tag contents)
;;   (cons type-tag contents))
;; (define (type-tag datum)
;;   (if (pair? datum)
;;       (car datum)
;;       (error "Bad tagged datum -- TYPE-TAG" datum)))
;; (define (content datum)
;;   (if (pair? datum)
;;       (cdr datum)
;;       (error "Bad tagged datum -- TYPE-TAG" datum)))

(define dispatch-table (make-hash))

(define (put-dispatch op type item)
  (hash-set! dispatch-table (list op type) item))
(define (get-dispatch op type)
  (hash-ref dispatch-table (list op type) #f))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get-dispatch op type-tags)))
      (if proc
	  (apply proc (map content args))
	  (error "No method for these types -- APPLY-GENERIC"
		 (list op type-tags))))))


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put-dispatch 'add '(scheme-number scheme-number)
		(lambda (x y) (tag (+ x y))))
  (put-dispatch 'sub '(scheme-number scheme-number)
		(lambda (x y) (tag (- x y))))
  (put-dispatch 'mul '(scheme-number scheme-number)
		(lambda (x y) (tag (* x y))))
  (put-dispatch 'div '(scheme-number scheme-number)
		(lambda (x y) (tag (/ x y))))
  (put-dispatch 'equ? '(scheme-number scheme-number)
		(lambda (x y) (= x y)))
  (put-dispatch '=zero? '(scheme-number)
		(lambda (x) (= x 0)))
  (put-dispatch 'make 'scheme-number
		(lambda (x) (tag x)))
  (put-dispatch 'negative '(scheme-number)
		(lambda (x) (- x)))
  (put-dispatch 'reduce '(scheme-number scheme-number)
		(lambda (n d)
		  (let ((g (gcd n d)))
		    (list (/ n g) (/ d g)))))
  (put-dispatch 'greatest-common-divisor '(scheme-number) gcd)
  'done)

(define (make-scheme-number n)
  ((get-dispatch 'make 'scheme-number) n))


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g ((if (> d 0) + -) (gcd n d))))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  (define (equ? r1 r2)
    (and (= (numer r1) (numer r2))
	 (= (denom r1) (denom r2))))
  (define (=zero? r)
    (= (numer r) 0))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put-dispatch 'add '(rational rational)
  		(lambda (x y) (tag (add-rat x y))))
  (put-dispatch 'sub '(rational rational)
  		(lambda (x y) (tag (sub-rat x y))))
  (put-dispatch 'mul '(rational rational)
  		(lambda (x y) (tag (mul-rat x y))))
  (put-dispatch 'div '(rational rational)
  		(lambda (x y) (tag (div-rat x y))))
  (put-dispatch 'equ? '(rational rational) equ?)
  (put-dispatch '=zero? '(rational) =zero?)
  (put-dispatch 'make 'rational
  		(lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get-dispatch 'make 'rational) n d))

;; complex rectangular package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (= (real-part z) 0)
	 (= (imag-part z) 0)))

  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put-dispatch 'real-part '(rectangular) real-part)
  (put-dispatch 'imag-part '(rectangular) imag-part)
  (put-dispatch 'magnitude '(rectangular) magnitude)
  (put-dispatch 'angle '(rectangular) angle)
  (put-dispatch 'equ? '(rectangular rectangular) equ?)
  (put-dispatch 'equ? '(rectangular rectangular) =zero?)
  (put-dispatch 'make-from-real-imag 'rectangular
		(lambda (x y)
		  (tag (make-from-real-imag x y))))
  (put-dispatch 'make-from-mag-ang 'rectangular
		(lambda (r a)
		  (tag (make-from-mag-ang r a))))
  'done)

;; complex polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  (define (equ? z1 z2)
    (and (= (magnitude z1) (magnitude z2))
	 (= (angle z1) (angle z2))))
  (define (=zero? z)
    (= (magnitude z) 0))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put-dispatch 'real-part '(polar) real-part)
  (put-dispatch 'imag-part '(polar) imag-part)
  (put-dispatch 'magnitude '(polar) magnitude)
  (put-dispatch 'angle '(polar) angle)
  (put-dispatch 'equ? '(polar polar) equ?)
  (put-dispatch '=zero? '(polar) =zero?)
  (put-dispatch 'make-from-real-imag 'polar
		(lambda (x y)
		  (tag (make-from-real-imag x y))))
  (put-dispatch 'make-from-mag-ang 'polar
		(lambda (r a)
		  (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get-dispatch 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get-dispatch 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put-dispatch 'add '(complex complex)
		(lambda (z1 z2)
		  (tag (add-complex z1 z2))))
  (put-dispatch 'sub '(complex complex)
		(lambda (z1 z2)
		  (tag (sub-complex z1 z2))))
  (put-dispatch 'mul '(complex complex)
		(lambda (z1 z2)
		  (tag (mul-complex z1 z2))))
  (put-dispatch 'div '(complex complex)
		(lambda (z1 z2)
		  (tag (div-complex z1 z2))))
  (put-dispatch 'equ? '(complex complex) equ?)
  (put-dispatch '=zero? '(complex) =zero?)
  (put-dispatch 'make-from-real-imag 'complex
		(lambda (x y)
		  (tag (make-from-real-imag x y))))
  (put-dispatch 'make-from-mag-ang 'complex
		(lambda (r a)
		  (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get-dispatch 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get-dispatch 'make-from-mag-ang 'complex) r a))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.77 ;;
;;;;;;;;;;;;;;;;;;;

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(put-dispatch 'real-part '(complex) real-part)
(put-dispatch 'imag-part '(complex) imag-part)
(put-dispatch 'magnitude '(complex) magnitude)
(put-dispatch 'angle '(complex) angle)

;; call trace
;; (magnitude (make-complex-from-real-imag 1 2))
;; (apply-generic 'magnitude '(complex rectangular 1 . 2))
;; ((get-dispatch 'magnitude '(complex)) '(rectangular 1 . 2))
;; (magnitude '(rectangular 1 . 2))
;; (apply-generic 'magnitude '(rectangular 1 . 2))
;; ((get-dispatch 'magnitude '(rectangular)) (cons 1 2))
;; ((lambda (z)
;;    (sqrt (+ (square (real-part z))
;; 	    (square (imag-part z)))))
;;  (cons 1 2))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.78 ;;
;;;;;;;;;;;;;;;;;;;

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(error "Bad tagged datum -- TYPE-TAG" datum)))

(define (content datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(error "Bad tagged datum -- TYPE-TAG" datum)))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.79 ;;
;;;;;;;;;;;;;;;;;;;

;; already added to the packages

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.80 ;;
;;;;;;;;;;;;;;;;;;;

;; already added to the packages

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.81 ;;
;;;;;;;;;;;;;;;;;;;

(define coercion-table (make-hash))

(define (put-coercion from to item)
  (hash-set! coercion-table (list from to) item))
(define (get-coercion from to)
  (hash-ref coercion-table (list from to) #f))

;; 1) If we call exp with two complex numbers as arguments
;; there will be an endless call to apply-generic after
;; coercing the types to themselves
		
;; 2) apply-generic works correctly as it is

;; 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (apply-generic op . args)						 ;;
;;   (let ((type-tags (map type-tag args)))					 ;;
;;     (let ((proc (get-dispatch op type-tags)))				 ;;
;;       (if proc								 ;;
;; 	  (apply proc (map content args))					 ;;
;; 	  (if (= (length args) 2)						 ;;
;; 	      (let ((type1 (car type-tags))					 ;;
;; 		    (type2 (cadr type-tags))					 ;;
;; 		    (a1 (car args))						 ;;
;; 		    (a2 (cadr args)))						 ;;
;; 		(if (eq? type1 type2)						 ;;
;; 		    (let ((t1->t2 (get-coercion type1 type2))			 ;;
;; 			  (t2->t1 (get-coercion type2 type1)))			 ;;
;; 		      (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))		 ;;
;; 			    (t2->t1 (apply-generic op a1 (t2->t1 a2)))		 ;;
;; 			    (else (error "No method for these types"		 ;;
;; 					 (list op type-tags)))))		 ;;
;; 		    (error "No method for these types"				 ;;
;; 			   (list op type-tags))))				 ;;
;; 	      (error "No method for these types"				 ;;
;; 		     (list op type-tags)))))))					 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;;;;;;;;;;;;;;;;;;;
;; Exercise 2.82 ;;
;;;;;;;;;;;;;;;;;;;

(define (all-same-types types)
  (define (same-of type rest)
    (cond ((null? rest) #t)
	  ((eq? (car rest) type)
	   (same-of type (cdr rest)))
	  (else #f)))
  (cond ((null? types) #t)
	(same-of (car types) (cdr types))))

(define (types-combinations types)
  ;; (sn rat com) => ((sn rat->sn com->sn) (sn->rat rat com->rat) (sn->com rat->com com))
  (map (lambda (first-type)
	 (map (lambda (other-type)
		(if (eq? first-type other-type)
		    (list first-type)
		    (list other-type first-type)))
	      types))
       types))

(define (coersions types)
  (map (lambda (types-row)
	 (map (lambda (types-cell)
		(if (= 1 (length types-cell))
		    (lambda (x) x)
		    (get-coercion (car types-cell)
				  (cadr types-cell))))
	      types-row))
       (types-combinations types)))

(define (applicable-conversions types)
  (filter (lambda (types-row)
	    (not (memq #f types-row)))
	  (coersions types)))

(define (apply-generic2 op . args)
  (let ((type-tags (map type-tag args)))
    (if (all-same-types type-tags)
	(let ((proc (get-dispatch op type-tags)))
	  (if proc
	      (apply proc (map content args))
	      (error "asdfasdf")))
	(let ((coercions (applicable-conversions type-tags)))
	  (if (null? coercions)
	      (error "asdf")
	      (car (map (lambda (coercions-row)
			  (apply apply-generic (cons op (map (lambda (coercion arg)
							       (coercion arg))
							     coercions-row
							     args))))
			coercions)))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.83 ;;
;;;;;;;;;;;;;;;;;;;

(define types-tower (make-hash))
(define (put-type . args)
  (let ((subtype (car args))
	(supertype (cdr args)))
    (cond ((null? supertype)
	   (hash-set! types-tower subtype 10))
	  (else
	   (hash-set! types-tower
		      subtype
		      (+ 1 (hash-ref types-tower supertype)))))))
(define (get-type-level type)
  (hash-ref types-tower type))

(define (install-int-package)
  (define (raise-int x)
    (make-rational x 1))
  (put-type 'int 'rat)
  (put-dispatch 'raise '(int) raise-int))

(define (raise x)
  (apply-generic 'raise x))

;; (define (raise-rat r)
;;   (make-real (/ (numer x) (denom x))))

;; (define (raise-real r)
;;   (make-complex-from-real-imag r 0))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.84 ;;
;;;;;;;;;;;;;;;;;;;

(define (apply-generic3 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get-dispatch op type-tags)))
      (cond (proc (apply proc (map content args)))
	    ((not (all-same-types type-tags))
	     (apply apply-generic (cons op (raise-lower args))))
	    (else (error "asdfasdf"))))))

(define (raise-lower args)
  (let ((levels (map (lambda (arg)
		       (get-type-level (type-tag arg)))
		     args)))
    (let ((higher-level (foldl (lambda (x y) (if (> x y) x y)) 0 levels)))
      (map (lambda (arg level)
	     (if (< level higher-level)
		 (raise arg)
		 arg))
	   args
	   levels))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.85 ;;
;;;;;;;;;;;;;;;;;;;

;; (define (project-complex z)
;;   (make-real (real-part z)))
;; (define (project-real r)
;;   (make-rat (numerator r) (denominator r)))
;; (define (project-rational r)
;;   (make-int (round (/ (numer r) (denom r)))))

;; (define (drop x)
;;   (if (equ? x (raise (project x)))
;;       (drop (project x))
;;       x))

;; (define (apply-generic4 op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get-dispatch op type-tags)))
;;       (cond (proc (apply proc (map content args)))
;; 	    ((not (all-same-types type-tags))
;; 	     (apply apply-generic (cons op (raise-lower args))))
;; 	    (else (error "asdfasdf"))))))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.86 ;;
;;;;;;;;;;;;;;;;;;;

(define (install-complex-package2)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get-dispatch 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get-dispatch 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
			 (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
			 (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
		       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
		       (sub (angle z1) (angle z2))))

  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put-dispatch 'add '(complex complex)
		(lambda (z1 z2)
		  (tag (add-complex z1 z2))))
  (put-dispatch 'sub '(complex complex)
		(lambda (z1 z2)
		  (tag (sub-complex z1 z2))))
  (put-dispatch 'mul '(complex complex)
		(lambda (z1 z2)
		  (tag (mul-complex z1 z2))))
  (put-dispatch 'div '(complex complex)
		(lambda (z1 z2)
		  (tag (div-complex z1 z2))))
  (put-dispatch 'equ? '(complex complex) equ?)
  (put-dispatch '=zero? '(complex) =zero?)
  (put-dispatch 'make-from-real-imag 'complex
		(lambda (x y)
		  (tag (make-from-real-imag x y))))
  (put-dispatch 'make-from-mag-ang 'complex
		(lambda (r a)
		  (tag (make-from-mag-ang r a))))
  'done)

;; complex polar package
;; (define (install-polar-package2)
;;   ;; internal procedures
;;   (define (magnitude z) (car z))
;;   (define (angle z) (cdr z))
;;   (define (make-from-mag-ang r a) (cons r a))
;;   (define (real-part z)
;;     (mul (magnitude z) (cosine (angle z))))
;;   (define (imag-part z)
;;     (mul (magnitude z) (sine (angle z))))
;;   (define (make-from-real-imag x y)
;;     (cons (squareroot (+ (square2 x) (square2 y)))
;; 	  (arctangent y x)))
;;   (define (equ? z1 z2)
;;     (and (equ? (magnitude z1) (magnitude z2))
;; 	 (equ? (angle z1) (angle z2))))
;;   (define (=zero? z)
;;     (equ? (magnitude z) 0))
  
;;   ;; interface to the rest of the system
;;   (define (tag x) (attach-tag 'polar x))
;;   (put-dispatch 'real-part '(polar) real-part)
;;   (put-dispatch 'imag-part '(polar) imag-part)
;;   (put-dispatch 'magnitude '(polar) magnitude)
;;   (put-dispatch 'angle '(polar) angle)
;;   (put-dispatch 'equ? '(polar polar) equ?)
;;   (put-dispatch '=zero? '(polar) =zero?)
;;   (put-dispatch 'make-from-real-imag 'polar
;; 		(lambda (x y)
;; 		  (tag (make-from-real-imag x y))))
;;   (put-dispatch 'make-from-mag-ang 'polar
;; 		(lambda (r a)
;; 		  (tag (make-from-mag-ang r a))))
;;   'done)

(define (cosine r)
  (apply-generic 'cosine r))
(define (sine r)
  (apply-generic 'sine r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.5.3 Example: Symbolic Algebra ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1))
		 (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
		   ((> (order t2) (order t1))
		    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term (make-term (order t1)
					    (add (coeff t1) (coeff t2)))
				 (add-terms (rest-terms L1)
					    (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  ;; (define (adjoin-term term term-list)
  ;;   (if (=zero? (coeff term))
  ;; 	term-list
  ;; 	(cons term term-list)))
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term))
	   term-list)
	  ((> (order term) (length term-list))
	   (adjoin-term term (cons 0 term-list)))
	  (else
	   (cons (coeff term) term-list))))
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
	       (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (the-empty-termlist) '())
  ;; (define (first-term term-list) (car term-list))
  ;; (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term (make-term (+ (order t1) (order t2))
				  (mul (coeff t1) (coeff t2)))
		       (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  ;; exercise 2.87
  (define (=zero?-poly p)
    (empty-termlist? (term-list p)))
  ;; exercise 2.88
  (define (negative-terms terms)
    (if (empty-termlist? terms)
	(the-empty-termlist)
	(let ((first (first-term terms)))
	  (adjoin-term (make-term (order first)
				  (negative (coeff first)))
		       (negative-terms (rest-terms terms))))))
  (define (negative-poly p)
    (make-poly
     (variable p)
     (negative-terms (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negative-poly p2)))

  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put-dispatch 'add '(polynomial polynomial)
		(lambda (p1 p2) (tag (add-poly p1 p2))))
  (put-dispatch 'mul '(polynomial polynomial)
		(lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put-dispatch 'make 'polynomial
		(lambda (var terms) (tag (make-poly var terms))))
  (put-dispatch '=zero? '(polynomial) =zero?-poly)
  (put-dispatch 'negative '(polynomial)
		(lambda (p) (tag (negative-poly p))))
  (put-dispatch 'sub '(polynomial polynomial)
		(lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)


(define (make-polynomial var terms)
  ((get-dispatch 'make 'polynomial) var terms))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.87 ;;
;;;;;;;;;;;;;;;;;;;

;; added =zero? to install-polynomial-package

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.88 ;;
;;;;;;;;;;;;;;;;;;;

;; added negative and sub

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.89 ;;
;;;;;;;;;;;;;;;;;;;

;; added adjoin-term, first-term & rest-terms

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.90 ;;
;;;;;;;;;;;;;;;;;;;

(define (install-term-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; interface to the system
  (define (tag x) (attach-tag 'term x))
  (put-dispatch 'order '(term) order)
  (put-dispatch 'coeff '(term) coeff)
  (put-dispatch 'make 'term
		(lambda (order coeff)
		  (tag (make-term order coeff))))
  'done)

(define (make-term order coeff)
  ((get-dispatch 'make 'term) order coeff))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))

;; ((100 5) (3 1) (2 10))
(define (install-sparse-terms-package)
  (define (term-order term)
    ((get-dispatch 'order '(term)) term))
  (define (term-coeff term)
    ((get-dispatch 'coeff '(term)) term))
  (define (adjoin-term-sparse term term-list)
    (if (=zero? (term-coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (let ((first (car term-list)))
      (make-term (term-order first)
		 (term-coeff first))))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  ;; interface to the rest of the system
  (define (tag t) (attach-tag 'sparse t))
  (put-dispatch 'adjoin-term '(term sparse)
		(lambda (term term-list)
		  (tag (adjoin-term-sparse term term-list))))
  (put-dispatch 'first-term '(sparse) first-term)
  (put-dispatch 'rest-terms '(sparse)
		(lambda (term-list)
		  (tag (rest-terms term-list))))
  (put-dispatch 'empty-termlist? '(sparse) empty-termlist?)
  (put-dispatch 'the-empty-termlist 'sparse
		(lambda () (tag (the-empty-termlist))))
  'done)

(define (the-empty-sparse-termlist)
  ((get-dispatch 'the-empty-termlist 'sparse)))

;; (5 2 0 2 3 1)
(define (install-dense-terms-package)
  (define (term-order term)
    ((get-dispatch 'order '(term)) term))
  (define (term-coeff term)
    ((get-dispatch 'coeff '(term)) term))
  (define (adjoin-term-dense term term-list)
    (cond ((=zero? (term-order term))
	   term-list)
	  ((> (term-order term) (length term-list))
	   (adjoin-term-dense term (cons 0 term-list)))
	  (else
	   (cons (term-coeff term) term-list))))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
	       (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  ;; interface to the rest of the system
  (define (tag t) (attach-tag 'dense t))
  (put-dispatch 'adjoin-term '(term dense)
		(lambda (term term-list)
		  (tag (adjoin-term-dense term term-list))))
  (put-dispatch 'first-term '(dense) first-term)
  (put-dispatch 'rest-terms '(dense)
		(lambda (term-list)
		  (tag (rest-terms term-list))))
  (put-dispatch 'empty-termlist? '(dense) empty-termlist?)
  (put-dispatch 'the-empty-termlist 'dense
		(lambda () (tag (the-empty-termlist))))
  'done)

(define (the-empty-dense-termlist)
  ((get-dispatch 'the-empty-termlist 'dense)))

(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term term term-list))
(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list)
  (apply-generic 'empty-termlist? term-list))


(define (install-polynomial-package2)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1))
		 (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
		   ((> (order t2) (order t1))
		    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term (make-term (order t1)
					    (add (coeff t1) (coeff t2)))
				 (add-terms (rest-terms L1)
					    (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	;; ver de retornar una the-empty-X-termlist de acuerdo
	;; a los argumentos en lugar de forzar una sparse
	(the-empty-sparse-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	;; idem arriba
	(the-empty-sparse-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term (make-term (+ (order t1) (order t2))
				  (mul (coeff t1) (coeff t2)))
		       (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  ;; exercise 2.87
  (define (=zero?-poly p)
    (empty-termlist? (term-list p)))
  ;; exercise 2.88
  (define (negative-terms terms)
    (if (empty-termlist? terms)
	;; idem mas arriba (mul-terms)
	(the-empty-sparse-termlist)
	(let ((first (first-term terms)))
	  (adjoin-term (make-term (order first)
				  (negative (coeff first)))
		       (negative-terms (rest-terms terms))))))
  (define (negative-poly p)
    (make-poly
     (variable p)
     (negative-terms (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negative-poly p2)))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((division-termlists (div-terms (term-list p1)
					     (term-list p2))))
	  (list (make-poly (variable p1)
			   (car division-termlists))
		(make-poly (variable p1)
			   (cadr division-termlists))))
	(error "Polys not in same var -- DIV-POLY"
	       (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
	(list (the-empty-sparse-termlist)
	      (the-empty-sparse-termlist))
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order t2) (order t1))
	      (list (the-empty-sparse-termlist) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1) (order t2))))
		(let ((rest-of-result
		       (div-terms
			(add-terms
			 L1
			 (negative-terms
			  (mul-terms (adjoin-term
				      (make-term new-o new-c)
				      (the-empty-sparse-termlist))
				     L2)))
			L2)))
		  (list (adjoin-term (make-term new-o new-c)
				     (car rest-of-result))
			(cadr rest-of-result))))))))

  (define (gcd-poly a b)
    (if (same-variable? (variable a) (variable b))
	(make-poly (variable a)
		   (gcd-terms (term-list a) (term-list b)))
	(error "Polys not in the save var -- GCD POLY"
	       (list a b))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
	(simplify-terms a)
	(gcd-terms b (pseudoremainder-terms a b))))
  (define (gcd-term-coeffs terms)
    (cond ((= (length terms) 2) (coeff (first-term terms)))
	  (else (gcd (coeff (first-term terms))
		     (gcd-term-coeffs (rest-terms terms))))))
  (define (simplify-terms terms)
    (let ((gcd (gcd-term-coeffs terms)))
      (mul-term-by-all-terms (make-term 0 (/ 1 gcd))
			     terms)))
  ;; (define (remainder-terms a b)
  ;;   (cadr (div-terms a b)))
  (define (pseudoremainder-terms a b)
    (let ((o1 (order (first-term a)))
	  (o2 (order (first-term b)))
	  (c (coeff (first-term b))))
      (let ((integerizing-factor (expt c (+ 1 (- o1 o2)))))
	(let ((pseudo-a
	       (mul-term-by-all-terms (make-term 0 integerizing-factor) a)))
	  (cadr (div-terms pseudo-a b))))))
  (define (reduce-terms n d)
    (let ((gcd (gcd-terms n d))
	  (order-numer (order (first-term n)))
	  (order-denom (order (first-term d))))
      (let ((o1 (max order-numer order-denom))
	    (o2 (order (first-term gcd)))
	    (c (coeff (first-term gcd))))
	(let ((integerizing-factor (expt c (+ 1 (- o1 o2)))))
	  (let ((scaled-numer
		 (mul-term-by-all-terms
		  (make-term 0 integerizing-factor)
		  n))
		(scaled-denom
		 (mul-term-by-all-terms
		  (make-term 0 integerizing-factor)
		  d)))
	    (list
	     (div-terms scaled-numer gcd)
	     (div-terms scaled-denom gcd)))))))
  (define (reduce-poly n d)
    (if (same-variable? (variable n) (variable d))
	(let ((reduced-terms
	       (reduce-terms (term-list n)
			     (term-list d))))
	  (list (make-poly (variable n) (caar reduced-terms))
		(make-poly (variable n) (caadr reduced-terms))))
	(error "Polys not in the save var -- REDUCE POLY"
	       (list n d))))
  
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put-dispatch 'add '(polynomial polynomial)
		(lambda (p1 p2) (tag (add-poly p1 p2))))
  (put-dispatch 'mul '(polynomial polynomial)
		(lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put-dispatch 'make 'polynomial
		(lambda (var terms) (tag (make-poly var terms))))
  (put-dispatch '=zero? '(polynomial) =zero?-poly)
  (put-dispatch 'negative '(polynomial)
		(lambda (p) (tag (negative-poly p))))
  (put-dispatch 'sub '(polynomial polynomial)
		(lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put-dispatch 'div '(polynomial polynomial)
		(lambda (p1 p2)
		  (let ((divided-polys
			 (div-poly p1 p2)))
		    (list (tag (car divided-polys))
			  (tag (cadr divided-polys))))))
  (put-dispatch 'greatest-common-divisor '(polynomial polynomial)
		(lambda (p1 p2)
		  (tag (gcd-poly p1 p2))))
  (put-dispatch 'reduce '(polynomial polynomial)
		(lambda (p1 p2)
		  (let ((reduced-polys
			 (reduce-poly p1 p2)))
		    (list (tag (car reduced-polys))
			  (tag (cadr reduced-polys))))))
  'done)

;; x^5 - 1
;; ------- = x^3 + x, remainder x - 1
;; x^2 - 1

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.91 ;;
;;;;;;;;;;;;;;;;;;;

;; added to (install-polynomial-package2)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.92 ;;
;;;;;;;;;;;;;;;;;;;

;; TODO

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.93 ;;
;;;;;;;;;;;;;;;;;;;

(define (install-rational-package-generic)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons (car reduced) (cadr reduced))))
  ;; (define (make-rat n d)
  ;;   (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
	      (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
	      (mul (denom x) (numer y))))
  (define (equ-rat? r1 r2)
    (and (equ? (numer r1) (numer r2))
	 (equ? (denom r1) (denom r2))))
  (define (=zero-rat? r)
    (=zero? (numer r)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put-dispatch 'add '(rational rational)
  		(lambda (x y) (tag (add-rat x y))))
  (put-dispatch 'sub '(rational rational)
  		(lambda (x y) (tag (sub-rat x y))))
  (put-dispatch 'mul '(rational rational)
  		(lambda (x y) (tag (mul-rat x y))))
  (put-dispatch 'div '(rational rational)
  		(lambda (x y) (tag (div-rat x y))))
  (put-dispatch 'equ? '(rational rational) equ-rat?)
  (put-dispatch '=zero? '(rational) =zero-rat?)
  (put-dispatch 'make 'rational
  		(lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-packages)
  (install-scheme-number-package)
  (install-term-package)
  (install-dense-terms-package)
  (install-sparse-terms-package)
  (install-polynomial-package2)
  (install-rational-package-generic)
  'done-packages)

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.94 ;;
;;;;;;;;;;;;;;;;;;;

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.95 ;;
;;;;;;;;;;;;;;;;;;;

;; El problema surge porque en el calculo de la division
;; aparecen numeros racionales como coeficientes

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.96 ;;
;;;;;;;;;;;;;;;;;;;

;; done :)
;; resuelto en el mismo package de polys

;;;;;;;;;;;;;;;;;;;
;; Exercise 2.97 ;;
;;;;;;;;;;;;;;;;;;;

(define (reduce n d)
  (apply-generic 'reduce n d))

(install-packages)

(define p1 (make-polynomial
	    'x
	    (adjoin-term
	     (make-term 1 1)
	     (adjoin-term
	      (make-term 0 1)
	      (the-empty-sparse-termlist)))))
(define p2 (make-polynomial
	    'x
	    (adjoin-term
	     (make-term 3 1)
	     (adjoin-term
	      (make-term 0 -1)
	      (the-empty-sparse-termlist)))))
(define p3 (make-polynomial
	    'x
	    (adjoin-term
	     (make-term 1 1)
	     (the-empty-sparse-termlist))))
(define p4 (make-polynomial
	    'x
	    (adjoin-term
	     (make-term 2 1)
	     (adjoin-term
	      (make-term 0 -1)
	      (the-empty-sparse-termlist)))))


