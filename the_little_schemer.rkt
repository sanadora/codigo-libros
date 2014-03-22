;-------------------------------------------------
;           Cosas sobre "The Little Schemer"
;-------------------------------------------------
(define atom?
  	(lambda (x)
	  (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
	(cond 
	  ((null? l) #t)
	  ((atom? (car l)) (lat? (cdr l)))
	  (else #f))))
   ;(or (null? l)
   ;    (and (atom? (car l)) (lat? (cdr l))))
 
(define member?
  (lambda (a lat)
	(cond 
	  ((null? lat) #f)
	  (else (or (equal? a (car lat))
				(member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
	(cond 
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? a (car lat)) (cdr lat))
			  (else (cons (car lat) 
						  (rember a (cdr lat)))))))))

;(multirember 'cut '(coffee cut tea cup and hick cup))
;=> (coffe tea and hick)
(define multirember
  (lambda (a lat)
	(cond
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? a (car lat)) (multirember a (cdr lat)))
			  (else (cons (car lat) 
						  (multirember a (cdr lat)))))))))

; (firsts '((a b c) (gato perro))) -> (a gato)
(define firsts
  (lambda (l)
	(cond 
	  ((null? l) (quote ()))
	  (else (cons (car (car l)) (firsts (cdr l)))))))

;(insertR 'alto 'quiero '(yo quiero rango quiero)) -> (yo quiero alto rango quiero)
(define insertR
  (lambda (new old lat)
	(cond 
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? old (car lat)) 
			   (cons old 
					 (cons new (cdr lat))))
			  (else (cons (car lat) 
						  (insertR new old (cdr lat)))))))))

;(multiinsertR 'secos 'tomates '(tomates y lechugas con tomates))
;=> (tomates secos y lechugas con tomates secos)
(define multiinsertR
  (lambda (new old lat)
	(cond 
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? (car lat) old) 
			   (cons old 
					 (cons new 
						   (multiinsertR new old (cdr lat)))))
			  (else (cons (car lat) 
						  (multiinsertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
	(cond 
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? old (car lat)) 
			   (cons new lat))
			  (else (cons (car lat) 
						  (insertL new old (cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
	(cond
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? old (car lat)) 
			   (cons new 
					 (cons old
						   (multiinsertL new old (cdr lat)))))
			  (else (cons (car lat)
						  (multiinsertL new old (cdr lat)))))))))

;(subst 'pizza 'milanesa '(quiero comer milanesa))
;=> (quiero comer pizza)
(define subst
  (lambda (new old lat)
	(cond
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? old (car lat)) 
			   (cons new (cdr lat)))
			  (else (cons (car lat)	
						  (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
	(cond 
	  ((null? lat) (quote ()))
	  (else (cond
			  ((or (eq? o1 (car lat))    
				   (eq? o2 (car lat))) 
			   (cons new (cdr lat)))
			  (else (cons (car lat)
						  (subst2 new o1 o2 (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
	(cond
	  ((null? lat) (quote ()))
	  (else (cond
			  ((eq? old (car lat))
			   (cons new 
					 (multisubst new old (cdr lat))))
			  (else (cons (car lat) 
						  (multisubst new old (cdr lat)))))))))


(define add1
  (lambda (n)
	(+ n 1)))

(define sub1
  (lambda (n)
	(- n 1)))
 
(define o+
  (lambda (n1 n2)
	(cond 
	  ((zero? n2) n1)
	  (else (o+ (add1 n1)
				(sub1 n2))))))

(define o-
  (lambda (n1 n2)
	(cond
	  ((zero? n2) n1)
	  (else (o- (sub1 n1) 
				(sub1 n2))))))

; es la multiplicacion normal
(define x
  (lambda (n m)
	(cond 
	  ((zero? m) 0)
	  (else (o+ n 
				(x n (sub1 m)))))))

; (addtup '(4 5 10)) => 19
(define addtup
  (lambda (tup)
	(cond
	  ((null? tup) 0)
	  (else (o+ (car tup) 
				(addtup (cdr tup)))))))

; (tup+ '(1 2 3) '(1 2 3)) => (2 4 6)
(define tup+
  (lambda (tup1 tup2)
	(cond
	  ((null? tup1) tup2)
	  ((null? tup2) tup1)
	  (else (cons (o+ (car tup1)
					  (car tup2))
				  (tup+ (cdr tup1) 
						(cdr tup2)))))))

(define >
  (lambda (n m)
	(cond
	  ((zero? n) #f)
	  ((zero? m) #t)
	  (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
	  ((zero? m) #f)
	  ((zero? n) #t)
	  (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((zero? n) (zero? m))
      ((zero? m) #f)
      (else (= (sub1 n) (sub1 m))))))

(define =2
  (lambda (n m)
	(cond
	  ((or (< n m) (> n m)) #f)
	  (else #t))))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (^ n (sub1 m)))))))

(define /
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (/ (o- n m) m))))))

; (length '(a b c d)) => 4
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; (pick 4 '(mi auto esta muy bueno)) => muy
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
 
; (rempick 4 '(mi auto esta muy bueno)) => (mi auto esta bueno)
(define rempick
  (lambda (n lat)
    (cond
      ((one?  n) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

; (no-nums '(mi 1 auto 2 esta)) => (mi auto esta)
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat)
                          (no-nums (cdr lat)))))))))

; (all-nums '(mi 1 auto 2 esta)) => (1 2)
(define all-nums
  (lambda (lat)
     (cond
       ((null? lat) (quote ()))
       (else (cond
               ((number? (car lat)) (cons (car lat)
                  (all-nums (cdr lat))))
               (else (all-nums (cdr lat))))))))

; eqan? es #t si dos argumentos son el mismo atomo
(define eqan?
  (lambda (a1 a2)
	(cond
	  ((and (number? a1) (number? a2))
	   (= a1 a2))
	  ((eq? a1 a2) #t)
	  (else #f))))

; occur cuenta la cantidad de veces que un atomo aparece en una lat
(define occur
  (lambda (a lat)
	(cond 
	  ((null? lat) 0)
	  (else
		(cond 
		  ((eqan? a (car lat))
		   (add1 (occur a (cdr lat))))
		  (else
			(occur a (cdr lat))))))))

; (one? n) es #t si n es 1
(define one?
  (lambda (n)
	(= n 1)))
  
;-------CAPITULO 5---------
;
;"rember-star" borra a en cualquier lista l
(define rember*
  (lambda (a l)
	(cond
	  ((null? l) (quote ()))
	  ((atom? (car l))
	   (cond
		 ((eqan? a (car l)) (rember* a (cdr l)))
		 (else (cons (car l)
				     (rember* a (cdr l))))))
	  (else
		(cons (rember* a (car l))
			  (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
	(cond
	  ((null? l) (quote ()))
	  ((atom? (car l)) 
	   (cond
		 ((eqan? old (car l)) 	
		  (cons (car l)
		 		(cons new
					  (insertR* new old (cdr l)))))
		 (else
		   (cons (car l)
				 (insertR* new old (cdr l))))))
	  (else
		 (cons (insertR* new old (car l))
			   (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
	(cond 
	  ((null? l) 0)
	  ((atom? (car l))
	   (cond
		 ((eqan? a (car l)) 
		  (add1 (occur* a (cdr l))))
		 (else
		   (occur* a (cdr l)))))
	  (else
		(o+ (occur* a (car l))
			(occur* a (cdr l)))))))

; substitute-star
(define subst*
  (lambda (new old l)
	(cond 
	  ((null? l) (quote ()))
	  ((atom? (car l)) 
	   (cond 
		 ((eqan? (car l) old)
		  (cons new
				(subst* new old (cdr l))))
		 (else
		   (cons (car l)
				 (subst* new old (cdr l))))))
	  (else
		(cons (subst* new old (car l))
			  (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
	(cond
	  ((null? l) (quote ()))
	  ((atom? (car l))
	   (cond 
		 ((eqan? old (car l))
		  (cons new
				(cons old
					  (insertL* new old (cdr l)))))
		 (else
		   (cons (car l)
				 (insertL* new old (cdr l))))))
	  (else
		(cons (insertL* new old (car l))
			  (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
	(cond
	  ((null? l) #f)
	  ((atom? (car l))
	   (or
		 (eqan? a (car l)) 
		 (member* a (cdr l))))
	  (else
		(or (member* a (car l))
			(member* a (cdr l)))))))

(define leftmost
  (lambda (l)
	(cond
	  ((atom? (car l)) (car l))
	  (else
		(leftmost (cdr l))))))

;(define eqlist?
  ;(lambda (l1 l2)
	;(cond
	  ;((and (null? l1) (null? l2)) #t)
	  ;((or (null? l1) (null? l2)) #f)
	  ;((and (atom? (car l1)) 
			;(atom? (car l2)))
	   ;(and (eqan? (car l1) (car l2)) 
			;(eqlist? (cdr l1) (cdr l2))))
	  ;((or (atom? (car l1)) (atom? (car l2))) #f)
	  ;(else (and (eqlist? (car l1) (car l2))
				 ;(eqlist? (cdr l1) (cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
	(cond
	  ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  (else (and
			  (equal? (car l1) (car l2))
			  (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
	(cond
	  ((and (atom? s1) (atom? s2)) 
	   (eqan? s1 s2))
	  ((or (atom? s1) (atom? s2)) #f)
	  (else
		(eqlist? s1 s2)))))

; rember elimina una S-expression de la lista l
; redefine la anterior rember (removia a de lat)
(define rember
  (lambda (s l)
	(cond
	  ((null? l) (quote ()))
	  ((equal? s (car l)) (cdr l))
	  (else
		(cons (car l)
			  (rember s (cdr l)))))))

;----------- CHAPTER 6 / SHADOWS ----------- 
(define numbered?
  (lambda (aexp)
	(cond
	  ((atom? aexp) (number? aexp))
	  (else
		(and (numbered? (car aexp))
			 (numbered? (car (cdr (cdr aexp)))))))))
;VER PORQUE NO ANDA 
;;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp))
;      (else
;	    ((car (cdr nexp)) 
;	    (value (car nexp))
;	    (value (car (cdr (cdr nexp))))))))

(define value
  (lambda (nexp)
	(cond
	  ((atom? nexp) nexp)
	  ((eq? (operator nexp) (quote +))
	   (+ (value (1st-sub-exp nexp))
		  (value (2nd-sub-exp nexp))))
	  ((eq? (operator nexp) (quote *))
	   (* (value (1st-sub-exp nexp))
		  (value (2nd-sub-exp nexp))))
	  (else	
		(^ (value (1st-sub-exp nexp))
		   (value (2nd-sub-exp nexp)))))))

(define 1st-sub-exp
  (lambda (aexp)
	(car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
	(car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
	(car aexp)))

(define sero?
  (lambda (n)
	(null? n)))

(define edd1
  (lambda (n)
	(cons (quote ()) n)))

(define zub1
  (lambda (n)
	(cdr n)))

(define p+
  (lambda (n m)
	(cond 
	  ((sero? m) n)
	  (else
		(p+ (edd1 n) (zub1 m))))))

;------------- CHAPTER 7 / FRIENDS AND RELATIONS ---------

(define set?
  (lambda (lat)
	(cond 
	  ((null? lat) #t)
	  ((member? (car lat) (cdr lat)) #f)
	  (else (set? (cdr lat))))))

(define makeset1
  (lambda (lat)
	(cond
	  ((null? lat) (quote ()))
	  (else
		(cond
		  ((member? (car lat) (cdr lat)) 
		   (makeset1 (cdr lat)))
		  (else
			(cons (car lat)
				  (makeset1 (cdr lat)))))))))

(define makeset
  (lambda (lat)
	(cond
	  ((null? lat) (quote ()))
	  (else
		(cons (car lat)
			  (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
	(cond
	  ((null? set1) #t)
	  (else (and (member? (car set1) set2) 
				 (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
	  (and (subset? set1 set2) 
		   (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
	(cond
	  ((null? set1) #f)
	  (else (or (member? (car set1) set2)
				(intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
	(cond
	  ((null? set1) (quote ()))
	  (else
		(cond
		  ((member? (car set1) set2) 
		   (cons (car set1)
				 (intersect (cdr set1) set2)))
		  (else (intersect (cdr set1) set2)))))))

(define union
  (lambda (set1 set2)
	(cond
	  ((null? set1) set2)
	  (else
		(cond
		  ((member? (car set1) set2)
		   (union (cdr set1) set2))
		  (else (cons (car set1) 
					  (union (cdr set1) set2))))))))

(define difference
  (lambda (set1 set2)
	(cond
	  ((null? set1) (quote ()))
	  (else
		(cond
		  ((member? (car set1) set2)
		   (difference (cdr set1) set2))
		  (else (cons (car set1) 
					  (difference (cdr set1) set2))))))))

(define intersectall
  (lambda (l-set)
	(cond
	  ((null? (cdr l-set)) (car l-set))
	  (else
		(intersect (car l-set)
				   (intersectall (cdr l-set)))))))

(define a-pair?1
  (lambda (l) 
	(cond
	  ((or (null? l) 
		   (null? (cdr l))) 
	   #f)
	  (else
		(cond
		  ((null? (cdr (cdr l))) #t)
		  (else #f))))))

(define a-pair?
  (lambda (x)
	(cond
	  ((atom? x) #f)
	  ((null? x) #f)
	  ((null? (cdr x)) #f)
	  ((null? (cdr (cdr x))) #t)
	  (else #f))))

(define first
  (lambda (p)
	(car p)))

(define second
  (lambda (p)
	(car (cdr p))))

(define third
  (lambda (l)
	(car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
	(cons s1
		  (cons s2 (quote ())))))


;RECORDAR:
; "set" es conjunto. Los elementos no se repiten
; "rel" es un conjunto de pares ordenados
(define fun?
  (lambda (rel)
	(set? (firsts rel))))

(define revrel
  (lambda (rel)
	(cond
	  ((null? rel) (quote ()))
	  (else
		(cons (revpair (car rel))
			  (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
	(build (second pair) (first pair))))

; fullfun? ve si la funcion es inyectiva.
; Sucede que tambien es sobreyectiva, a cada elemento del
; espacio de llegada (seconds) le corresponde al menos
; uno del espacio de partida (firsts)
(define fullfun?
  (lambda (fun)
	(fun? (revrel fun))))

;===================================
; CAPITULO 6 - LAMBDA THE ULTIMATE
;===================================
(define rember-fOLD
  (lambda (test? a l)
	(cond
	  ((null? l) (quote ()))
	  (else
		(cond
		  ((test? a (car l)) (cdr l))
		  (else (cons (car l)
					  (rember-fOLD test? a (cdr l)))))))))

(define eq?-c
  (lambda (a)
	(lambda (x)
	  (eq? a x))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
	(lambda (a l)
	  (cond
		((null? l) (quote ()))
		(else (cond
				((test? a (car l)) (cdr l))
				(else (cons (car l))
					  ((rember-f test?) a (cdr l)))))))))
  
(define rember-eq? (rember-f eq?))

(define insertL-f
  (lambda (test?)
	(lambda (new old l)
	  (cond
		((null? l) (quote()))
		(else (cond
				((test? old (car l))
				 (cons new l))
				(else (cons (car l)
							((insertL-f test?) new old (cdr l))))))))))


(define insertR-f
  (lambda (test?)
	(lambda (new old l)
	  (cond
		((null? l) (quote ()))
		(else (cond
			  ((test? old (car l))
			   (cons old
					 (cons new (cdr l))))
			  (else (cons (car l)
						  ((insertR-f test?) new old (cdr l))))))))))

(define seqL
  (lambda (new old l)
	(cons new
		  (cons old l))))

(define seqR
  (lambda (new old l)
	(cons old
		  (cons new l))))

(define insert-g
  (lambda (seq)
	(lambda (new old l)
	  (cond
		((null? l) (quote ()))
		(else (cond
				((eq? old (car l))
				 (seq new old (cdr l)))
				(else
				  (cons (car l)
						((insert-g seq) new old (cdr l))))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(define insertL (insert-g (lambda (new old l)
							(cons new (cons old l)))))

(define seqS 
  (lambda (new old l)
	(cons new l)))

(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l)
	l))

(define yyy
  (lambda (a l)
	((insert-g seqrem) #f a l)))

(define atom-to-function
  (lambda (x)
	(cond
	  ((eq? x (quote +)) +)
	  ((eq? x (quote *)) *)
	  (else ^))))

(define value
  (lambda (nexp)
	(cond
	  ((atom? nexp) nexp)
	  (else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp))
												(value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
	(lambda (a lat)
	  (cond
		((null? lat) (quote ()))
		((test? (car lat) a)
		 ((multirember-f test?) a (cdr lat)))
		(else (cons (car lat)
					((mulitermber-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
  (eq?-c (quote tuna)))

(define multiremberT
  (lambda (test? lat)
	(cond
	  ((null? lat) (quote()))
	  ((func (car lat)) (multiremberT test? (cdr lat)))
	  (else (cons (car lat)
				  (multiremberT test? (cdr lat)))))))


(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember&co a 
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat)
                                           seen)))))
      (else (multirember&co a
                            (cdr lat)
                            (lambda (newlat seen)
                              (col (cons (car lat)
                                         newlat)
                                   seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

; (multiinsertRL 'new 'oldL 'oldR '(a b oldL oldR))
; > (a b new oldL oldR new)
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new 
                                      (cons oldL newlat))
                                (add1 L)
                                R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                       (cons new newlat))
                                 L
                                 (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col (cons (car lat) newlat)
                                     L
                                     R)))))))

(define even?
  (lambda (n)
	(= (* (/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
	(cond
	  ((null? l) (quote ()))
	  ((atom? (car l))
	   (cond 
		 ((and (number? (car l)) (even? (car l)))
		  (cons (car l)
				(evens-only* (cdr l))))
		 (else (evens-only* (cdr l)))))
	  (else (cons (evens-only* (car l))
		      (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col (quote ()) 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only*&co (cdr l)
			(lambda (newl prodevens sumodds)
			  (col (cons (car l)
				     newl)
			       (* (car l)
				  prodevens)
			       sumodds))))
	(else (evens-only*&co (cdr l)
			      (lambda (newl prodevens sumodds)
				(col newl
				     prodevens
				     (+ (car l)
					sumodds)))))))
     (else (evens-only*&co (car l)
			   (lambda (al ap as)
			     (evens-only*&co (cdr l)
					     (lambda (dl dp ds)
					       (col (cons al dl)
						    (* ap dp)
						    (+ as ds))))))))))

(define l '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
	  (cons product
		newl))))
(evens-only*&co l the-last-friend)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 9. ...and Again, and Again, and Again,... ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn) (keep-looking a (pick sorn lat) lat))
     (else (eq? a sorn)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(looking 'caviar '(6 2 grits caviar 5 7 3))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
		  (align (second pora)))))))

(align '(a b))
(align '((a b) c))

;=============================================
;=============================================
(define 
	(sumar-o-restar a b)
	((if (< a b) + -) a b))

(define
	(abs1 a) 
	(cond ((> a 20) (+ a 1))
		  ((and (> a 10) (< a 15)) (* a 2))
		  (else (- a 20))))

(define (fibo-recu a)
	(cond ((= a 0) 0)
		  ((= a 1) 1)
		  (else (+ (fibo-recu (- a 1))
					(fibo-recu (- a 2))))))

(define (fibo-iter a)
  	(define (iteracion x y counter)
	  		(if (< counter 0)
			  	y
	  			(iteracion (+ x y) x (- counter 1))))
	(iteracion 0 1 a))

