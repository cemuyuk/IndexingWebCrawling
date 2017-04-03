(require racket/unsafe/ops)  	  	     	 
  	  	     	 
(define (set-car! p x)  	  	     	 
  (if (pair? p)  	  	     	 
      (unsafe-set-mcar! p x)  	  	     	 
      (raise-type-error 'set-car! "pair" p)))
  	  	     	 
(define (set-cdr! p x)  	  	     	 
  (if (pair? p)  	  	     	 
      (unsafe-set-mcdr! p x)  	  	     	 
      (raise-type-error 'set-cdr! "pair" p)))
  	  	     	 
(define (append! a b)  	  	     	 
  (if (null? (cdr a))  	  	     	 
      (set-cdr! a b)  	  	     	 
      (append! (cdr a) b)))  	  	     	 
  	  	     	 
(define (write-line x)  	  	     	 
  (write x)  	  	     	 
  (newline)  	  	     	 
  )  	  	     	 
  	  	     	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the merge-sort function is provided for your use for exercise 9
;; this function, gets a list and a comparing function ( in this case
;; you should use (symbol<? x y)) and returns the sorted list.
;; you can also write your own sorting procedure if you wish.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  	  	     	 
					; sorting a list
(define (split ls)  	  	     	 
  (letrec ([split-h (lambda (ls ls1 ls2)
		      (cond  	  	     	 
		       [(or (null? ls) (null? (cdr ls)))
			(cons (reverse ls2) ls1)]
		       [else (split-h (cddr ls)
				      (cdr ls1) (cons (car ls1) ls2))]))])
    (split-h ls ls '())))  	  	     	 
  	  	     	 
(define (merge pred ls1 ls2)  	  	     	 
  (cond  	  	     	 
   [(null? ls1) ls2]  	  	     	 
   [(null? ls2) ls1]  	  	     	 
   [(pred (car (car  ls1)) (car (car ls2)))
    (cons (car ls1) (merge pred (cdr ls1) ls2))]
   [else (cons (car ls2) (merge pred ls1 (cdr ls2)))]))
  	  	     	 
(define (merge-sort pred ls)  	  	     	 
  (cond  	  	     	 
   [(null? ls) ls]  	  	     	 
   [(null? (cdr ls)) ls]  	  	     	 
   [else (let ([splits (split ls)])
	   (merge pred  	  	     	 
		  (merge-sort pred (car splits))
		  (merge-sort pred (cdr splits))))]))
  	  	     	 
