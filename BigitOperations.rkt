(define zero
  (lambda () (list 0)))

(define is-zero?
  (lambda (n)
    (if (list? n)
        (if (and (eq? (length n) 1) (eq? (car n) 0))
            #t
            #f)
         #f)))

(define successor
  (lambda (n)
    (cond
      ((null? n) '())
      ((< (car n) 15) ;less than 15
       (if (eq? (length n) 1)
           (append (list (+ (car n) 1))'())
           (append (list (+ (car n) 1))(cdr n))))
               
           
      ((eq? (car n) 15) ;equal to 15
       (if (eq? (length n) 1)
           (append (list 0 1)(successor (cdr n)))
           (append (list 0)(successor (cdr n)))))
    )))

(define pred-helper
  (lambda (n)
    (cond
      ((null? n) '())
      ((and (= (car n) 1)(eq? (length n) 1)) '())
      ((> (car n) 0) ;greater than 0
       (if (eq? (length n) 1)
           (append (list (- (car n) 1))'())
           (append (list (- (car n) 1))(cdr n))))
      ((eq? (car n) 0) ;equal to 0
       (if (eq? (length n) 1)
           '()
           (append (list 15)(pred-helper (cdr n))))))))
    
(define predecessor
  (lambda (n)
    (if (and (= (car n) 1)(eq? (length n) 1))
        (zero)
        (pred-helper n))))
    
    




