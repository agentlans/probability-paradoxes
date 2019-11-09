
(define (has-duplicate lst less)
  "Returns true if and only if there are duplicate elements in lst."
  (let loop ((lst2 (sort lst less)))
    (cond ((< (length lst2) 2) #f)
	  ((equal? (car lst2) (cadr lst2)) #t)
	  (else (loop (cdr lst2))))))

(define (repeat f n)
  "Calls f n times and returns result."
  (do ((i 0 (+ i 1))
       (temp '() (cons (f) temp)))
      ((= i n) temp)))

(define (count lst)
  "Counts number of elements in lst that are true."
  (let loop ((temp 0)
	     (rest lst))
    (if (null? rest) temp
	(loop (+ temp (if (car rest) 1 0))
	      (cdr rest)))))

(define (prob-matching-birthday k)
  "Returns probability that there are two people with same birthday
out of k random people."
  (let ((trials 1000))
    (/ (count
	(repeat (lambda ()
		      (has-duplicate
		       (repeat (lambda ()
				 (random 365)) k) <))
		trials)) trials)))

;; Probability of two people with same birthday out of 23 people
(prob-matching-birthday 23)
