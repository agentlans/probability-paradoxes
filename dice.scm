(use-modules (srfi srfi-1))
(use-modules (srfi srfi-43))

(define (vector-sum freq-vec)
  (apply + (vector->list freq-vec)))

(define (normalize freq-vec)
  "Normalizes frequency to probability."
  (let ((tc (vector-sum freq-vec)))
    (vector-map (lambda (i x) (/ x tc))
		freq-vec)))
;;(normalize '#(0 1 2 0))

(define (p-greater-const prob-vec k)
  "Probability that random variable > constant k."
  (vector-sum (vector-map (lambda (i x)
			    (if (> i k) x 0))
			  prob-vec)))
;; (p-greater-const '#(0 0 1/2 1/2) 2)

(define (p-equal-const prob-vec k)
  "Probability that random variable = constant k."
  (vector-ref prob-vec k))

(define (p-greater prob-vec1 prob-vec2)
  "Probability that random variable 1 > random variable 2"
  (let ((c-max (max (vector-length prob-vec1)
		    (vector-length prob-vec2))))
    (vector-sum (vector-map
		 (lambda (i x)
		   (* (p-greater-const prob-vec1 i)
		      (p-equal-const prob-vec2 i)))
		 (list->vector (iota c-max))))))
;; (p-greater '#(0 1/2 1/2) '#(0 1 0))

(define die-a (normalize '#(0 0 1 0 1 0 0 0 0 1)))
(define die-b (normalize '#(0 1 0 0 0 0 1 0 1 0)))
(define die-c (normalize '#(0 0 0 1 0 1 0 1 0 0)))

;; Probability that each die beats others
(p-greater die-a die-b)
(p-greater die-b die-c)
(p-greater die-c die-a)
