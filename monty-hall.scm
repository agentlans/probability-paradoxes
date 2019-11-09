(define (start-game)
  "Returns an instance of the Monty Hall problem which the player can choose
the door to open and the decision to keep or switch doors.
Returns true if player wins otherwise false."
  (lambda (door decision)
    (let* ((prize (random 3))
	   ;; Open the door that's not the prize and not the one the player opened
	   (opened-door
	    (car (filter (lambda (d)
			   (not (member d (list door prize))))
			 '(0 1 2)))))
      ;; See whether player wins
      (cond ((equal? decision 'keep)
	     (equal? prize door))
	    ((equal? decision 'switch)
	     (and (not (equal? prize door))
		  (not (equal? prize opened-door))))
	    (else 'dont-understand)))))

(define (count-wins f n)
  "Executes procedure f n number of times and
counts how many times (f) is true."
  (let loop ((win 0)
	     (loss 0))
    (if (= (+ win loss) n)
	(list win loss)
	(if (f)
	    (loop (+ win 1) loss)
	    (loop win (+ loss 1))))))

;; Start the Monty Hall game
(define game (start-game))

;; Always pick door number 1 then switch.
;; These numbers refer to wins and losses, respectively
(count-wins (lambda ()
	      (game 1 'switch)) 1000)

;; Always pick door number 1 and keep that door
(count-wins (lambda ()
	      (game 1 'keep)) 1000)
