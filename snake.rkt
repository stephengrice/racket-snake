#lang racket

(provide snake%)

; Snake class
(define snake% (class object%
	(init body-arg)
	; Initialize body as the provided list or a random grid location
	; Body is a list of each of the snake's squares, denoted as '(x y) where x and y are the coordinates within grid system
	(define body
		(cond
		[(empty? body-arg) (list (random-square))]
		[else body-arg]))
	(super-new)
	(define/public (draw dc)
		(map (lambda (elem)
			(draw-square (car elem) (cdr elem) dc))
			body))))

