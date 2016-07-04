#lang racket

(require
	"config.rkt")

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

; Helper functions for grid system
	; Draw-square: Draws square in grid system
(define (draw-square x y dc)
	(send dc draw-rectangle (* x square-size) (* y square-size) square-size square-size))
	; random-square: gets random square in grid
(define (random-square)
	(cons (random 1 (car (grid-max)) (current-pseudo-random-generator)) (random 1 (cdr (grid-max)) (current-pseudo-random-generator))))
	; grid-max: returns a pair containing the max x and y values that are onscreen in grid system
(define (grid-max)
	(cons (floor (/ (car window-size) square-size)) (floor (/ (cdr window-size) square-size))))
