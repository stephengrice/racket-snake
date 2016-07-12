#lang racket

(require
	"config.rkt")

(provide snake%)

; Snake class
(define snake% (class object%
	(init [body-arg empty])
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
			body))
	(define/public (get-body)
		body)
	(define/public (move direction)
		; 1. Add new head in proper direction
		(define new-head (car body))	
		(cond
			[(eq? direction 'right)
				; new-head.x += 1
				(set! new-head (cons (+ (car new-head) 1) (cdr new-head)))]
			
			[(eq? direction 'left)
				; new-head.x -= 1
				(set! new-head (cons (- (car new-head) 1) (cdr new-head)))]

			[(eq? direction 'up)
				; new-head.y -= 1
				(set! new-head (cons (car new-head) (- (cdr new-head) 1)))]

			[(eq? direction 'down)
				; new-head.y += 1
				(set! new-head (cons (car new-head) (+ (cdr new-head) 1)))])
		(and debug? (printf "Head: ~v, ~v\n" (car (car body)) (cdr (car body))))
		(set! body (cons new-head body))
		; 2. Remove tail
		(set! body (remove-tail body)))))

; Helper functions for grid system
	; Draw-square: Draws square in grid system
(define (draw-square x y dc)
	(send dc draw-rectangle (* x square-size) (* y square-size) square-size square-size))
	; random-square: gets random square in grid
(define (random-square)
	(cons (random (+ 1 (car (grid-max)))) (random (cdr (grid-max)))))
	; grid-max: returns a pair containing the max x and y values that are onscreen in grid system
(define (grid-max)
	(cons (floor (/ (car window-size) square-size)) (floor (/ (cdr window-size) square-size))))
	; remove-tail: Remove tail from l and return result
(define (remove-tail l)
	(cond
		[(empty? l) (error "remove-tail: list cannot be empty")]
		[(empty? (cdr l)) empty]
		[else (cons (car l) (remove-tail (cdr l)))]))
