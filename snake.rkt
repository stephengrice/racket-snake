#lang racket

(require
	"config.rkt")

(provide 
	snake%
	food%)

; Snake class
(define snake% (class object%
	(init [body-arg empty])
	; Initialize body as the provided list or a random grid location
	; Body is a list of each of the snake's squares, denoted as '(x y) where x and y are the coordinates within grid system
	(define body
		(cond
		[(empty? body-arg) (list (random-square))]
		[else body-arg]))
	(define direction 'right)
	(super-new)
	(define/public (draw dc)
		(send dc set-pen "Black" 0 'transparent)
		(send dc set-brush "Dark Green" 'solid)
		(map (lambda (elem)
			(draw-square elem dc))
			body))
	(define/public (get-body)
		body)
	(define/public (get-head)
		(car body))
	(define/public (set-direction val)
		(set! direction val))
	(define/public (collides? pair)
		(define result #f)
		; Check each body element to see if collides with given pair
		(for ([i body])
			#:break (equal? result #t)
			(when (equal? pair (get-head))
				(set! result #t)))
		; Return the resulting boolean
		result)
	(define/public (move)
		; 1. Add new head in proper direction
		(grow)
		(and debug? (printf "Moved head: ~v, ~v\n" (car (car body)) (cdr (car body))))
		; 2. Remove tail (unless instructed to grow during this step)
		(set! body (remove-tail body)))
	(define/public (grow)
		; Add one more square to the snake according to direction
		(define new-head (get-head))	
		(cond
			[(eq? direction 'right)
				(set! new-head (square-right new-head))]
			[(eq? direction 'left)
				(set! new-head (square-left new-head))]
			[(eq? direction 'up)
				(set! new-head (square-above new-head))]
			[(eq? direction 'down)
				(set! new-head (square-below new-head))])	
		(set! body (cons new-head body)))))

; Food class
(define food% (class object%
	(define body (random-square))
	(super-new)
	(define/public (draw dc)
		(send dc set-brush "Green" 'solid)
		(draw-square body dc))
	(define/public (get-body)
		body)
	(define/public (move)
		(set! body (random-square)))))

; Helper functions for grid system
	; Draw-square: Draws square in grid system
(define (draw-square position dc)
	(let ([x (car position)]
		[y (cdr position)])
		(send dc draw-rectangle (* x square-size) (* y square-size) square-size square-size)))
	; random-square: gets random square in grid
(define (random-square)
	(cons (random (+ 1 (car (grid-max)))) (random (cdr (grid-max)))))
	; grid-max: returns a pair containing the max x and y values that are onscreen in grid system
(define (grid-max)
	(define pix-width (car window-size))
	(define pix-height (cdr window-size))
	(define dc-width (/ pix-width default-scale))
	(define dc-height (/ pix-height default-scale))
	(cons (- (/ dc-width square-size) 1) (- (/ dc-height square-size) 1)))
	; remove-tail: Remove tail from l and return result
(define (remove-tail l)
	(cond
		[(empty? l) (error "remove-tail: list cannot be empty")]
		[(empty? (cdr l)) empty]
		[else (cons (car l) (remove-tail (cdr l)))]))
; Helper functions for relative square locations
(define (square-above square)
	(cons (car square) (- (cdr square) 1)))

(define (square-below square)
	(cons (car square) (+ (cdr square) 1)))

(define (square-left square)
	(cons (- (car square) 1) (cdr square)))

(define (square-right square)
	(cons (+ (car square) 1) (cdr square)))
