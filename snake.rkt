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
	(define/public (dying?)
		(define offscreen? (not (onscreen? (get-head))))
		; If head is colliding with some other part of the body, dying is true
		(define overlap? (collides? (cdr body) (get-head)))
		(printf "offscreen?: ~v~n" offscreen?)
		(printf "overlap?: ~v~n" overlap?)
		(or offscreen? overlap?))
	(define/public (eating? food-location)
		(collides? body food-location))
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
	; collides? Check if `list` has any collisions with `point` (check if list contains the point)
(define (collides? arg-list arg-pair)
	(define result #f)
	; Check each body element to see if collides with given pair
	(for ([i arg-list])
		#:break (equal? result #t)
		(when (equal? arg-pair i)
			(set! result #t)))
	; Return the resulting boolean
	result)
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
	; onscreen?
(define (onscreen? pair)
	(define local-max (grid-max))
	(define x (car pair))
	(define y (cdr pair))
	(define within-x-axis? (and (>= x 0) (<= x (car local-max))))
	(define within-y-axis? (and (>= y 0) (<= y (cdr local-max))))
	(and within-x-axis? within-y-axis?))
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
