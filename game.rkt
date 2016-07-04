#lang racket
(require 
	racket/gui
	racket/draw)

; Variable declarations
(define running? #t)
(define fps 60)
(define frame-time (/ 1000 fps))
(define square-size 5)
(define window-size (cons 600 400))

; Snake class
(define snake% (class object%
	(init body-arg)
	; Initialize body as provided list or random spot
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

; Render: All drawing code
(define render (lambda (dc)
	(send dc clear)
	(send dc set-scale 2 2)
	(define sn (new snake% [body-arg empty]))
	(send sn draw dc)))

; Game logic: Update snake and food
(define (update-game)
	0)
; Define frame
(define frame
	(new frame% 
		[label "Racket Snake"]
		[min-width (car window-size)]
		[min-height (cdr window-size)]))

; Define custom canvas to handle input
(define custom-canvas 
	(class canvas%
		; Overrided method for mouse input
		;(define/override (on-event event)
		;	(printf "Mouse input detected\n"))
		; Overrided method for keyboard input
		(define/override (on-char event)
			(printf "Keyboard input detected\n"))
		(super-new)))

; Create instance of custom canvas with paint-callback
(define my-canvas (new custom-canvas
	[parent frame]
	[paint-callback (lambda (canvas dc)
		(render dc))]))

(define dc (send my-canvas get-dc))


; Main game loop
(define game-thread (thread (lambda ()
	(let loop ()
		; Get time for start of this loop iteration
		(define start-time (current-inexact-milliseconds))
	
		; Update, render
		(update-game)
		(render dc)
	
		; Determine amount of time passed
		(define elapsed-time (- start-time (current-inexact-milliseconds)))
		; Sleep if time left
		(define sleep-time (/ (- frame-time elapsed-time ) 1000))
		(cond [(> sleep-time 0)
			(sleep sleep-time)
			(printf "Slept for ~v~n" sleep-time)])
		; Loop game
		(cond
			[running? (loop)])))))


; Show the frame
(send frame show #t)
