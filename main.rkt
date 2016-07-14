#lang racket
(require 
	racket/gui
	racket/draw
	"config.rkt"
	"snake.rkt")

; Variable declarations
(define running? #t)
(define fps 20)
(define frame-time (/ 1000 fps))
(define snake (new snake% [body-arg empty]))
(define direction 'right)

(define render (lambda (dc)
	(send dc clear)
	(send dc set-scale 2 2)
	(send snake draw dc)))

; Game logic: Update snake and food
(define (update-game)
	(and debug? (printf "updated game\n"))
	(send snake move direction))
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
			(define code (send event get-key-code))
			(cond 
				[(eq? code 'left) (set! direction 'left)]
				[(eq? code 'right) (set! direction 'right)]
				[(eq? code 'up) (set! direction 'up)]
				[(eq? code 'down) (set! direction 'down)]))
		(super-new)))

; Create instance of custom canvas with paint-callback
(define game-canvas (new custom-canvas
	[parent frame]
	[paint-callback (lambda (canvas dc)
		(render dc))]))

(define dc (send game-canvas get-dc))


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
			(and debug? (printf "Slept for ~v~n" sleep-time))])
		; Loop game
		(cond
			[running? (loop)])))))


; Show the frame
(send frame show #t)
