#lang racket
(require 
	racket/gui
	racket/draw)

(define running? #t)
(define fps 60)
(define frame-time (/ 1000 fps))

(define test-value 0)

(define render (lambda (dc)
	(send dc clear)
	(send dc set-scale 2 2)
	(send dc set-text-foreground "red")
	(send dc draw-text "Racket Snake" 0 0)
	(send dc draw-text "Hey" test-value 0)))


; Game logic: Update snake and food
(define (update-game)
	(if (> test-value 600) (set! test-value 0) (set! test-value (+ test-value 1))))

; Define frame
(define frame
	(new frame% 
		[label "Racket Snake"]
		[min-width 600]
		[min-height 400]))

; Define custom canvas to handle input
(define custom-canvas 
	(class canvas%
		; Overrided method for mouse input
		(define/override (on-event event)
			(printf "Mouse input detected\n"))
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
