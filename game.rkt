#lang racket
(require 
	racket/gui
	racket/draw)

(define running? #t)
(define fps 60)
(define frame-time (/ 1000 fps))

(define render (lambda (canvas dc)
	(printf "In render\n")
	(send dc set-scale 2 2)
	(send dc set-text-foreground "red")
	(send dc draw-text "Racket Snake" 0 0)))

; Main game loop
(define (game-loop)
	; Get time for start of this loop iteration
	(define start-time (current-inexact-milliseconds))
	
	; Update, render
	(update-game)
	(printf "Executed game logic\n")
	; Determine amount of time passed
	(define elapsed-time (- start-time (current-inexact-milliseconds)))
	; Sleep if time left
	(define sleep-time (/ (- frame-time elapsed-time ) 1000))
	(printf "Sleep time: ~v" sleep-time)
	(cond [(> sleep-time 0)
		(printf "About to sleep ~v" sleep-time)
		(sleep sleep-time)
		(printf "Slept for ~v" sleep-time)])
	; Loop game
	(cond
		[running? (game-loop)]))

; Game logic: Update snake and food
(define (update-game)
	(printf "Executing game logic\n"))


; GUI:
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
	[paint-callback render]))
; Show the frame
(send frame show #t)


; Start the game loop
(game-loop)
