#lang racket
(require 
	racket/gui
	racket/draw)

(define running? #t)
(define fps 60)
(define frame-time (/ 1000 fps))

(define render (lambda (canvas dc)
	(send dc set-scale 2 2)
	(send dc set-text-foreground "red")
	(send dc draw-text "Racket Snake" 0 0)))

; Main game loop
(define (game-loop last-time)
	; Get time for start of this loop iteration
	(define current-time (current-seconds))
	; Determine amount of time since last iteration
	(define elapsed-time (- current-time last-time)
	; Compare time
	(cond 
		; Case 1: There is extra time left (before the frame is up)
		[(< elapsed-time frame-time) 
			; In this case, we can execute the game logic
			(game-logic)
			(sleep (- frame-time elapsed-time))]
		[else (game)])
	; And wait appropriate amount
	(game-loop current-time))

; Game logic: Update snake and food
(define (game-logic)
	printf("Executing game logic\n"))


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
