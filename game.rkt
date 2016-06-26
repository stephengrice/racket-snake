#lang racket
(require 
	racket/gui
	racket/draw)

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
			(printf "Mouse input detected"))
		; Overrided method for keyboard input
		(define/override (on-char event)
			(printf "Keyboard input detected"))
		(super-new)))

; Create instance of custom canvas with paint-callback
(define my-canvas (new custom-canvas
	[parent frame]
	[paint-callback (lambda (canvas dc)
		(send dc set-scale 2 2)
		(send dc set-text-foreground "red")
		(send dc draw-text "Racket Snake Initial Commit" 0 0))]))

; Show the frame
(send frame show #t)