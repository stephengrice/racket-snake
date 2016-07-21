#lang racket
(require 
	racket/gui
	racket/draw
	"config.rkt"
	"snake.rkt")

; Game class to manage all game operations
(define game% (class object%
	(define running? #t)
	(define frame-time (/ 1000 fps))
	(super-new)
	; Public methods
	(define/public (loop)
		(thread (lambda () (let inner-loop ()
			; Get start time for this loop iteration
			(define start-time (current-inexact-milliseconds))

			; Update, render
			(update)
			(render dc)

			; Determine amount of time passed
			(define elapsed-time (- start-time (current-inexact-milliseconds)))
			; Sleep if time left
			(define sleep-time (/ (- frame-time elapsed-time) 1000))
			(cond [(> sleep-time 0)
				(sleep sleep-time)])
			;Loop game
			(when running?
				(inner-loop))))))
	(define/public (render dc)
		(send dc clear)
		(send dc set-scale 2 2)
		(send snake draw dc)
		(send food draw dc)
		(draw-score dc)
		(when (not running?)
			(send dc draw-text "Game Over" 20 40)))
	(define/public (update)
		(send snake move)
		(when (send snake eating? (send food get-body))
			(send snake grow)
			(send food move)
			(set! score (+ score 1)))
		(when (send snake dying?)
			(game-over)))
	; Private methods
	(define/private (draw-score dc)
		(define str-score (string-append "Score: " (number->string score)))
		(send dc draw-text str-score 20 20))
	(define/private (game-over)
		(set! running? #f))))


; Define frame
(define frame
	(new frame% 
		[label "Racket Snake"]
		[min-width (car window-size)]
		[min-height (cdr window-size)]))

; Define custom canvas to handle input
(define custom-canvas%
	(class canvas%
		; Overrided method for mouse input
		;(define/override (on-event event)
		;	(printf "Mouse input detected\n"))
		; Overrided method for keyboard input
		(define/override (on-char event)
			(define code (send event get-key-code))
			(cond 
				[(eq? code 'left) (send snake set-direction 'left)]
				[(eq? code 'right) (send snake set-direction 'right)]
				[(eq? code 'up) (send snake set-direction 'up)]
				[(eq? code 'down) (send snake set-direction 'down)]))
		(super-new)))

; Create instance of custom canvas with paint-callback
(define game-canvas (new custom-canvas%
	[parent frame]
	[paint-callback (lambda (canvas dc)
		(send game render dc))]))

; Variable declarations
(define snake (new snake% [body-arg (list (cons 0 0))]))
(define food (new food%))
(define score 0)
(define game (new game%))
(define dc (send game-canvas get-dc))

; Begin game loop as separate thread
(define game-thread (send game loop))

; Show the frame
(send frame show #t)
