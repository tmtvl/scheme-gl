(use-modules (chickadee)
	     (gl)
	     (gl enums)
             (system repl coop-server))

(define window-title "Scheme GL")

(define window-width 640)
(define window-height 480)

(define get-repl
  (let ((repl #f))
    (lambda ()
      (if (not repl)
	  (set! repl (spawn-coop-repl-server)))
      repl)))

(define (update tick)
  (poll-coop-repl-server (get-repl)))

(define (draw alpha)
  (gl-clear (clear-buffer-mask color-buffer))

  (gl-begin (begin-mode quads)
	    (gl-color 1.0 0.0 0.0)
	    (gl-vertex -0.5 -0.5)
	    (gl-color 1.0 1.0 0.0)
	    (gl-vertex 0.5 -0.5)
	    (gl-color 0.0 1.0 0.0)
	    (gl-vertex 0.5 0.5)
	    (gl-color 0.0 0.0 1.0)
	    (gl-vertex -0.5 0.5)))

(run-game #:window-title window-title
	  #:window-width window-width
	  #:window-height window-height
	  #:update (lambda (tick)
		     (update tick))
	  #:draw (lambda (alpha)
		   (draw alpha)))
