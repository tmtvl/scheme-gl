(use-modules ((chickadee)
	      #:prefix chickadee:)
	     (chickadee game-loop)
	     (gl)
	     (gl enums)
	     (gl low-level)
	     (glu)
	     (sdl2)
	     (sdl2 events)
	     (sdl2 video))

(define current-window #f)
(define gl-context #f)

(define title "Guile SDL/GL Window")
(define width 640)
(define height 480)

(define rtri 0.0)
(define rquad 0.0)

(define toggle-fullscreen
  (let ((fullscreen #f))
    (lambda ()
      (set! fullscreen (not fullscreen))
      (set-window-fullscreen! current-window fullscreen))))

(define (update time)
  (set! rtri
    (if (>= rtri 360.0)
	0.0
	(+ rtri 0.2)))
  (set! rquad
    (if (<= rquad -360.0)
	0.0
	(- rquad 0.15)))

  (let ((ev (poll-event)))
    (cond ((quit-event? ev)
	   (chickadee:abort-game)
	   (quit))
	  ((keyboard-down-event? ev)
	   (case (keyboard-event-key ev)
	     ('f4 (toggle-fullscreen)))))))

(define (render alpha)
  (gl-clear (logior (clear-buffer-mask color-buffer)
		    (clear-buffer-mask depth-buffer)))

  (gl-load-identity)

  (gl-translate -1.5 0.0 -6.0)

  (gl-rotate rtri 0.0 1.0 0.0)

  (gl-begin (begin-mode triangles)
	    ;; FRONT
	    (gl-color 1.0 0.0 0.0)
	    (gl-vertex  0.0  1.0  0.0)
	    (gl-color 0.0 1.0 0.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    (gl-color 0.0 0.0 1.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    ;; RIGHT
	    (gl-color 1.0 0.0 0.0)
	    (gl-vertex  0.0  1.0  0.0)
	    (gl-color 0.0 0.0 1.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    (gl-color 0.0 1.0 0.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    ;; BACK
	    (gl-color 1.0 0.0 0.0)
	    (gl-vertex  0.0  1.0  0.0)
	    (gl-color 0.0 1.0 0.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    (gl-color 0.0 0.0 1.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    ;; LEFT
	    (gl-color 1.0 0.0 0.0)
	    (gl-vertex  0.0  1.0  0.0)
	    (gl-color 0.0 0.0 1.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-color 0.0 1.0 0.0)
	    (gl-vertex -1.0 -1.0  1.0))

  (gl-load-identity)

  (gl-translate 1.5 0.0 -7.0)

  (gl-rotate rquad 1.0 1.0 1.0)

  (gl-begin (begin-mode quads)
	    ;; TOP
	    (gl-color 0.0 1.0 0.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    (gl-vertex -1.0  1.0  1.0)
	    (gl-vertex  1.0  1.0  1.0)
	    ;; BOTTOM
	    (gl-color 1.0 0.5 0.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    ;; FRONT
	    (gl-color 1.0 0.0 0.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-vertex -1.0  1.0  1.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    ;; BACK
	    (gl-color 1.0 1.0 0.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    ;; LEFT
	    (gl-color 0.0 0.0 1.0)
	    (gl-vertex -1.0  1.0  1.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    ;; RIGHT
	    (gl-color 1.0 0.0 1.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    (gl-vertex  1.0 -1.0  1.0))

  (swap-gl-window current-window))

(define (gl-resize width height)
  (if (= height 0)
      (set! height 1))

  (gl-viewport 0 0 width height)

  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)

  (glu-perspective 45.0 (* (/ width height) 1.0) 0.1 100.0)

  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity))

(define (gl-init)
  (set-gl-shade-model (shading-model smooth))

  (set-gl-clear-color 0.0 0.0 0.0 0.0)
  (set-gl-clear-depth 1.0)

  (gl-enable (enable-cap depth-test))
  (set-gl-depth-function (depth-function lequal))

  (glHint (hint-target perspective-correction-hint)
	  (hint-mode nicest)))

(define (init)
  (sdl-init)

  (set! current-window
    (make-window #:title title
		 #:size (list width height)
		 #:opengl? #t))

  (set! gl-context
    (make-gl-context current-window))

  (gl-resize width height)

  (gl-init))

(define (quit)
  (delete-gl-context! gl-context)
  (set! gl-context #f)

  (close-window! current-window)
  (set! current-window #f)

  (sdl-quit))

(define (run-game)
  (init)

  (run-game* #:update (lambda (time)
			(update time))
	     #:render (lambda (alpha)
			(render alpha))
	     #:time sdl-ticks
	     #:error (lambda (stack key args)
		       (quit)
		       (error "We encountered an error."
			      key args stack))))

(run-game)
