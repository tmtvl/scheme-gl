(use-modules ((chickadee)
	      #:prefix chickadee:)
	     (chickadee game-loop)
	     (gl)
	     (gl enums)
	     (gl low-level)
	     (glu)
	     (sdl2)
	     (sdl2 events)
	     (sdl2 image)
	     (sdl2 surface)
	     (sdl2 video))

(define current-window #f)
(define gl-context #f)

(define title "Guile SDL/GL Window")
(define width 640)
(define height 480)

(define xrot 0.0)
(define yrot 0.0)
(define zrot 0.0)

(define cb-texture #f)

(define (update time)
  (set! xrot
    (if (>= xrot 360.0)
	0.0
	(+ xrot 0.3)))
  (set! yrot
    (if (>= yrot 360.0)
	0.0
	(+ yrot 0.2)))
  (set! zrot
    (if (>= zrot 360.0)
	0.0
	(+ zrot 0.4)))

  (let ((ev (poll-event)))
    (cond ((quit-event? ev)
	   (chickadee:abort-game)
	   (quit)))))

(define (render alpha)
  (gl-clear (logior (clear-buffer-mask color-buffer)
		    (clear-buffer-mask depth-buffer)))

  (gl-load-identity)

  (gl-translate 0.0 0.0 -5.0)

  (gl-rotate xrot 1.0 0.0 0.0)
  (gl-rotate yrot 0.0 1.0 0.0)
  (gl-rotate zrot 0.0 0.0 1.0)

  (gl-bind-texture (texture-target texture-2d)
		   cb-texture)

  (gl-begin (begin-mode quads)
	    ;; FRONT
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex -1.0  1.0  1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    ;; BACK
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    ;; TOP
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex -1.0  1.0  1.0)
	    ;; BOTTOM
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    ;; RIGHT
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    ;; LEFT
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex -1.0  1.0  1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex -1.0 -1.0  1.0))

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
  (gl-enable (enable-cap texture-2d))

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

  (image-init)

  (set! gl-context
    (make-gl-context current-window))

  (gl-resize width height)

  (let ((image (load-image "color_blindness.png")))
    (set! cb-texture
      (gl-generate-texture))

    (gl-bind-texture (texture-target texture-2d)
		     cb-texture)

    (glTexImage2D (texture-target texture-2d)
		  0 3
		  (surface-width image)
		  (surface-height image)
		  0 (pixel-format rgb)
		  (pixel-type unsigned-byte)
		  (surface-pixels image))

    (glTexParameteri (texture-target texture-2d)
		     (texture-parameter-name texture-min-filter)
		     (texture-min-filter linear))

    (glTexParameteri (texture-target texture-2d)
		     (texture-parameter-name texture-mag-filter)
		     (texture-mag-filter linear)))

  (gl-init))

(define (quit)
  (image-quit)

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
