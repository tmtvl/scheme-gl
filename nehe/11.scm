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

(define wiggle #f)

(define points
  (list->vector
   (map
    (lambda (i)
      (let ((x (/ (quotient i 45) 5.0))
	    (y (/ (remainder i 45) 5.0)))
	(vector (- x 4.5)
		(- y 4.5)
		(sin (* (/ (* x 40.0)
			   360.0)
			3.141592654
			2)))))
    (iota (* 45 45)))))

(define flag-texture #f)

(define xrot 0.0)
(define yrot 0.0)
(define zrot 0.0)

(define xspeed 0.3)
(define yspeed 0.2)
(define zspeed 0.4)

(define (update time)
  (if wiggle
      (begin
	(for-each
	 (lambda (y)
	   (let ((hold (vector-ref
			(vector-ref points y)
			2)))
	     (for-each
	      (lambda (x)
		(vector-set!
		 (vector-ref points (+ x y))
		 2
		 (vector-ref
		  (vector-ref points (+ x y 45))
		  2)))
	      (iota 44 0 45))
	     (vector-set! (vector-ref points
				      (+ (* 44 45) y))
			  2
			  (vector-ref
			(vector-ref points y)
			2))))
	 (iota 45))
	(set! wiggle #f))
      (set! wiggle #t))

  (set! xrot
    (if (>= (abs xrot) 360.0)
	xspeed
	(+ xrot xspeed)))
  (set! yrot
    (if (>= (abs yrot) 360.0)
	yspeed
	(+ yrot yspeed)))
  (set! zrot
    (if (>= (abs zrot) 360.0)
	zspeed
	(+ zrot zspeed)))

  (let ((ev (poll-event)))
    (cond ((quit-event? ev)
	   (chickadee:abort-game)
	   (quit)))))

(define (render alpha)
  (gl-clear (logior (clear-buffer-mask color-buffer)
		    (clear-buffer-mask depth-buffer)))

  (gl-load-identity)

  (gl-translate 0.0 0.0 -12.0)

  (gl-rotate xrot 1.0 0.0 0.0)
  (gl-rotate yrot 0.0 1.0 0.0)
  (gl-rotate zrot 0.0 0.0 1.0)

  (gl-bind-texture (texture-target texture-2d)
		   flag-texture)

  (gl-begin (begin-mode quads)
	    (map
	     (lambda (i)
	       (let* ((x (quotient i 44))
		      (y (remainder i 44))
		      (fx (/ x 44.0))
		      (fy (- 1.0 (/ y 44.0)))
		      (fxb (/ (1+ x) 44.0))
		      (fyb (- 1.0 (/ (1+ y) 44.0)))
		      (bl (vector-ref points (+ (* x 45) y)))
		      (tl (vector-ref points (+ (* x 45) y 1)))
		      (tr (vector-ref points (+ (* x 45) y 46)))
		      (br (vector-ref points (+ (* x 45) y 45))))
		 (gl-texture-coordinates fx fy)
		 (gl-vertex (vector-ref bl 0)
			    (vector-ref bl 1)
			    (vector-ref bl 2))
		 (gl-texture-coordinates fx fyb)
		 (gl-vertex (vector-ref tl 0)
			    (vector-ref tl 1)
			    (vector-ref tl 2))
		 (gl-texture-coordinates fxb fyb)
		 (gl-vertex (vector-ref tr 0)
			    (vector-ref tr 1)
			    (vector-ref tr 2))
		 (gl-texture-coordinates fxb fy)
		 (gl-vertex (vector-ref br 0)
			    (vector-ref br 1)
			    (vector-ref br 2))))
	     (iota (* 44 44))))

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
	  (hint-mode nicest))

  (glPolygonMode (cull-face-mode back)
		 (polygon-mode fill))
  (glPolygonMode (cull-face-mode front)
		 (polygon-mode line)))

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

  (let ((image (load-image "color_blindness.png"))
	(tx-target (texture-target texture-2d)))
    (set! flag-texture (gl-generate-texture))

    (gl-bind-texture tx-target
		     flag-texture)

    (glTexImage2D tx-target 0 3
		  (surface-width image)
		  (surface-height image)
		  0
		  (pixel-format rgb)
		  (pixel-type unsigned-byte)
		  (surface-pixels image))

    (glTexParameteri tx-target
		     (texture-parameter-name texture-mag-filter)
		     (texture-mag-filter linear))

    (glTexParameteri tx-target
		     (texture-parameter-name texture-min-filter)
		     (texture-min-filter linear)))

  (gl-init))

(define (quit)
  (gl-delete-texture flag-texture)
  (set! flag-texture #f)

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
