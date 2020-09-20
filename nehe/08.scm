(use-modules ((chickadee)
	      #:prefix chickadee:)
	     (chickadee game-loop)
	     (gl)
	     (gl enums)
	     (gl low-level)
	     (glu)
	     (glu low-level)
	     (rnrs bytevectors)
	     (sdl2)
	     (sdl2 events)
	     (sdl2 image)
	     (sdl2 surface)
	     (sdl2 video)
	     (system foreign))

(define current-window #f)
(define gl-context #f)

(define title "Guile SDL/GL Window")
(define width 640)
(define height 480)

(define render-light #f)
(define l-pressed #f)
(define f-pressed #f)

(define xrot 0.0)
(define yrot 0.0)

(define xspeed 0.0)
(define yspeed 0.0)

(define z -5.0)

(define light-ambient
  (let ((bv (make-bytevector 16)))
    (for-each (lambda (i)
		(bytevector-ieee-single-native-set! bv
						    i
						    0.5))
	      (iota 3 0 4))
    (bytevector-ieee-single-native-set! bv
					12
					1.0)
    bv))
(define light-diffuse
  (let ((bv (make-bytevector 16)))
    (for-each (lambda (i)
		(bytevector-ieee-single-native-set! bv
						    i
						    1.0))
	      (iota 4 0 4))
    bv))
(define light-position
  (let ((bv (make-bytevector 16)))
    (bytevector-ieee-single-native-set! bv
					8
					2.0)
    (bytevector-ieee-single-native-set! bv
					12
					1.0)
    bv))

(define mfilter 0)
(define textures '())

(define (update time)
  (set! xrot
    (if (>= (abs xrot) 360.0)
	0.0
	(+ xrot xspeed)))
  (set! yrot
    (if (>= (abs yrot) 360.0)
	0.0
	(+ yrot yspeed)))

  (let ((ev (poll-event)))
    (cond ((quit-event? ev)
	   (chickadee:abort-game)
	   (quit))
	  ((keyboard-down-event? ev)
	   (case (keyboard-event-key ev)
	     ((l) (if (not l-pressed)
		     (begin
		       (set! l-pressed #t)
		       (set! render-light
			 (not render-light))

		       (if render-light
			   (gl-enable (enable-cap lighting))
			   (gl-disable (enable-cap lighting))))))
	     ((f) (if (not f-pressed)
		     (begin
		       (set! f-pressed #t)
		       (set! mfilter
			 (if (= mfilter 2)
			     0
			     (+ mfilter 1))))))
	     ((page-up) (set! z (- z 0.02)))
	     ((page-down) (set! z (+ z 0.02)))
	     ((right) (set! yspeed (+ yspeed 0.01)))
	     ((left) (set! yspeed (- yspeed 0.01)))
	     ((up) (set! xspeed (+ xspeed 0.01)))
	     ((down) (set! xspeed (- xspeed 0.01)))))
	  ((keyboard-up-event? ev)
	   (case (keyboard-event-key ev)
	     ((l) (set! l-pressed #f))
	     ((f) (set! f-pressed #f)))))))

(define (render alpha)
  (gl-clear (logior (clear-buffer-mask color-buffer)
		    (clear-buffer-mask depth-buffer)))

  (gl-load-identity)

  (gl-translate 0.0 0.0 z)

  (gl-rotate xrot 1.0 0.0 0.0)
  (gl-rotate yrot 0.0 1.0 0.0)

  (gl-bind-texture (texture-target texture-2d)
		   (list-ref textures mfilter))

  (gl-begin (begin-mode quads)
	    ;; FRONT
	    (gl-normal  0.0  0.0  1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex -1.0  1.0  1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    ;; BACK
	    (gl-normal  0.0  0.0 -1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    ;; TOP
	    (gl-normal  0.0  1.0  0.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex -1.0  1.0 -1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex -1.0  1.0  1.0)
	    ;; BOTTOM
	    (gl-normal  0.0 -1.0  0.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex -1.0 -1.0 -1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex -1.0 -1.0  1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    ;; RIGHT
	    (gl-normal  1.0  0.0  0.0)
	    (gl-texture-coordinates 1.0 0.0)
	    (gl-vertex  1.0 -1.0 -1.0)
	    (gl-texture-coordinates 1.0 1.0)
	    (gl-vertex  1.0 -1.0  1.0)
	    (gl-texture-coordinates 0.0 1.0)
	    (gl-vertex  1.0  1.0  1.0)
	    (gl-texture-coordinates 0.0 0.0)
	    (gl-vertex  1.0  1.0 -1.0)
	    ;; LEFT
	    (gl-normal -1.0  0.0  0.0)
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

  (set-gl-clear-color 0.0 0.0 0.0 0.5)
  (set-gl-clear-depth 1.0)

  (gl-enable (enable-cap depth-test))
  (set-gl-depth-function (depth-function lequal))

  (glHint (hint-target perspective-correction-hint)
	  (hint-mode nicest))

  (glLightfv (light-name light1)
	     (light-parameter ambient)
	     light-ambient)

  (glLightfv (light-name light1)
	     (light-parameter diffuse)
	     light-diffuse)

  (glLightfv (light-name light1)
	     (light-parameter position)
	     light-position)

  (gl-enable (enable-cap light1)))

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
    (set! textures
      (map
       (lambda (_)
	 (gl-generate-texture))
       (iota 3)))

    (for-each
     (lambda (tx mag-param min-param builder)
       (gl-bind-texture tx-target
			tx)

       (glTexParameteri tx-target
			(texture-parameter-name texture-mag-filter)
			mag-param)
       (glTexParameteri tx-target
			(texture-parameter-name texture-min-filter)
			min-param)

       (builder))
     textures
     (list (texture-mag-filter nearest)
	   (texture-mag-filter linear)
	   (texture-mag-filter linear))
     (list (texture-min-filter nearest)
	   (texture-min-filter linear)
	   (texture-min-filter linear-mipmap-nearest))
     (append (make-list 2
			(lambda ()
			  (glTexImage2D tx-target
					0 3
					(surface-width image)
					(surface-height image)
					0 (pixel-format rgb)
					(pixel-type unsigned-byte)
					(surface-pixels image))))
	     (list (lambda ()
		     (gluBuild2DMipmaps tx-target
					3
					(surface-width image)
					(surface-height image)
					(pixel-format rgb)
					(pixel-type unsigned-byte)
					(scm->pointer
					 (surface-pixels image))))))))

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
