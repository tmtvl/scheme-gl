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

(define twinkle #f)
(define t-pressed #f)

(define star-count 50)

(define (make-star r g b dist angle)
  (list (list r g b)
	dist angle))

(define (star-r star)
  (caar star))

(define (star-g star)
  (cadar star))

(define (star-b star)
  (caddar star))

(define (star-dist star)
  (cadr star))

(define (star-angle star)
  (caddr star))

(define stars
  (map
   (lambda (i)
     (make-star (/ (random 256) 255.0)
		(/ (random 256) 255.0)
		(/ (random 256) 255.0)
		(* (/ i star-count) 5.0)
		0.0))
   (iota star-count)))

(define reversed-stars
  (reverse stars))

(define zoom -15.0)

(define tilt -90.0)

(define spin 0.0)

(define tx #f)

(define (update time)
  (set! stars
    (map
     (lambda (star i)
       (if (< (star-dist star) 0.01)
	   (make-star (/ (random 256) 255.0)
		      (/ (random 256) 255.0)
		      (/ (random 256) 255.0)
		      (+ (star-dist star) 5.0)
		      (+ (star-angle star)
			 (* (/ i star-count)
			    1.0)))
	   (make-star (star-r star)
		      (star-g star)
		      (star-b star)
		      (- (star-dist star) 0.01)
		      (+ (star-angle star)
			 (* (/ i star-count)
			    1.0)))))
     stars
     (iota star-count)))

  (set! reversed-stars
    (reverse stars))

  (let ((ev (poll-event)))
    (cond ((quit-event? ev)
	   (chickadee:abort-game)
	   (quit))
	  ((keyboard-down-event? ev)
	   (case (keyboard-event-key ev)
	     ((t) (if (not t-pressed)
		      (begin
			(set! t-pressed #t)
			(set! twinkle
			  (not twinkle)))))
	     ((page-up) (set! zoom (- zoom 0.2)))
	     ((page-down) (set! zoom (+ zoom 0.2)))
	     ((up) (set! tilt (- tilt 0.5)))
	     ((down) (set! tilt (+ tilt 0.5)))))
	  ((keyboard-up-event? ev)
	   (case (keyboard-event-key ev)
	     ((t) (if t-pressed
		      (set! t-pressed #f))))))))

(define (render alpha)
  (gl-clear (logior (clear-buffer-mask color-buffer)
		    (clear-buffer-mask depth-buffer)))

  (gl-bind-texture (texture-target texture-2d)
		   tx)

  (for-each
   (lambda (star tstar)
     (gl-load-identity)

     (gl-translate 0.0 0.0 zoom)

     (gl-rotate tilt 1.0 0.0 0.0)

     (gl-rotate (star-angle star) 0.0 1.0 0.0)

     (gl-translate (star-dist star) 0.0 0.0)

     (gl-rotate (- (star-angle star)) 0.0 1.0 0.0)

     (gl-rotate (- tilt) 1.0 0.0 0.0)

     (if twinkle
	 (begin
	   (gl-color (star-r tstar)
		     (star-g tstar)
		     (star-b tstar)
		     1.0)

	   (gl-begin (begin-mode quads)
		     (gl-texture-coordinates 0.0 0.0)
		     (gl-vertex -1.0 -1.0 0.0)
		     (gl-texture-coordinates 1.0 0.0)
		     (gl-vertex  1.0 -1.0 0.0)
		     (gl-texture-coordinates 1.0 1.0)
		     (gl-vertex  1.0  1.0 0.0)
		     (gl-texture-coordinates 0.0 1.0)
		     (gl-vertex -1.0  1.0 0.0))))

     (gl-rotate spin 0.0 0.0 1.0)

     (gl-color (star-r star)
	       (star-g star)
	       (star-b star)
	       1.0)

     (gl-begin (begin-mode quads)
	       (gl-texture-coordinates 0.0 0.0)
	       (gl-vertex -1.0 -1.0 0.0)
	       (gl-texture-coordinates 1.0 0.0)
	       (gl-vertex  1.0 -1.0 0.0)
	       (gl-texture-coordinates 1.0 1.0)
	       (gl-vertex  1.0  1.0 0.0)
	       (gl-texture-coordinates 0.0 1.0)
	       (gl-vertex -1.0  1.0 0.0))

     (set! spin
       (if (>= spin 360.0)
	   0.0
	   (+ spin 0.01))))
   stars
   reversed-stars)

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

  (glHint (hint-target perspective-correction-hint)
	  (hint-mode nicest))

  (set-gl-blend-function (blending-factor-src src-alpha)
			 (blending-factor-dest one))

  (gl-enable (enable-cap blend)))

(define (init)
  (sdl-init)

  (set! current-window
    (make-window #:title title
		 #:size (list width height)
		 #:opengl? #t))

  (set! gl-context
    (make-gl-context current-window))

  (gl-resize width height)

  (let ((image (load-image "star.png"))
	(tx-target (texture-target texture-2d)))
    (set! tx (gl-generate-texture))

    (gl-bind-texture tx-target
		     tx)

    (glTexParameteri tx-target
		     (texture-parameter-name texture-mag-filter)
		     (texture-mag-filter linear))
    (glTexParameteri tx-target
		     (texture-parameter-name texture-min-filter)
		     (texture-min-filter linear))

    (glTexImage2D tx-target 0 3
		  (surface-width image)
		  (surface-height image)
		  0
		  (pixel-format rgba)
		  (pixel-type unsigned-byte)
		  (surface-pixels image)))

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
