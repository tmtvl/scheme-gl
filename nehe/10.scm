(use-modules ((chickadee)
	      #:prefix chickadee:)
	     (chickadee game-loop)
	     (gl)
	     (gl enums)
	     (gl low-level)
	     (glu)
	     (sdl2)
	     (sdl2 events)
	     (sdl2 video)
	     (srfi srfi-9)
	     (sxml simple))

(define current-window #f)
(define gl-context #f)

(define title "Guile SDL/GL Window")
(define width 640)
(define height 480)

(define sector1 #f)

(define-record-type <vertex>
  (make-vertex x y z u v)
  vertex?
  (x vertex-x set-vertex-x!)
  (y vertex-y set-vertex-y!)
  (z vertex-z set-vertex-z!)
  (u vertex-u set-vertex-u!)
  (v vertex-v set-vertex-v!))

(define-record-type <triangle>
  (MAKE-TRIANGLE vertices)
  triangle?
  (vertices triangle-vertices set-triangle-vertices!))

(define (make-triangle v1 v2 v3)
  (MAKE-TRIANGLE (vector v1 v2 v3)))

(define-record-type <sector>
  (MAKE-SECTOR triangles num-triangles)
  sector?
  (triangles sector-triangles set-sector-triangles!)
  (num-triangles sector-num-triangles set-sector-num-triangles!))

(define (make-sector triangles)
  (MAKE-SECTOR triangles
	       (vector-length triangles)))

(define (update time)
  (let ((ev (poll-event)))
    (cond ((quit-event? ev)
	   (chickadee:abort-game)
	   (quit)))))

(define (render alpha)
  (gl-clear (logior (clear-buffer-mask color-buffer)
		    (clear-buffer-mask depth-buffer)))

  (gl-load-identity)

  (swap-gl-window current-window))

(define (setup-world)
  (let* ((world
	  (caddr
	   (call-with-input-file "world.xml"
	     (lambda (port)
	       (xml->sxml port
			  #:trim-whitespace? #t)))))
	 (triangles (cddadr world)))
    (set! sector1
      (make-sector
       (list->vector
	(map
	 (lambda (triangle)
	   (let ((c1 (cdadr (cadr triangle)))
		 (c2 (cdadr (caddr triangle)))
		 (c3 (cdadr (cadddr triangle))))
	     (make-triangle
	      (make-vertex (string->number (cadr (list-ref c1 0)))
			   (string->number (cadr (list-ref c1 1)))
			   (string->number (cadr (list-ref c1 2)))
			   (string->number (cadr (list-ref c1 3)))
			   (string->number (cadr (list-ref c1 4))))
	      (make-vertex (string->number (cadr (list-ref c2 0)))
			   (string->number (cadr (list-ref c2 1)))
			   (string->number (cadr (list-ref c2 2)))
			   (string->number (cadr (list-ref c2 3)))
			   (string->number (cadr (list-ref c2 4))))
	      (make-vertex (string->number (cadr (list-ref c3 0)))
			   (string->number (cadr (list-ref c3 1)))
			   (string->number (cadr (list-ref c3 2)))
			   (string->number (cadr (list-ref c3 3)))
			   (string->number (cadr (list-ref c3 4)))))))
	 triangles))))))

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
