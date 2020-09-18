(use-modules (chickadee)
	     (chickadee render)
	     (chickadee render buffer)
	     (chickadee render shader)
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

(define get-shader
  (let ((my-shader #f))
    (lambda ()
      (if (not my-shader)
	  (set! my-shader
	    (load-shader "vert.glsl" "frag.glsl")))
      my-shader)))

(define (draw alpha)
  (let* ((data (f32vector -0.5 -0.5
			  1.0 0.0 0.0
			  0.5 -0.5
			  0.0 1.0 0.0
			  0.0 0.5
			  0.0 0.0 1.0))
	 (buffer (make-buffer data
			      #:stride 20))
	 (vertices (make-buffer-view #:buffer buffer
				     #:type 'vec2
				     #:component-type 'float
				     #:length 3))
	 (colors (make-buffer-view #:buffer buffer
				   #:type 'vec3
				   #:component-type 'float
				   #:length 3
				   #:offset 8))
	 (index-buffer (make-buffer (u32vector 0 1 2)
				    #:target 'index))
	 (indices (make-buffer-view #:type 'scalar
				    #:component-type 'unsigned-int
				    #:buffer index-buffer)))
    (gpu-apply (get-shader)
	       (make-vertex-array #:indices indices
				  #:attributes `((0 . ,vertices)
						 (1 . ,colors))))))

(run-game #:window-title window-title
	  #:window-width window-width
	  #:window-height window-height
	  #:update (lambda (tick)
		     (update tick))
	  #:draw (lambda (alpha)
		   (draw alpha)))
