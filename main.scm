(import (chicken process-context)
        (chicken port)
        (chicken format)
        (chicken condition)
        (chicken memory)
        args
        miscmacros
        (prefix sdl2 sdl2:))

;; Print a variable number of strings to stderr
(define print-err
  (lambda s
    (with-output-to-port (current-error-port)
      (lambda ()
        (apply print s)))))

;; List of arguments that the program uses
(define opts
 (list (args:make-option (s scale) (required: "VALUE") "scale value [default: 4]"
         (let ((scale (string->number arg)))
           (if scale
               (set! arg scale)
               (begin
                 (print-err (format "Invalid scale value: ~a" arg))
                 (usage)
                 (exit 1)))
           ))
       (args:make-option (h help) #:none "Display this text"
         (usage))))

;; Print usage of the program to stderr
(define (usage)
 (print-err "Usage: " (car (argv)) " [options...] rom-path")
 (print-err "\n")
 (print-err (args:usage opts))
 (exit 1))

;; Initialize SDL components
(define (sdl-init)
  ;; Initialize the parts of SDL that we need.
  (sdl2:set-main-ready!)
  (sdl2:init! '(video events joystick))

  ;; Automatically call sdl2:quit! when program exits normally.
  (on-exit sdl2:quit!)

  ;; Call sdl2:quit! and then call the original exception handler if an
  ;; unhandled exception reaches the top level.
  (current-exception-handler
   (let ((original-handler (current-exception-handler)))
     (lambda (exception)
       (sdl2:quit!)
       (original-handler exception))))
)

(define screen-width 64)
(define screen-heigth 32)

;; Setup SDL objects
;; Creates `window`
(define (sdl-setup scale)
  (define window
    (sdl2:create-window!
     "Chip8-scheme"                                  ; title
     'centered  'centered                            ; x, y
     (* screen-width scale)  (* screen-heigth scale) ; w, h
     '(shown opengl))                                ; flags
  )
  (on-exit (lambda () (sdl2:destroy-window! window)))
  (define renderer
    (sdl2:create-renderer! window -1 '())
  )
  (on-exit (lambda () (sdl2:destroy-renderer! renderer)))
  (define texture
    (sdl2:create-texture renderer 'rgba8888 'streaming screen-width screen-heigth)
  )
  (values window renderer texture)
)

(define (paint-pixel pixel vals)
  (for-each (lambda (ch-val) (pointer-u8-set! (pointer+ pixel (car ch-val))
                                              (cdr ch-val)))
            (map cons '(0 1 2 3) vals))
)

;; Update SDL framebuffer
(define (sdl-update-fb texture)
  (receive (pixels pitch)
    (sdl2:lock-texture-raw! texture #f)
    (begin
      (do ((y 0 (add1 y)))
        ((= y screen-heigth))
        (let ((row-offset (* y pitch)))
          (do ((x 0 (add1 x)))
            ((= x screen-width))
            ; paint pixel (x, y)
            (let* ((pixel-offset (+ row-offset (* x 4)))
                   (pixel (pointer+ pixels pixel-offset)))
                  (paint-pixel pixel (if (= (modulo (+ x y) 2) 1)
                                         '(#xff #xff #xff #xff)
                                         '(#xff #x00 #x00 #x00))
                  )
                  ; (for-each (lambda (ch) (pointer-u8-set! (pointer+ pixel ch) 255))
                  ;           '(0 1 2 3))
            )
            ; (print (format "~a ~a" x y))
          )
        )
      )
      ; (pointer-u8-set! (pointer+ pixels 0) 255)
      ; (pointer-u8-set! (pointer+ pixels 1) 255)
      ; (pointer-u8-set! (pointer+ pixels 2) 255)
      ; (pointer-u8-set! (pointer+ pixels 3) 255)
      ; (exit 0)
    )
  )
  (sdl2:unlock-texture! texture)
)

(define (main-loop renderer texture)
  (let ((done #f))
    (while (not done)
           (let ((ev (sdl2:poll-event!)))
             (when ev
               (case (sdl2:event-type ev)
                 ((quit)
                  (set! done #t))
                 ((key-down)
                  (case (sdl2:keyboard-event-sym ev)
                    ((escape)
                     (set! done #t))
                  )
                )
               )
             )
          )
          (sdl-update-fb texture)
          (sdl2:render-copy! renderer texture #f #f)
          (sdl2:render-present! renderer)
          (sdl2:delay! 100)
    )
  )
)

;; Bind arguments and run everything
(receive (options operands)
    (args:parse (command-line-arguments) opts)
    (let ((scale (or (alist-ref 'scale options) 4))
          (rom-path (if (null? operands)
                        (begin
                          (print-err "Missing rom-path")
                          (usage)
                          (exit 1))
                        (car operands))))
      (print "--scale -> " scale)
      (print "rom-path -> " rom-path)
      (sdl-init)
      (receive (window renderer texture)
               (sdl-setup scale)
               (sdl-update-fb texture)
               (main-loop renderer texture))
   )
)

(exit 0)
