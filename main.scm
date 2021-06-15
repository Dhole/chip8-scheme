(import (chicken process-context)
        (chicken port)
        (chicken format)
        (chicken condition)
        args
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

;; Setup SDL objects
;; Creates `window`
(define (sdl-setup scale)
  (define window
  (sdl2:create-window!
   "Chip8-scheme"                       ; title
   'centered  'centered                 ; x, y
   (* 64 scale)  (* 32 scale)           ; w, h
   '(shown opengl))                     ; flags
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
      (sdl-setup scale)
   )
)

(exit 0)
