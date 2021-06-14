(import (chicken process-context)
        (chicken port)
        args
        format
        sdl2)

(define print-err
  (lambda s
    (with-output-to-port (current-error-port)
      (lambda ()
        (apply print s)))))

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

(define (usage)
 (print-err "Usage: " (car (argv)) " [options...] rom-path")
 (print-err "\n")
 (print-err (args:usage opts))
 (exit 1))

(receive (options operands)
    (args:parse (command-line-arguments) opts)
    (let ((scale (alist-ref 'scale options'))
          (rom-path (if (null? operands)
                        (begin
                          (print-err "Missing rom-path")
                          (usage)
                          (exit 1))
                        (car operands))))
      (print "--scale -> " (or scale 4))
      (print "rom-path -> " rom-path)))

(exit 0)
