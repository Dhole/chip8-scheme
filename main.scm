(import (chicken process-context)
        (chicken port)
        (chicken format)
        (chicken condition)
        (chicken memory)
        (chicken random)
        srfi-151
        defstruct
        args
        miscmacros
        modular-arithmetic
        (prefix sdl2 sdl2:))

;; Chip8

(define screen-width 64)
(define screen-heigth 32)
(define mem-size #x1000)
(define rom-addr #x200)
(define sprite-char-length 5)
(define sprite-chars-addr #x0000)
(define fb-len (/ (* screen-width screen-heigth) 8))

(define (+u8 a b)
  ((mod+ #x100) a b))
(define (-u8 a b)
  ((mod- #x100) a b))
(define (+u16 a b)
  ((mod+ #x10000) a b))

(defstruct chip8
           mem
           v
           i
           pc
           stack
           sp
           dt
           st
           keypad
           fb
           tone
           time
           rng)

(define (make-zero-vector length)
  (let ((v (make-vector length)))
    (let zero ((i 0))
      (unless (= i length)
        (begin
          (vector-set! v i 0)
          (zero (+ i 1)))))
    v))

(define (new-chip8)
  (let ((ch8 (make-chip8)))
    (chip8-mem-set! ch8 (make-zero-vector mem-size))
    (chip8-v-set! ch8 (make-zero-vector #x10))
    (chip8-i-set! ch8 0)
    (chip8-pc-set! ch8 rom-addr)
    (chip8-stack-set! ch8 (make-zero-vector #x10))
    (chip8-sp-set! ch8 0)
    (chip8-dt-set! ch8 0)
    (chip8-st-set! ch8 0)
    (chip8-keypad-set! ch8 0)
    (chip8-fb-set! ch8 (make-zero-vector fb-len))
    (chip8-tone-set! ch8 #f)
    (chip8-time-set! ch8 0)
    (chip8-rng-set! ch8 '())
    ch8
  )
)

(define (chip8-load-rom ch8 rom)
  (let ((mem (chip8-mem ch8))
        (rom-len (vector-length rom)))
    (let copy ((i 0))
      (unless (= i rom-len)
          (begin
            (vector-set! mem (+ rom-addr i) (vector-ref rom i))
            (copy (+ i 1))
          )
      )
    )
  )
)

;; Op: Clear the display.
(define (chip8-op-cls ch8)
  (let clear ((i 0))
    (unless (= i fb-len)
      (begin
        (vector-set! (chip8-fb ch8) i 0)
        (clear (+ i 1)))))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  109)

(define (chip8-op-call-rca-1802 ch8 addr)
  100)

;; Op: Return from a subroutine.
(define (chip8-op-ret ch8)
  (chip8-sp-set! ch8 (- (chip8-sp ch8) 1))
  (chip8-pc-set! ch8 (vector-ref (chip8-stack ch8) (chip8-sp ch8)))
  105)

;; Op: Jump to addr.
(define (chip8-op-jp ch8 addr)
  (chip8-pc-set! ch8 addr)
  105)

;; Op: Call subroutine at addr.
(define (chip8-op-call ch8 addr)
  (vector-set! (chip8-stack ch8) (chip8-sp ch8) (+u16 (chip8-pc ch8) 2))
  (chip8-sp-set! ch8 (+u8 (chip8-sp ch8) 1))
  (chip8-pc-set! ch8 addr)
  105)

; Op: Skip next instruction if a == b.
(define (chip8-op-se ch8 a b)
  (if (= a b)
      (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 4))
      (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2)))
  61)

; Op: Skip next instruction if a != b.
(define (chip8-op-sne ch8 a b)
  (if (not (= a b))
      (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 4))
      (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2)))
  61)

; Op: Set Vx = v.
(define (chip8-op-ld ch8 x v)
  (vector-set! (chip8-v ch8) x v)
  27)

; Op: Wait for a key press, store the value of the key in Vx.
(define (chip8-op-ld-vx-k ch8 x)
  (let test ((i 0))
    (unless (= i #x10)
      (if (bit-set? i (chip8-keypad ch8))
          (begin
            (vector-set! (chip8-v ch8) x i)
            (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2)))
          (test (+ i 1)))))
  200)

; Op: Set delay timer = Vx.
(define (chip8-op-ld-dt ch8 v)
  (chip8-dt-set! ch8 v)
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  45)

; Op: Set sound timer = Vx.
(define (chip8-op-ld-st ch8 v)
  (chip8-st-set! ch8 v)
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  45)

; Op: Set I = location of sprite for digit v.
(define (chip8-op-ld-f ch8 v)
  (chip8-i-set! ch8 (+u16 sprite-chars-addr (* v sprite-char-length)))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  91)

; Op: Store BCD representation of v in memory locations I, I+1, and I+2.
(define (chip8-op-ld-b ch8 v)
  (let* ((d2 (truncate (/ v 100)))
         (v1 (- v (* d2 100)))
         (d1 (truncate (/ v1 10)))
         (v2 (- v1 (* d1 10)))
         (d0 v2))
    (vector-set! (chip8-mem ch8) (+ (chip8-i ch8) 0) d2)
    (vector-set! (chip8-mem ch8) (+ (chip8-i ch8) 1) d1)
    (vector-set! (chip8-mem ch8) (+ (chip8-i ch8) 2) d0))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  927)

; Op: Store registers V0 through Vx in memory starting at location I.
(define (chip8-op-ld-i-vx ch8 x)
  (let ((mem (chip8-mem ch8))
        (v (chip8-v ch8))
        (ch8-i (chip8-i ch8)))
       (let copy ((i 0))
         (unless (= i (+ x 1))
           (begin
             (vector-set! mem (+ ch8-i i) (vector-ref v i))
             (copy (+ i 1))))))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  605)

; Op: Read registers V0 through Vx from memory starting at location I.
(define (chip8-op-ld-vx-i ch8 x)
  (let ((mem (chip8-mem ch8))
        (v (chip8-v ch8))
        (ch8-i (chip8-i ch8)))
       (let copy ((i 0))
         (unless (= i (+ x 1))
           (begin
             (vector-set! v i (vector-ref mem (+ ch8-i i)))
             (copy (+ i 1))))))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  605)

; Op: Set Vx = Vx + b.
(define (chip8-op-add ch8 x b)
  (let ((vx (vector-ref (chip8-v ch8) x)))
    (vector-set! (chip8-v ch8)
                 (if (> (+ vx b) #xff)
                     1
                     0)
                 #xf)
    (vector-set! (chip8-v ch8)
                 (+u8 vx b)
                 x))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set I = I + b.
(define (chip8-op-add16 ch8 b)
  (chip8-i-set! ch8 (+u16 (chip8-i ch8) b))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  86)

; Op: Set Vx = Vx OR b.
(define (chip8-op-or ch8 x b)
  (vector-set! (chip8-v ch8)
               (bitwise-ior (vector-ref (chip8-v ch8) x) b)
               x)
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set Vx = Vx AND b.
(define (chip8-op-and ch8 x b)
  (vector-set! (chip8-v ch8)
               (bitwise-and (vector-ref (chip8-v ch8) x) b)
               x)
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set Vx = Vx XOR b.
(define (chip8-op-xor ch8 x b)
  (vector-set! (chip8-v ch8)
               (bitwise-xor (vector-ref (chip8-v ch8) x) b)
               x)
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set Vx = Vx - b.
(define (chip8-op-sub ch8 x b)
  (let ((vx (vector-ref (chip8-v ch8) x)))
    (vector-set! (chip8-v ch8)
                 (if (> b vx)
                     1
                     0)
                 #xf)
    (vector-set! (chip8-v ch8)
                 (-u8 vx b)
                 x))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set Vx = b - Vx, set Vf = NOT borrow.
(define (chip8-op-subn ch8 x b)
  (let ((vx (vector-ref (chip8-v ch8) x)))
    (vector-set! (chip8-v ch8)
                 (if (> b vx)
                     0
                     1)
                 #xf)
    (vector-set! (chip8-v ch8)
                 (-u8 vx b)
                 x))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set Vx = Vx >> 1.
(define (chip8-op-shr ch8 x)
  (let ((vx (vector-ref (chip8-v ch8) x)))
    (vector-set! (chip8-v ch8)
                 (if (eq? (bitwise-and vx #x01) #x01)
                     1
                     0)
                 #xf)
    (vector-set! (chip8-v ch8)
                 (arithmetic-shift vx -1)
                 x))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set Vx = Vx << 1.
(define (chip8-op-shl ch8 x)
  (let ((vx (vector-ref (chip8-v ch8) x)))
    (vector-set! (chip8-v ch8)
                 (if (eq? (bitwise-and vx #x80) #x80)
                     1
                     0)
                 #xf)
    (vector-set! (chip8-v ch8)
                 (arithmetic-shift vx 1)
                 x))
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  200)

; Op: Set I = addr
(define (chip8-op-ld-i ch8 addr)
  (chip8-i-set! ch8 addr)
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  55)

; Op: Set Vx = random byte AND v
(define (chip8-op-rnd ch8 x v)
  (vector-set! (chip8-v ch8)
               (bitwise-and (pseudo-random-integer  #x100) v)
               x)
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  164)

; Op: Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
(define (chip8-op-drw ch8 pos-x pos-y n)
  (define collision 0)
  (let* ((pos-x (remainder pos-x 64))
         (pos-y (remainder pos-y 32))
         (shift (remainder pos-x 8))
         (col-a (truncate (/ pos-x 8)))
         (col-b (remainder (+ col-a 1) (/ screen-width 8)))
         (fb (chip8-fb ch8)))
         (let draw ((i 0))
           (let* ((byte (vector-ref (chip8-mem ch8) (+ (chip8-i ch8) i)))
                  (y (remainder (+ pos-y i) screen-heigth))
                  (a (arithmetic-shift byte (- 0 shift)))
                  (off_a (+ (/ (* y screen-width) 8) col-a))
                 )
                 (set! collision
                   (bitwise-ior collision (bitwise-and (vector-ref fb off_a) a)))
                 (vector-set! fb off_a (bitwise-xor (vector-ref fb off_a) a))
                 (when (not (= shift 0))
                   (let ((b (arithmetic-shift byte (+ -8 shift)))
                         (off_b (+ (/ (* y screen-width) 8) col-b)))
                        (set! collision
                          (bitwise-ior collision (bitwise-and (vector-ref fb off_b) b)))
                        (vector-set! fb off_b (bitwise-xor (vector-ref fb off_b) b)
                   )
                 )
             )
           (unless (= i n)
             (begin
               (vector-set! v i 0)
               (zero (+ i 1)))))
      )
      (vector-set! (chip8-v ch8)
                   (if (not (= collision 0))
                       1
                       0)
                   #xf)
  )
  (chip8-pc-set! ch8 (+u16 (chip8-pc ch8) 2))
  22734)

(define (test ch8)
  (chip8-op-cls ch8)
  (chip8-op-call-rca-1802 ch8 0)
  (chip8-op-call ch8 0)
  (chip8-op-ret ch8)
  (chip8-op-jp ch8 0)
  (chip8-op-se ch8 0 0)
  (chip8-op-sne ch8 0 0)
  (chip8-op-ld ch8 0 0)
  (chip8-op-ld-vx-k ch8 0)
  (chip8-op-ld-dt ch8 0)
  (chip8-op-ld-st ch8 0)
  (chip8-op-ld-f ch8 0)
  (chip8-op-ld-b ch8 2)
  (chip8-op-ld-i-vx ch8 2)
  (chip8-op-ld-vx-i ch8 2)
  (chip8-op-add ch8 0 0)
  (chip8-op-add16 ch8 0)
  (chip8-op-or ch8 0 0)
  (chip8-op-and ch8 0 0)
  (chip8-op-xor ch8 0 0)
  (chip8-op-sub ch8 0 0)
  (chip8-op-subn ch8 0 0)
  (chip8-op-shr ch8 0)
  (chip8-op-shl ch8 0)
  (chip8-op-ld-i ch8 0)
  (chip8-op-rnd ch8 0 0)
  (chip8-op-drw ch8 0 0 0)
)

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
            )
          )
        )
      )
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

(define (read-rom-file path)
  (define in (open-input-file path))
  (define rom (make-vector (- mem-size rom-addr)))
  (let copy ((i 0))
    (let ((b (read-char in)))
      (unless (eof-object? b)
        (begin
          ; (display b)
          (vector-set! rom i b)
          (copy (+ i 1))
        )
      )
    )
  )
  (close-input-port in)
  rom
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

      (define rom (read-rom-file rom-path))

      (define ch8 (new-chip8))
      (chip8-load-rom ch8 rom)
      (test ch8)
      (receive (window renderer texture)
               (sdl-setup scale)
               (sdl-update-fb texture)
               (main-loop renderer texture))
   )
)

(exit 0)
