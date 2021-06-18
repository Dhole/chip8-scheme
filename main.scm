(import (chicken process-context)
        (chicken port)
        (chicken format)
        (chicken condition)
        (chicken memory)
        defstruct
        args
        miscmacros
        (prefix sdl2 sdl2:))

;; Chip8

(define screen-width 64)
(define screen-heigth 32)
(define mem-size #x1000)
(define rom-addr #x200)
(define fb-len (/ (* screen-width screen-heigth) 8))

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

(define (new-chip8)
  (let ((ch8 (make-chip8)))
    (chip8-mem-set! ch8 (make-vector mem-size))
    (chip8-v-set! ch8 (make-vector #x10))
    (chip8-i-set! ch8 0)
    (chip8-pc-set! ch8 rom-addr)
    (chip8-stack-set! ch8 (make-vector #x10))
    (chip8-sp-set! ch8 0)
    (chip8-dt-set! ch8 0)
    (chip8-st-set! ch8 0)
    (chip8-keypad-set! ch8 0)
    (chip8-fb-set! ch8 (make-vector fb-len))
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
  (chip8-pc-set! ch8 (+ (chip8-pc ch8) 2))
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
  (vector-set! (chip8-stack ch8) (chip8-sp ch8) (+ (chip8-pc ch8) 2))
  (chip8-sp-set! ch8 (+ (chip8-sp ch8) 1))
  (chip8-pc-set! ch8 addr)
  105)

; Op: Skip next instruction if a == b.
(define (chip8-op-se ch8 a b)
  (if (= a b)
      (chip8-pc-set! ch8 (+ (chip8-pc ch8) 4))
      (chip8-pc-set! ch8 (+ (chip8-pc ch8) 2)))
  61
)

; Op: Skip next instruction if a != b.
(define (chip8-op-sne ch8 a b)
  (if (not (= a b))
      (chip8-pc-set! ch8 (+ (chip8-pc ch8) 4))
      (chip8-pc-set! ch8 (+ (chip8-pc ch8) 2)))
  61
)

; Op: Set Vx = v.
(define (chip8-op-ld ch8 x v)
  (vector-set! (chip8-v ch8) x v)
  27
)
; /// Op: Wait for a key press, store the value of the key in Vx.
; fn op_ld_vx_k(self: *Self, x: u8) usize {
;     var i: u8 = 0;
;     while (i < 0x10) : (i += 1) {
;         if (testBit(self.keypad, i)) {
;             self.v[x] = i;
;             self.pc += 2;
;             break;
;         }
;     }
;     return 200;
; }

; Op: Set delay timer = Vx.
(define (chip8-op-ld-dt ch8 v)
  (chip8-dt-set! ch8 v)
  (chip8-pc-set! ch8 (+ (chip8-pc ch8) 2))
  45)

; Op: Set sound timer = Vx.
(define (chip8-op-ld-st ch8 v)
  (chip8-st-set! ch8 v)
  (chip8-pc-set! ch8 (+ (chip8-pc ch8) 2))
  45)

(define (test ch8)
  (chip8-op-cls ch8)
  (chip8-op-call-rca-1802 ch8 0)
  (chip8-op-call ch8 0)
  (chip8-op-ret ch8)
  (chip8-op-jp ch8 0)
  (chip8-op-se ch8 0 0)
  (chip8-op-sne ch8 0 0)
  (chip8-op-ld ch8 0 0)
  (chip8-op-ld-dt ch8 0)
  (chip8-op-ld-st ch8 0)
)

; /// Op: Set I = location of sprite for digit v.
; fn op_ld_f(self: *Self, v: u8) usize {
;     self.i = SPRITE_CHARS_ADDR + v * @as(u16, SPRITE_CHAR_LEN);
;     self.pc += 2;
;     return 91;
; }

; /// Op: Store BCD representation of v in memory locations I, I+1, and I+2.
; fn op_ld_b(self: *Self, _v: u8) usize {
;     var v = _v;
;     const d2 = v / 100;
;     v = v - d2 * 100;
;     const d1 = v / 10;
;     v = v - d1 * 10;
;     const d0 = v / 1;
;     self.mem[self.i + 0] = d2;
;     self.mem[self.i + 1] = d1;
;     self.mem[self.i + 2] = d0;
;     self.pc += 2;
;     return 927;
; }
;
; /// Op: Store registers V0 through Vx in memory starting at location I.
; fn op_ld_i_vx(self: *Self, x: u8) usize {
;     var i: usize = 0;
;     while (i < x + 1) : (i += 1) {
;         self.mem[self.i + i] = self.v[i];
;     }
;     self.pc += 2;
;     return 605;
; }
; /// Op: Read registers V0 through Vx from memory starting at location I.
; fn op_ld_vx_i(self: *Self, x: u8) usize {
;     var i: usize = 0;
;     while (i < x + 1) : (i += 1) {
;         self.v[i] = self.mem[self.i + i];
;     }
;     self.pc += 2;
;     return 605;
; }
; /// Op: Set Vx = Vx + b.
; fn op_add(self: *Self, x: u8, b: u8) usize {
;     const overflow = @addWithOverflow(u8, self.v[x], b, &self.v[x]);
;     self.v[0xf] = if (overflow) 1 else 0;
;     self.pc += 2;
;     return 45;
; }

; Op: Set I = I + b.
(define (chip8-op-add16 b)
  (chip8-i-set! ch8 (+ (chip8-i ch8) b))
  (chip8-pc-set! ch8 (+ (chip8-pc ch8) 2))
  86)

; /// Op: Set Vx = Vx OR b.
; fn op_or(self: *Self, x: u8, b: u8) usize {
;     self.v[x] |= b;
;     self.pc += 2;
;     return 200;
; }
; /// Op: Set Vx = Vx AND b.
; fn op_and(self: *Self, x: u8, b: u8) usize {
;     self.v[x] &= b;
;     self.pc += 2;
;     return 200;
; }
; /// Op: Set Vx = Vx XOR b.
; fn op_xor(self: *Self, x: u8, b: u8) usize {
;     self.v[x] ^= b;
;     self.pc += 2;
;     return 200;
; }
; /// Op: Set Vx = Vx - b.
; fn op_sub(self: *Self, x: u8, b: u8) usize {
;     const overflow = @subWithOverflow(u8, self.v[x], b, &self.v[x]);
;     self.v[0xf] = if (overflow) 1 else 0;
;     self.pc += 2;
;     return 200;
; }
; /// Op: Set Vx = b - Vx, set Vf = NOT borrow.
; fn op_subn(self: *Self, x: u8, b: u8) usize {
;     const overflow = @subWithOverflow(u8, self.v[x], b, &self.v[x]);
;     self.v[0xf] = if (overflow) 0 else 1;
;     self.pc += 2;
;     return 200;
; }
; /// Op: Set Vx = Vx >> 1.
; fn op_shr(self: *Self, x: u8) usize {
;     const overflow = shr1WithOverflow(u8, self.v[x], &self.v[x]);
;     self.v[0xf] = if (overflow) 1 else 0;
;     self.pc += 2;
;     return 200;
; }
; /// Op: Set Vx = Vx << 1.
; fn op_shl(self: *Self, x: u8) usize {
;     const overflow = @shlWithOverflow(u8, self.v[x], 1, &self.v[x]);
;     self.v[0xf] = if (overflow) 1 else 0;
;     self.pc += 2;
;     return 200;
; }
; /// Op: Set I = addr
; fn op_ld_i(self: *Self, addr: u16) usize {
;     self.i = addr;
;     self.pc += 2;
;     return 55;
; }
; /// Op: Set Vx = random byte AND v
; fn op_rnd(self: *Self, x: u8, v: u8) usize {
;     self.v[x] = (self.rng.random.int(u8)) & v;
;     self.pc += 2;
;     return 164;
; }

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
