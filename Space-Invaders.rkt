;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Space-Invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH 300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5) ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 20)

(define INVADE-RATE 100)

(define MTS (empty-scene WIDTH HEIGHT "MidnightBlue"))

(define INVADER
(overlay/xy (ellipse 10 15 "outline" "yellow") ;cockpit cover
-5 6
(ellipse 20 10 "solid" "yellow"))) ;saucer

(define TANK
(overlay/xy (overlay (ellipse 28 8 "solid" "black") ;tread center
(ellipse 30 10 "solid" "white")) ;tread outline
5 -14
(above (rectangle 5 10 "solid" "white") ;gun
(rectangle 20 10 "solid" "white")))) ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;; with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
(... (fn-for-loinvader (game-invaders s))
(fn-for-lom (game-missiles s))
(fn-for-tank (game-tank s))))

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;; the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1)) ;center going right
(define T1 (make-tank 50 1)) ;going right
(define T2 (make-tank 50 -1)) ;going left

#;
(define (fn-for-tank t)
(... (tank-x t) (tank-dir t)))

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;; the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12)) ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10)) ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
(... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvaders is one of:
;; - empty
;; - (cons Invader ListOfInvaders)
(define LI0 empty)
(define LI1 (cons I1 empty))
(define LI2 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
(cond [(empty? loi) (...)]
[else
(... (fn-for-invader (first loi))
(fn-for-loi (rest loi)))]))

;; Template Rules Used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListOfInvader

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300)) ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10))) ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1) 5))) ;> hit U1

#;
(define (fn-for-missile m)
(... (missile-x m) (missile-y m)))

;; ListOfMissles is one of:
;; - empty
;; - (cons Missle ListOfMissles)
(define LM0 empty)
(define LM1 (cons M1 empty))
(define LM2 (cons M1 (cons M2 empty)))

#;
(define (fn-for-lom lom)
(cond [(empty? lom) (...)]
[else
(... (fn-for-missle (first lom))
(fn-for-lom (rest lom)))]))

;; Template Rules Used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lom) is Missle
;; - self-reference: (rest lom) is ListOfMissles

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start space invader program by evaluating (main G0)
(define (main log)
(big-bang log
(stop-when stop?) ; Game -> Boolean
(on-key handle-key) ; Game KeyEvent -> Game
(on-tick next-game) ; Game -> Game
(to-draw render-game))) ; Game -> Image

;; Game -> Boolean
;; stops the game when a Invader reaches bottom of screen
(check-expect (stop? (make-game empty empty T0)) false)
(check-expect (stop? (make-game (list I1 I2) (list M1 M2) T1)) true)
(check-expect (stop? (make-game (list (make-invader 150 (+ HEIGHT 10) 10)) empty (make-tank 0 1))) true);exactly landed

; (define (stop? g) false) ;stub
(define (stop? g)
(check? (game-invaders g)))

;; ListOfInvader -> Boolean
;; Returns true is an invader in ListOfInvaders reaches HEIGHT
(check-expect (check? empty) false)
(check-expect (check? (cons (make-invader 150 HEIGHT 1.5) empty)) true)
(check-expect (check? (cons (make-invader 150 (+ 10 HEIGHT) -10) empty)) true)

;(define (check? i) false) ;stub

(define (check? loi)
(cond [(empty? loi) false]
[else
(if (>= (invader-y (first loi)) HEIGHT)
true
(check? (rest loi)))]))

;; Game KeyEvent -> Game
;; changes the dx of the tank from positive to negative if arrow keys are pressed
(check-expect (handle-key (make-game empty empty (make-tank 40 1)) "left") (make-game empty empty (make-tank 40 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 40 -1)) "right") (make-game empty empty (make-tank 40 1)))
(check-expect (handle-key (make-game empty empty (make-tank 40 -1)) " ") (make-game empty
(cons (make-missile 40 (- HEIGHT TANK-HEIGHT/2)) empty)
(make-tank 40 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 40 -1)) "a") (make-game empty empty (make-tank 40 -1)))
;(define (handle-key g ke) (make-game empty empty T0)) ;stub

(define (handle-key g ke)
(cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
[(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
[(key=? ke " ") (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g)) (game-tank g))]
[else g]))

;; Game -> Game
;; Changes the positions of all elements in ListOfInvaders, and ListOfMissles and tank
(check-expect (next-game (make-game empty empty T0)) (make-game empty empty (make-tank (+ TANK-SPEED (/ WIDTH 2)) 1)))
(check-expect (next-game (make-game (list (make-invader 40 0 1.5)) empty (make-tank (/ WIDTH 2) 1)))
(make-game (list (make-invader (+ INVADER-X-SPEED 40) 1.5 1.5)) empty (make-tank (+ TANK-SPEED (/ WIDTH 2)) 1)))
(check-expect (next-game (make-game (list (make-invader WIDTH 20 1.5)) (list (make-missile 40 50)) (make-tank (/ WIDTH 2) 1)))
(make-game (list (make-invader WIDTH 21.5 -1.5)) (list (make-missile 40 (- 50 MISSILE-SPEED))) (make-tank (+ TANK-SPEED (/ WIDTH 2)) 1)))

;(define (next-game g) (make-game empty empty T0)) ;stub

(define (next-game g)
(make-game (main-invaders (game-invaders g) (game-missiles g))
(main-missiles (game-missiles g))
(next-tank (game-tank g))))

;; ListOfInvaders -> ListOfInvaders
;; produce filtered and ticked list of invaders
(check-expect (main-invaders empty empty) empty)
(check-expect (main-invaders (list (make-invader 4 4 1.5) (make-invader 5 60 1.5))
(list (make-missile 4 14)))
(list (make-invader 6.5 61.5 1.5)))
;<template as function composition>

(define (main-invaders loi lom)
(nothit-only (next-invaders loi) (main-missiles lom)))

;; ListOfInvaders -> ListOfInvaders
;; advances the positions of all invaders and creates new invaders every random ticks
(check-random (next-invaders empty) (if (spawn? (random INVADE-RATE))
(cons (make-invader (random WIDTH) 0 1.5) empty)
empty))
(check-random (next-invaders (list (make-invader 20 0 1.5)
(make-invader (- WIDTH 1) 10 1.5)))
(list (make-invader 21.5 1.5 1.5)
(make-invader WIDTH 11.5 -1.5)))
;(define (next-invaders i) empty) ;stub

(define (next-invaders loi)
(cond [(empty? loi) (if (spawn? (random INVADE-RATE))
(cons (make-invader (random WIDTH) 0 1.5) empty)
empty)]
[else
(cons (next-invader (first loi))
(next-invaders (rest loi)))]))

;; Invader -> Invader
;; advaces an invader
(check-expect (next-invader (make-invader 0 0 1.5)) (make-invader 1.5 1.5 1.5))
(check-expect (next-invader (make-invader (- WIDTH 1) 10 1.5)) (make-invader WIDTH 11.5 -1.5))
(check-expect (next-invader (make-invader 1 200 -1.5)) (make-invader 0 201.5 1.5))

;(define (next-invader i) empty) ;stub

(define (next-invader i)
(cond [(> (+ (invader-x i) (invader-dx i)) WIDTH) (make-invader WIDTH (+ INVADER-Y-SPEED (invader-y i)) (- (invader-dx i)))]
[(< (+ (invader-x i) (invader-dx i)) 0) (make-invader 0 (+ INVADER-Y-SPEED (invader-y i)) (- (invader-dx i)))]
[else
(make-invader (+ (invader-x i) (invader-dx i)) (+ INVADER-Y-SPEED (invader-y i)) (invader-dx i))]))

;; Natural -> Boolean
;; tells when to spawn invader. there's a 1/100 chance it will spawn every tick
(check-random (spawn? (random INVADE-RATE)) (equal? (random INVADE-RATE) (random INVADE-RATE)))
;(define (spawn? n) false) ;stub
(define (spawn? n)
(equal? n (random INVADE-RATE)))

;; ListOfMissiles -> ListOfMissiles
;; produce filtered and ticked list of missiles
(check-expect (main-missiles empty) empty)
(check-expect (main-missiles (cons (make-missile 3 14)
(cons (make-missile 90 -1)
empty)))
(list (make-missile 3 (- 14 MISSILE-SPEED))))
;(define (main-missiles lom) empty); stub
;<template as function composition>
(define (main-missiles lom)
(onscreen-only (next-missiles lom)))

;; ListOfMissiles -> ListOfMissiles
;; advances the postions of all misiles
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (cons (make-missile 3 4)
(cons (make-missile 90 100)
empty)))
(cons (make-missile 3 (- 4 MISSILE-SPEED))
(cons (make-missile 90 (- 100 MISSILE-SPEED))
empty)))
;(define (next-missiles m) empty) ;stub

;<template from ListOfMissiles>
(define (next-missiles lom)
(cond [(empty? lom) empty]
[else
(cons (next-missile (first lom))
(next-missiles (rest lom)))]))

;; Missile -> Missile
;; produce a new missile that is MISSILE-SPEED pixel farther down the screen
(check-expect (next-missile (make-missile 6 20)) (make-missile 6 (- 20 MISSILE-SPEED)))
;(define (next-missile m) (make-missile 0 0))
;<template from Missile>
(define (next-missile m)
(make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile -> ListOfMissile
;; produce a list containing only those missiles in lod that are onscreen?
(check-expect (onscreen-only empty) empty)
(check-expect (onscreen-only (cons (make-missile 3 4)
(cons (make-missile 1 -1)
empty)))
(cons (make-missile 3 4)
empty))

; (define (onescreen-only lom) empty)
;<template from ListOfMissile>
(define (onscreen-only lom)
(cond [(empty? lom) empty]
[else
(if (onscreen? (first lom))
(cons (first lom) (onscreen-only (rest lom)))
(onscreen-only (rest lom)))]))

;; Missile -> Boolean
;; produce true if missile has not reached 0(fallen off screen)
(check-expect (onscreen? (make-missile 2 -1)) false)
(check-expect (onscreen? (make-missile 2 0)) true)
(check-expect (onscreen? (make-missile 2 1)) true)

; (define (onscreen? m) false)
;<template from Missile>
(define (onscreen? m)
(<= 0 (missile-y m)))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; produce a list containing only those invaders in loi that are not-hit?
(check-expect (nothit-only empty empty) empty)
(check-expect (nothit-only (cons (make-invader 14 4 1.5) empty)
(list (make-missile 4 14)))
empty)
(check-expect (nothit-only (cons (make-invader 4 4 1.5) empty)
empty)
(cons (make-invader 4 4 1.5) empty))

;(define (nothit-only loi lom) empty)

(define (nothit-only loi lom)
(cond [(empty? loi) empty]
[(empty? lom) loi]
[else
(if (not-hit? (first loi) (first lom))
(cons (first loi) (nothit-only (rest loi) (rest lom)))
(nothit-only (rest loi) (rest lom)))]))

;; Invader Missile -> Boolean
;; produces true if invader is not hit by a missle
(check-expect (not-hit? (make-invader 14 4 1.5)
(make-missile 4 14))
false)
(check-expect (not-hit? (make-invader 4 4 1.5)
(make-missile 14 9))
false)
(check-expect (not-hit? (make-invader 4 4 1.5)
(make-missile 4 50))
true)
;(define (not-hit? i m) false) ;stub
(define (not-hit? i m)
(not (and (<= (- (missile-y m) HIT-RANGE) (invader-y i) (+ (missile-y m) HIT-RANGE))
(<= (- (missile-x m) HIT-RANGE) (invader-x i) (+ (missile-x m) HIT-RANGE)))))

;; Tank -> Tank
;; advances the position of the tank
(check-expect (next-tank (make-tank 0 1)) (make-tank TANK-SPEED 1))
(check-expect (next-tank (make-tank 0 -1)) (make-tank 0 1))
(check-expect (next-tank (make-tank WIDTH 1)) (make-tank WIDTH -1))
;(define (next-tank t) (make-tank 0 1)) ;stub

(define (next-tank t)
(cond [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH) (make-tank WIDTH (- (tank-dir t)))]
[(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0) (make-tank 0 (- (tank-dir t)))]
[else
(make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))

;; Game -> Image
;; renders the game as an image onto MTS
(check-expect (render-game (make-game empty empty (make-tank 0 1))) (place-image TANK 0 (- HEIGHT TANK-HEIGHT/2) MTS))
(check-expect (render-game (make-game (list (make-invader 0 0 1.5)) empty (make-tank 0 1)))
(place-image INVADER 0 0 (place-image TANK 0 (- HEIGHT TANK-HEIGHT/2) MTS)))
(check-expect (render-game (make-game (list (make-invader 0 0 1.5)) (list (make-missile 0 TANK-HEIGHT/2)) (make-tank 0 1)))
(place-image INVADER 0 0 (place-image MISSILE 0 TANK-HEIGHT/2 (place-image TANK 0 (- HEIGHT TANK-HEIGHT/2) MTS))))

;(define (render-game g) (square 1 "solid" "white"));stub

;<template from ListOfGame>
(define (render-game g)
(render-invaders (game-invaders g)
(render-missiles (game-missiles g)
(render-tank (game-tank g)))))

;; ListOfInvaders Image -> Image
;; renders the invaders into an (missiles) image
(check-expect (render-invaders empty MISSILE) MISSILE)
(check-expect (render-invaders (list (make-invader 0 0 1.5)) MISSILE) (place-image INVADER 0 0 MISSILE))

;(define (render-invaders loi img) MISSILE)

(define (render-invaders loi img)
(cond [(empty? loi) img]
[else
(place-invader (first loi)
(render-invaders (rest loi) img))]))

;; Invader Image -> Image
;; place Invader on img as specified by i
(check-expect (place-invader (make-invader 9 5 1.5) MTS)
(place-image INVADER 9 5 MTS))

; (define (place-invader i img) MTS)
;<template from Invaders w/ extra atomic parameter>
(define (place-invader i img)
(place-image INVADER (invader-x i) (invader-y i) img))

;; ListOfMissiles Image -> Image
;; renders the missiles into an (tank) image
(check-expect (render-missiles empty TANK) TANK)
(check-expect (render-missiles (list (make-missile 0 0)) TANK) (place-image MISSILE 0 0 TANK))

; (define (render-missiles lom img) TANK)
(define (render-missiles lom img)
(cond [(empty? lom) img]
[else
(place-missile (first lom)
(render-missiles (rest lom) img))]))

;; Missile Image -> Image
;; place Missile on img as specified by i
(check-expect (place-missile (make-missile 9 5) MTS)
(place-image MISSILE 9 5 MTS))
; (define (place-missile lom img) MTS)
(define (place-missile m img)
(place-image MISSILE (missile-x m) (missile-y m) img))

;; Tank -> Image
;; render the tank image at appropriate place on MTS
(check-expect (render-tank (make-tank 10 1)) (place-image TANK 10 (- HEIGHT TANK-HEIGHT/2) MTS))

;(define (render t) MTS) ;stub

;<use template from Tank>
(define (render-tank t)
(place-image TANK (tank-x t)
(- HEIGHT TANK-HEIGHT/2)
MTS))