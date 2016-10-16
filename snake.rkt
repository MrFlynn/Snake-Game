;; Isabel Whittaker-Walker
;; Nick Pleatsikas nick@pleatsikas.me
;; March 13 2013

(require 2htdp/image)
(require 2htdp/universe)

;; A seg is a (make-seg x y)
(define-struct seg (x y))
;; An x is the x-coordinate of the seg in logical coordinates.
;; A y is the y-coordinate of the seg in logical coordinates.
;; Example:
(define segEX1 (make-seg 33 40))

;; a Body is either a...
;; - (cons seg Body)
;; - (cons seg empty)
;; Example:
(define BodyEX1 (list (make-seg 40 30)
                      (make-seg 39 30)))

;; A world is a (make-worm body num num)
(define-struct worm (Body x-loc y-loc x-vel y-vel))
;; A body is a list-of-segments
;; x-vel is the velocity of the body on the x-axis.
;; y-vel is the velocity of the body on the y-axis.
;; loc is the location of the food in logical coordinates.
;; Examples:
(define wormEX1 (make-worm BodyEX1 20 20 1 0))

;;Constants
(define W 250)
(define H 250)
(define SEG-R 2.5)
(define FOOD-R 2.5)
(define LOG 5)

;; A logical coordinate, is one movement of the snake in any direction, 
;; it is 5 px in actual coordinates

(define BACK (empty-scene W H))
(define FOOD (circle FOOD-R "solid" "green"))
(define SEG (circle SEG-R "solid" "red"))

;; Wish List
;; Tick Function
;; snake-hit-side?
;; snake-eat-food?
;; snake-hit-self?
;; snake-grow
;; move-snake
;; end-world
;; Draw Function
;; Key

;; move-worm : worm -> worm
;; Takes a worm and returns in a new location, does not deal with key inputs
(check-expect (move-worm (make-worm (list (make-seg 3 12)
                                          (make-seg 4 12)
                                          (make-seg 5 12)
                                          (make-seg 5 13)
                                          (make-seg 5 14)
                                          )
                                    15 16
                                    -1 0))
              (make-worm (list 
                          (make-seg 2 12)
                          (make-seg 3 12)
                          (make-seg 4 12)
                          (make-seg 5 12)
                          (make-seg 5 13)
                         15 16
                         -1 0)))


(define (move-worm w)
  [(empty? (rest (worm-Body w)))
   (make-worm (make-seg
               (+ (worm-x-vel w) (first (worm-Body (seg-x w))))
               (+ (worm-y-vel w) (first (worm-Body (seg-y w)))))
              (worm-x-loc w)
              (worm-y-loc w)
              (worm-x-vel w)
              (worm-y-vel w))]
  [(cons? (rest (worm-Body w)))
   (make-worm (list
               (make-seg
                (+ (worm-x-vel w) (first (worm-Body (seg-x w))))
                (+ (worm-y-vel w) (first (worm-Body (seg-y w)))))
               (reverse (rest (reverse (worm-Body w)))))
              (worm-x-loc w)
              (worm-y-loc w)
              (worm-x-vel w)
              (worm-y-vel w))])



;; handle-tick : worm -> worm
;; Takes a worm and moves the worm one logical square and iff the worm eats
;; some food, it returns a longer 
(define (handle-tick w)
  (cond
    [(snake-eat-food? w) (increase-size w)]
    [else (move-worm w)]))

;; draw-worm: Worm -> Image
;; Takes a worm an returns an image of the worm.
(define (draw-worm w)
  (cond
    [(empty? (rest (worm-Body w)))]
    [(cons? (rest (worm-Body w)))]))

;; worm-handle-key: Worm KE -> Worm
;; Takes a worm and a key-event and either returns a worm heading in the
;; direction of the key that is pressed.
(check-expect (worm-handle-key (make-seg 33 3 -1 0)
                               "up")
              (make-seg 33 3 0 -1))
(check-expect (worm-handle-key (make-seg 33 3 -1 0)
                               "down")
              (make-seg 33 3 0 1))
(check-expect (worm-handle-key (make-seg 33 3 -1 0)
                               "left")
              (make-seg 33 3 -1 0))
(check-expect (worm-handle-key (make-seg 33 3 -1 0)
                               "right")
              (make-seg 33 3 1 0))
(check-expect (worm-handle-key (make-seg 33 3 -1 0)
                               "z")
              (make-seg 33 3 -1 0))

(define (worm-handle-key w ke)
  (cond
    [(key=? ke "up") (turn-worm-up w)]
    [(key=? ke "down") (turn-worm-down w)]
    [(key=? ke "left") (turn-worm-left w)]
    [(key=? ke "right") (turn-worm-right w)]
    [else w]))

;; snake-hit-side? : Snake -> Boolean
;; returns true if the snake has hit the side
(check-expect (worm-hit-side? (make-seg 33 33 -1 0))
              false)
(check-expect (worm-hit-side? (make-seg 1 33 1 0))
              false)
(check-expect (worm-hit-side? (make-seg 5 1 0 -1))
              true)
(check-expect (worm-hit-side? (make-seg 1 7 -1 0))
              true)
(check-expect (worm-hit-side? (make-seg 0 49 0 1))
              true)
(check-expect (worm-hit-side? (make-seg 49 27 1 0))
              true)

(define (worm-hit-side? s)
  (or
   (and (<= (first(seg-y s) 1)
            (= (seg-y-vel s) -1))
        (and (<= (seg-x s) 1)
             (= (seg-x-vel s) -1))
        (and (>= (seg-y s) 49)
             (= (seg-y-vel s) 1))
        (and (>= (seg-x s) 49)
             (= (seg-x-vel s) 1)))))

;; snake-hit-self? : worm --> Boolean
;; Takes a snake and if it is hitting itself, it returns true.
#;(define (snake-hit-self w))

;; worm-main : worm -> worm
(define (worm-main start)
  (big-bang start
            (on-tick worm-handle-tick)
            (on-key worm-handle-key)
            (to-draw draw-worm)
            (stop-when 
             worm-hit-side?)))

;; worm-start
(define worm-start (make-seg 20 20 1 0))
