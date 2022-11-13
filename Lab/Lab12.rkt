;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab12) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise (Reviewed) 1
;Design the function digits->num which takes a non-empty list of digits (i.e. natural numbers between 0 and 9) and
;combines all the digits into a single number in base-10. For example:

;digits->num: NELON -> Number
;as above
(check-expect (digits->num (list 1 2 3)) 123)
(check-expect (digits->num (list 0 2 3)) 23)
(check-expect (digits->num (list 1 2 0)) 120)

(define (digits->num nelon)
  (local[;digits->num/a: NELON Number -> Number
          ;combine all the digits into a single number in base-10
          ;Accumulator: current power of 10
          (define (digits->num/a nelon acc)
           (cond
             [(empty? nelon)0]
             [(cons? nelon)(+
                            (* (first nelon)
                               acc)
                               (digits->num/a (rest nelon) (/ acc 10)))]))]
    (digits->num/a nelon (expt 10 (sub1 (length nelon))))))

;Exercise (Reviewed) 2
;Design the function longest-streak which takes in a list of natural numbers and
;computes the largest number of times a single value appears in the list in a row.
;(The actual repeated value doesn’t matter; we only care how many times it repeats.) For example:

(check-expect (longest-streak (list 2 2 2)) 3)
(check-expect (longest-streak (list 1 2 1 2)) 2)
(check-expect (longest-streak (list 1 1 2 2 2 3 4 4)) 3)
(check-expect (longest-streak (list 1 2 2 2 2 3 4 4 4 4 5)) 4)

;longest-streak:  LON -> Number
;as above
(define (longest-streak lon)
  (cond
    [(empty? lon)0]
    [(cons? lon)(max(length(filter (λ(n)(= n (first lon))) lon))
                    (longest-streak (rest lon)))]))

;Exercise 3
;Define the template for functions that take in a Dir.
(require 2htdp/image)
(define WIDTH 600)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define LINE-LENGTH 10)
(define LINE-COLOR "black")
(define START-POSITION (make-posn 200 200))

; A Dir is one of:
; - "left"
; - "right"
; - "up"
; - "down"
; and represents one of the 4 possible directions

(define (dir-temp x)
  (cond
    [(= x "left")...]
    [(= x "right")...]
    [(= x "up")...]
    [(= x "down")...]))

;Exercise 4
;Write down the signatures and purpose statements for the handler functions you need. This is your "wishlist" of functions that you will need to create.
;Keep in mind that we will need to use a list of Dirs as our world state.

;Exercise 5
;Design the function move-posn that takes a Posn, a Dir, and a Number and produces that position shifted by the given amount in the given direction.
;For example, (move-posn (make-posn 1 2) "left" 3) would produce (make-posn -2 2).

;move-posn: Posn Dir Number -> Posn
;Takes a Posn, a Dir, and a Number and produces that position shifted by the given amount in the given direction
(check-expect (move-posn (make-posn 1 2) "left" 3) (make-posn -2 2))
(check-expect (move-posn (make-posn 1 2) "right" 3) (make-posn 4 2))
(check-expect (move-posn (make-posn 1 2) "up" 3) (make-posn 1 -1))
(check-expect (move-posn (make-posn 1 2) "down" 3) (make-posn 1 5))

(define (move-posn posn dir num)
  (cond
    [(string=? dir "left")(make-posn (- (posn-x posn) num) (posn-y posn))]
    [(string=? dir "right")(make-posn (+ (posn-x posn) num) (posn-y posn))]
    [(string=? dir "up")(make-posn (posn-x posn) (- (posn-y posn) num))]
    [(string=? dir "down")(make-posn (posn-x posn) (+ (posn-y posn) num))]))

;Exercise 6
;Design the function draw-from-start, which takes a [List-of Dir], a Posn, and an Image. It then adds lines starting at the given position and going in each direction.
;Each line should start where the last one ended. You can use the pre-defined function add-line to add lines to an image.
;Note: You should not use a list abstraction for this problem.

;move-lop: [List-of Dir] Posn -> Posn
;Takes a [List-of Dir] and original Posn then produce the final position.
(check-expect (move-lop (list "right" "up")(make-posn 200 200))
              (make-posn 203 197))

(define (move-lop lop p)
  (cond
    [(empty? lop)p]
    [(cons? lop)(move-lop (rest lop)
                          (move-posn p (first lop) 3))]))

;draw-from-start: [List-of Dir] Posn Image -> Image
;Adds lines starting at the given position and going in each direction

(check-expect (draw-from-start (list "right" "up") START-POSITION BACKGROUND)
              (add-line BACKGROUND
                        (posn-x START-POSITION)
                        (posn-y START-POSITION)
                        (posn-x (move-lop (list "right" "up") START-POSITION))
                        (posn-y (move-lop (list "right" "up") START-POSITION))
                        (make-pen LINE-COLOR LINE-LENGTH "solid" "round" "round")))

(define (draw-from-start lop p img)
  (add-line img
                        (posn-x p)
                        (posn-y p)
                        (posn-x (move-lop lop p))
                        (posn-y (move-lop lop p))
                        (make-pen LINE-COLOR LINE-LENGTH "solid" "round" "round")))

;Exercise 7 [INCOMPLETE: Can only draw from start to end position. Maybe need accumulator]
;Use draw-from-start to design the function for your to-draw clause.

;draw-from-start/v2: List-of Position -> Image

(define (draw-from-start/v2 lop)
  (local [(define (draw-from-start lop p img)
          (add-line img
                    (posn-x p)
                    (posn-y p)
                    (posn-x (move-lop lop p))
                    (posn-y (move-lop lop p))
                    (make-pen LINE-COLOR LINE-LENGTH "solid" "round" "round")))]
    (draw-from-start lop START-POSITION BACKGROUND)))

;Exercise 8
;Design the function for your on-key clause. When an arrow key is pressed, add the appropriate direction to the end of the list so far. When another key is pressed, nothing should happen.

;change: KeyEvent -> List-of Position
;as above
(check-expect (change (list "right" "left") "left")
              (list "right" "left" "left"))

(define (change lop a-key)
  (cond
    [(key=? a-key "left")  (append lop (list "left"))]
    [(key=? a-key "right") (append lop (list "right"))]
    [(key=? a-key "up")    (append lop (list "up"))]
    [(key=? a-key "down")  (append lop (list "down"))]
    [else lop]))

;Exercise 9 [INCOMPLETE: Can only draw from start to end position. Maybe need accumulator]
;Design the function etch-a-sketch which runs your line-drawing program.

(define (etch-a-sketch x)
  (big-bang x
    (to-draw draw-from-start/v2)
    (on-key change)))

#|[INCOMPLETE:Not fully understand accumulator and generative recursion]
Exercise 10 Design the function rotate-dir which takes a Dir and rotates it 90 degrees counter-clockwise.

Exercise 11 Design the function rotate-all-dirs which takes a [List-of Dir] and rotates every Dir in the given list 90 degrees counter-clockwise.

Exercise 12 Design the function generate-fractal-dirs which takes a natural number (representing the number of iterations left to draw) and a list of Dirs and performs the following algorithm:
If the number is zero, return the list unchanged.

If the number is positive, return a new modified list as follows:
Rotate every Dir in the input list

Reverse the list of rotated directions

Append this rotated/reversed list to the end of the input list

Recursively call the function with one less iteration

Exercise 13 Design the function for the to-draw clause which takes a natural number and draws that many iterations of the dragon fractal using the generate-fractal-dirs function and the drawing function from our line-drawing program. To start, you should give generate-fractal-dirs the initial list (list "down").

Exercise 14 Design the function for the on-key clause. Remember that your world state is a natural number. This number should increase by one if the up arrow key is pressed and decrease by one if the down arrow key is pressed. Otherwise nothing will happen.
|#