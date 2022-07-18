;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exe 10
;multiple-of-5: number -> boolean
;accepts a number and determines if it is a multiple 5
(define (multiple-of-5? Num)
  (integer? (/ Num 5)))

;Exe 11
;greet: string -> string string
;Define a constant GREETING which contains your greeting. Then, use it in a function that takes a name and outputs a full greeting.
(define GREETING "how are you?")
(define (greet Name)
  (string-append Name "," " " GREETING))

;Exe 12
;Call these functions in the interactions window. What happens if you call (multiple-of-5? "cat") or (greet #t)?
;(multiple-of-5? "cat"): expects a number, given "cat"
;(greet #t): string-append: expects a string, given #true

;Exe 13
;Run the stepper on your program. Follow it through to see how it evaluates the tests.

;Exe 14
;Incorrect. This is a logical error. Since the program is to multiply, we should use the symbol "*".

;Exe 15
;write clear example. (times-itself 2) = 2 * 2 = 4

;Exe 16
;fixing cost = $55m. How much % of net worth
;Given someone’s net worth, define a function that will determine what percentage of their net worth it would take to fix the water.
; percentage-NW: Number -> Number
; Given someone’s net worth, determine what percentage of their net worth it would take to fix the water.
(define FixCost 55000000)

;(define (percentage-NW x)
;  (...x....FixCost))

(check-expect (percentage-NW 5000000) (/ 5000000 FixCost))
(check-expect (percentage-NW 0) (/ 0 FixCost))

(define (percentage-NW x)
 (/ x FixCost))

;Exe 17
;Define the function absolute which consumes a number and produces the absolute value of that number (do not use abs).
;absolute: number -> number
;consumes a number and produces the absolute value of that number

;define (absolute x)
;  (If (x) (x) x))

(check-expect (absolute -12) 12)
(check-expect (absolute 12) 12)

(define (absolute x)
  (if (<= x 0) (- x) x))

;Exe 18
;Define the function letter-grade. It consumes a Score and produces the corresponding letter grade ("A", "B", "C", "D", or "F").
;You may be as wishful as desired in choosing your own grading scale.

; A Score is a number in the range [0, 100]  
; Interpretation: The marks student get in taking the exam

(define SC-100 100)
(define SC-80 80)
(define SC-70 70)
(define SC-60 60)
(define SC-49 49)
(define SC-150 150)


;(define (sc-temp sc)
;  (....sc...))

; A grade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "F"
; Interpretation: The grade awarded based on the exam's score

(define GD-A "A")
(define GD-B "B")
(define GD-C "C")
(define GD-D "D")
(define GD-F "F")

(define (gd-temp gd)
  (...
   (cond
     [(string=? gd GD-A) ...]
     [(string=? gd GD-B) ...]
     [(string=? gd GD-C) ...]
     [(string=? gd GD-D) ...]
     [(string=? gd GD-F) ...])))

;letter-grade: score -> grade
;take in the score of exam and award the grade.

(check-expect (letter-grade SC-100) GD-A) 
(check-expect (letter-grade SC-80) GD-B) 
(check-expect (letter-grade SC-70) GD-C) 
(check-expect (letter-grade SC-60) GD-D) 
(check-expect (letter-grade SC-49) GD-F)
(check-expect (letter-grade SC-150) "invalid") 

(define (letter-grade sc)
  (cond
     [(and (>= sc 90)(<= sc 100)) GD-A]
     [(and (>= sc 80)(<= sc 89)) GD-B]
     [(and (>= sc 70)(<= sc 79)) GD-C]
     [(and (>= sc 50)(<= sc 69)) GD-D]
     [(and (>= sc 0)(<= sc 49)) GD-F]
     [else "invalid"]))

;Exercise 19
;Define a function that given a number of hours worked an an hourly wage computes how much money has been earned.
;Any hours worked over 40 hours must be paid at 1.5 times the given rate.  

;Money-Earned: Number -> Number
;given a number of hours worked an an hourly wage computes how much money has been earned
;Any hours worked over 40 hours must be paid at 1.5 times the given rate.

(define RATE 1)
(define XRATE
  (* 1.5 RATE))

(check-expect (Money-Earned 0) 0)
(check-expect (Money-Earned -1) "invalid")
(check-expect (Money-Earned 40) 40)
(check-expect (Money-Earned 50) (+(* 1 40)(* 1.5 10)))

(define (Money-Earned sc)
  (cond
     [(and (>= sc 0)(<= sc 40)) (* sc RATE)]
     [(> sc 40) (+(* RATE 40) (* XRATE (- sc 40)))]
     [else "invalid"]))

;Exercise 20
;Define the function runtime that given a movie title outputs its runtime in minutes.
;Ensure the function works on a handful of inputs, and for others throws an error, which can be tested with check-error.

;A title is a string
;Interpretation: A title is a movie title

(define alient 120)
(define boiboi 100)
(define catwoman 60)

;(define (title-temp t)
;  .....t....)


;runtime: title -> Num
;given a movie title outputs its runtime in minutes

(check-expect (runtime alient) 120)
(check-expect (runtime boiboi) 100)
(check-expect (runtime catwoman) 60)
(check-error (runtime 0) "no such movie title")


(define (runtime t)
  (cond
     [(= t alient) 120]
     [(= t boiboi) 100]
     [(= t catwoman) 60]
     [else (error "no such movie title")]))

(require 2htdp/image)
(require 2htdp/universe)


;Exercise 21
;Use triangle, square, rectangle, above, and overlay/align to draw yourself a house with a roof and door
;(and circle if you’re feeling bold enough for a door handle).

(define roof
  (triangle 40 "solid" "tan"))

(define door
   (square 20 "solid" "slateblue"))

(define body
   (rectangle 40 40 "outline" "black"))

(define house (above roof body))

(define house2 (overlay/align "left" "bottom"
                 door
                 house))

;Exercise 22
;Congratulations, you’ve moved up a tax bracket. Define a constant WINDOW and place two of them on your humble home.
;Note how in using a constant we only have to draw it once and get to use it twice!

(define WINDOW
   (square 5 "outline" "green"))

(define house22 (overlay/align "left" "middle"
                 WINDOW
                 house2))

(define house22F (overlay/align "right" "middle"
                 WINDOW
                 house22))

;Exercise 23
;Define a function scene that takes in a natural number and places a circle of that radius at the center of a 50x50 empty-scene.
;Use modulo to ensure the radius always stays below 20.

;rshape: Number -> Scene

;(define (rshape x)
;  (place-image/align
;   (circle (if (<= x 20) x (+ 1 (modulo x 20))) "solid" "blue")
;   25 25 "center" "center"
;   (empty-scene 50 50)))

;(animate rshape)

;Exercise 25 (incomplete)
;Use cond (or if if you’re feeling cheeky) to make the circle grow and shrink in a loop instead of grow and suddenly dissapear and grow again.
;Since computing the radius is now a somewhat complex computation, it makes sense to write a seperate helper function which only computes the radius that scene calls.
;Hint: quotient, even?.

(define (rshape25 x)
  (place-image/align
   (circle (RADIUS x) "solid" "blue")
   25 25 "center" "center"
   (empty-scene 50 50)))

(define (RADIUS r)
  (cond
     [(<= r 5)  r]
     [(and (>= 6)(<= r 20)) (quotient r 20)]
     [else (+ 1 (modulo r 20)) ]))

(animate rshape25)
