;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;Exercise (Reviewed) 1

; A Shape is one of:
; - "circle"
; - "square"
; - "triangle"
; and represents a kind of shape

;Interpretation: Draw one of the shape enumerated above.

;(circle 30 "solid" "green")
;(square 30 "solid" "green")
;(triangle 30 "solid" "green")

;(define (shape-temp s)
;   (cond
;       [(string=? s "circle") (circle 30 "solid" "green")]
;       [(string=? s "square") (square 30 "solid" "green")]
;       [(string=? s "triangle") (triangle 30 "solid" "green")]))


;Exercise (Reviewed) 2
;Design a function draw which draws a Shape. Make use of your handy template from the past exercise,
;and don’t forget to follow every step of the design recipe for functions (signature, purpose statement, tests, code).

;draw: shape -> image
;Take in a string and draw the shape

(check-expect (shape "circle")(circle 30 "solid" "green"))
(check-expect (shape "square")(square 30 "solid" "green"))
(check-expect (shape "triangle")(triangle 30 "solid" "green"))
(check-error (shape "abc")"no such shape")

(define (shape s)
   (cond
       [(string=? "circle" s) (circle 30 "solid" "green")]
       [(string=? "square" s) (square 30 "solid" "green")]
       [(string=? "triangle" s) (triangle 30 "solid" "green")]
       [else (error "no such shape")]))

;Exercise 3
;Design a function draw-scene which overlays the image of a Shape on an empty-scene of a fixed size.
;Do you need to follow the Shape template here? (No)
;How many tests does it need? (3 tests)

;draw-scene1: shape -> image
;overlays the image of a Shape on an empty-scene of a fixed size
(define (draw-scene1 x)
  (overlay (shape x) BACK))

(define BACK (empty-scene 160 90))
(check-expect (draw-scene1 "circle")(overlay (circle 30 "solid" "green")BACK))
(check-expect (draw-scene1 "square")(overlay (square 30 "solid" "green")BACK))
(check-expect (draw-scene1 "triangle")(overlay (triangle 30 "solid" "green")BACK))



;Exercise 4
;Design a function next-shape which consumes a Shape and outputs the "next" Shape
;(any order is fine, so long as next-shape "cycles through" all the shapes.)

;next-shape: shape -> shape
;consumes a Shape and outputs the "next" Shape

(check-expect (next-shape "circle") "square")
(check-expect (next-shape "square") "triangle")
(check-expect (next-shape "triangle") "circle")

(define (next-shape s)
  (cond
       [(string=? "circle" s) "square"]
       [(string=? "square" s) "triangle"]
       [(string=? "triangle" s) "circle"]))


;Exercise 5
;Compose the draw-scene and next-shape functions in a big-bang animation that cycles through all of the shapes.

(big-bang "circle"
    [to-draw draw-scene1]
    [on-tick next-shape])

;Exercise 6
;2 would cause it to tick once every two seconds

(big-bang "circle"
    [to-draw draw-scene1]
    [on-tick next-shape 2])

;Exercise 7
;Wrap your call to big-bang in a main function, which takes a Shape and uses that shape as the initial state.
;Don’t forget to give it a signature and purpose statement,
;but you cannot test the main program. Why not? (Its sole reason for existing is that you can launch your world program conveniently from DrRacket’s interactions area)

;main: string -> world
;launches the program from some initial state

(define (main s)
  (big-bang s
    [to-draw draw-scene1]
    [on-tick next-shape 2]))

;Exercise 8
;Launch your animation by calling your main function from the interactions window.
(main "square")


;Exercise (Reviewed) 9
;Go ahead and copy the starter code into DrRacket and run the animation. 
; What condition is making the sun stop over the moon? (when the moon x-position is at 200)
;How does it work? (when the moon x-position is at 200, the is-total-eclipse function will return the boolean true and the world program will stop) 

(require 2htdp/image)
(require 2htdp/universe)
 
(define SKY (rectangle 400 200 "solid" "midnightblue"))
(define SUN (circle 50 "solid" "yellow"))
(define MOON (circle 50 "solid" "lightgray"))
(define SUN-AND-SKY (overlay SUN SKY))
(define MOON-SPEED 3)
 
; draw-eclipse : Number -> Image
; Draws the sky, with the moon at a given position x
(define (draw-eclipse x)
  (place-image MOON x 100 SUN-AND-SKY))
 
; move-moon : Number -> Number
; Produces the next position of the moon given the current position
(define (move-moon x)
  (+ x MOON-SPEED))
 
; is-total-eclipse? : Number -> Boolean
; Is the moon fully eclipsing the sun?
(define (is-total-eclipse? x)
  (>= x 200))
 
(big-bang 0
  [on-tick move-moon]
  [to-draw draw-eclipse]
  [stop-when is-total-eclipse?])

;Exercise (Reviewed) 10
;Change the MOON-SPEED constant to 3.
;What now happens when running the eclipse animation? (The eclipse does not happen as the moon continue to move across the sky)
;What is the bug? (The is-total-eclipse function does not return true as x will never become 200. The reason is that when the mooon speed is 
;a multiple of 3 and it will never become 200)
;How can you fix it? (>= x 200)

;Exercise 11
;Define three constants, TOP-SPEED, MID-SPEED, and LOW-SPEED which define how many meters/second Cornelius’ ship can move at each depth.

(define TOP-SPEED 5)
(define MID-SPPED 4)
(define LOW-SPEED 2)

;Exercise 12
;Define any other constants you might need for your program. Keep in mind that you will need to show the different ocean depths in different colors.

(define FIRST-100
  (rectangle 300 100 "solid" "light blue"))

(define NEXT-200
  (rectangle 300 200 "solid" "medium blue"))

(define LAST-100
  (rectangle 300 100 "solid" "dark blue"))

(define END-100
  (rectangle 300 100 "solid" "red"))


(define SEA
  (above FIRST-100
        NEXT-200
        LAST-100
        END-100))

(define SUB
  (rectangle 50 30 "solid" "black"))


;Exercise 13
;Design a data definition for a SeaLevel. This data definition should encompass the three non-overlapping adjacent intervals mentioned above.
;We will use this data definition as the world state for our program.
;It will allow us to determine which speed the ship should be moving as it descends through the murky depths of the ocean.
;Remember to follow all the steps of the data design recipe.

;A SeaLevel consist of 3 levels of depth.

;Intepretation: The ship will be decending at different speed based on the depth of sea level.
; - first-100: For the first 100 meters the ship will sink at a rate of 5 meters/second. 
; - Next-300: For the next 200 meters the ship will sink at a rate of 4 meters/second
; - Last-100: For the last 100 meters it will sink very carefully at a rate of only 2 meters/second

;(check-expect (SeaLevel 50) 5))
;(check-expect (SeaLevel 250) 4))
;(check-expect (SeaLevel 350) 2))
;(check-expect (SeaLevel 450) 0))

;(define (SeaLevel-temp x)
;   (cond
;      [(..FIRST-100..) TOP-SPEED]
;      [(...NEXT-200..) MID-SPPED]
;      [(...LAST-100...)LOW-SPEED]
;      [(> 400)0)]))

;Exercise 14
;Write down the signature and purpose statement for each handler function you will need.
;Every big-bang program needs a to-draw clause. 
;What other clause will be useful to change the location of the ship as time passes? (on-tick)
;Which clause will help us stop the program when the ship reaches the bottom of the traversable ocean? (stop-when)


;Exercise 15
;Design the function that draws the ship at the correct location on the screen.
;Your program should show the entire traversable ocean, with the different depths as different colors since the ocean gets darker as you descend.
;Which handler does this belong to? (to-draw)

;Sub-Sea: Number -> Image
;Draws the sea levels, with the submarine at a given position x
(define (Sub-Sea y)
  (place-image SUB 150 y SEA))

;Exercise 16
;Design the function that moves the ship as time passes. The ship should descend at the rates specified by Cornelius.
;Which handler does this belong to? (on-tick)

; move-sub : Number -> Number
; Produces the next position of the sub given the current position

(check-expect (move-sub 50)(+ 50 5))
(check-expect (move-sub 250)(+ 250 4))
(check-expect (move-sub 350)(+ 350 2))
(check-expect (move-sub 450)(+ 450 0))

(define (move-sub y)
  (+ y (SeaLevel-Speed y)))

;Helper function
;SeaLevel-Speed: Number -> Number
;Determine the sub decending speed based on the sea level.

(check-expect (SeaLevel-Speed 50)5)
(check-expect (SeaLevel-Speed 250)4)
(check-expect (SeaLevel-Speed 350)2)
(check-expect (SeaLevel-Speed 450)0)
(check-expect (SeaLevel-Speed -2)"invalid")

(define (SeaLevel-Speed y)
  (cond
      [(and (> y 0)(<= y 100)) TOP-SPEED]
      [(and (> y 100)(<= y 300)) MID-SPPED]
      [(and (> y 300)(<= y 400))LOW-SPEED]
      [(> y 400)0]
      [else "invalid"]))

;Exercise 17
;Design the function which determines whether or not the program should stop.
;Your program should end when the ship reaches the bottom of the safely traversable ocean.
;Which handler does this belong to? (stop-when)

; is-sea-bottom? : Number -> Boolean
; the ship reaches the bottom of the traversable ocean?

(check-expect (is-sea-bottom? 300) #false)
(check-expect (is-sea-bottom? 500) #true)

(define (is-sea-bottom? y)
  (> y 400))

;Exercise 18
;Write a function sinking-ship which, given the initial state of Cornelius’ submarine.

(define (sinking-ship y)
(big-bang y
  [to-draw Sub-Sea]
  [on-tick move-sub]
  [stop-when is-sea-bottom?]))

