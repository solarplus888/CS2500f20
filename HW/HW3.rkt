;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise 1
;For each definition, write down:
;The names of the functions (constructors, selectors, and predicates) that each definition introduces
;A reasonable data definition
;An interpretation
;Three examples
;The function template for that data definition

(define-struct book [title author year])

;A book is make-book (String String Real).
;title - title of the book
;author - author of the book
;year - year of publish of the book

;Interpretation: A book consist of the title, author and year of publish.

;construtor
;make-book: String String Real -> book

;predicate
;book? = Any-> Boolean

;selectors
;book-title : book -> String
;book-author : book -> String
;book-year : book -> Real

(define BOOK1 (make-book "Romance of Monkey" "Ali" 1900))
(define BOOK2 (make-book "How to win" "Bruce.W" 1980))
(define BOOK3 (make-book "888" "Cai Sheng" 1988))

(define (book-temp x)
  (...(book-title x)...
     (book-author x)...
     (book-year x)...))

(define-struct date [day month year])

; A date is make-date (Real Real Real)
; day - 2 digits : 01 to 30 or 31 days
; month - 2 digits from 01 to 12 month
; year - 4 digits

;Interpretation: a date is day, month and year.

;construtor:
;make-date: Real Real Real -> date

;predicate:
;date? = Any -> Boolean

;selectors:
;date-day : date -> Real
;date-month : date -> Real
;date-year : date -> Real

(define date1 (make-date 01 01 1901))
(define date2 (make-date 01 01 1902))
(define date3 (make-date 01 01 1903))

(define (date-temp x)
  (...(date-day x)...
      (date-month x)...
      (date-year x)...))


(define-struct holiday [title date is-observed])

;A holiday is make-holiday (String String Boolean)
;title - holiday description
;date - DDMMYY
;is-observed - holiday is approved or not

;Interpretation: Describe reason of holiday, the date applied and whether approved or not.

;constructor:
;make-holiday: String String Boolean -> holiday

;predicate:
;holiday?= : key -> Boolean

;selector:
;holiday-title : holiday -> String
;holiday-date : holiday -> String
;holiday-is-observed : holiday -> Boolean

(define holiday1 (make-holiday "Sick leave" "010122" "Y"))
(define holiday2 (make-holiday "Sick leave" "110122" "N"))
(define holiday3 (make-holiday "Travelling to USA" "010422" "Y"))

(define (holiday-temp x)
  (...(holiday-title x)...
      (holiday-date x)...
      (holiday-is-observed x)...))


;Exercise 2a
;Design a new data definition called Time, which represents the time of day in hours and minutes.

(define-struct time [hours minutes])

;A time is make-time [Real Real]
; hours is 12 hours from 1 - 12
; minutes is 60 minutes from 0 - 59
;Interpretation: hours and minutes of a clock.

(define time1 (make-time 01 00))
(define time2 (make-time 01 01))
(define time3 (make-time 01 59))
(define time4 (make-time 02 00))
(define time5 (make-time 13 62))
(define time6 (make-time 12 59))
(define time7 (make-time 12 00))

(define (time-temp x)
  (...(time-hours x)...
      (time-minutes x)...))

;Exercise 2b
;Design a function that consumes a Time, t, and produces a Time that represents the next minute immediately after t.

;clock: time -> time
;consumes a Time, t, and produces a Time that represents the next minute immediately after t

(check-expect (clock time1) time2)
(check-expect (clock time3) time4)
(check-expect (clock time6) time1)
(check-expect (clock time5) "hours is 12 hours from 1 - 12 ; minutes is 60 minutes from 0 - 59")


(define (clock t)
  (cond
    [(and (>= (time-hours t) 1)(< (time-hours t) 12)(>= (time-minutes t) 0)(<= (time-minutes t) 58)) (make-time (time-hours t)(+ (time-minutes t) 1))]
    [(and (>= (time-hours t) 1)(< (time-hours t) 12)(= (time-minutes t) 59)) (make-time (+ (time-hours t) 1) (- (time-minutes t) 59))]
    [(and (= (time-hours t) 12)(= (time-minutes t) 59)) (make-time (- (time-hours t) 11) (- (time-minutes t) 59))]
    [(and (= (time-hours t) 12)(>= (time-minutes t) 0)(<= (time-minutes t) 58)) (make-time (time-hours t)(+ (time-minutes t) 1))]
    [else "hours is 12 hours from 1 - 12 ; minutes is 60 minutes from 0 - 59"]))
    
;Exercise 2c
;design a function that consumes a Time and uses big-bang to draw an animated clock that runs starting from the given time.

(require 2htdp/image)
(require 2htdp/universe)
 
(define CLOCK-HOUR-HAND
  (above (rectangle 3 20 "solid" "black")
         (rectangle 3 20 "solid" "transparent")))
 
(define CLOCK-MINUTE-HAND
  (above (rectangle 2 30 "solid" "blue")
         (rectangle 2 30 "solid" "transparent")))
 
(define CLOCK-FACE (circle 40 "outline" "red"))
 
;draw-time: time -> Img
;take a time and draw the clock
(define (draw-time t)
     (overlay (rotate (* (time-hours t) -30) CLOCK-HOUR-HAND)
              (rotate (* (time-minutes t) -6) CLOCK-MINUTE-HAND)
              CLOCK-FACE))
 
;move-clock: clock -> clock
;a moving clock
(define (move-clock t)
  (clock t))

  
(big-bang time7
  [to-draw draw-time]
  [on-tick move-clock])


;Exercise 3a
;A hotel review website, such as Yelp, lists a lot of information about each hotel, including its name, luxury rating (in stars),
;and a summary of its price (in dollar signs). Design a new data definition called Hotel, which contains these three pieces of information about a hotel.
;Assume that hotels are rated with one to five stars, and their price is rated using one to four "$"s.


(define-struct hotel [name rating price])
;A hotel is make-hotel [String Img Img]
;Name - hotel name
;rating - luxury rating (in stars from 1 to 5)
;price - hotel price rating (in "$" from 1 to 4)
;Interpretation: Hotel lists the information about the hotel name, luxury rating & price rating.

(define hotel1 (make-hotel "The St. Regis New York" 4 4))
(define hotel2 (make-hotel "Cheaper The Metro Hotel" 4 2))
(define hotel3 (make-hotel "Taylor Hotel" 2 2))
(define hotel4 (make-hotel "M Hotel" 4 4))
(define hotel5 (make-hotel "V Hotel" 4 2))

(define (hotel-temp x)
  (...(hotel-name x)...
      (hotel-rating x)...
      (hotel-price x)...))

;Exercise 3b
;Finding a hotel that’s not too costly can be tricky. Design a function called in-budget? that takes a Hotel and a price rating,
;and produces #true if the Hotel is no more expensive than the price rating you’re willing to spend.

;in-budget?: hotel price -> boolean
;Takes a Hotel and a price rating and produces #true if the Hotel is no more expensive than the price rating you’re willing to spend

(check-expect (in-budget? hotel1 3) #false)
(check-expect (in-budget? hotel1 4) #true)
;(check-expect (in-budget? hotel1 0) "no such price")
;(check-expect (in-budget? hotel1 5) "no such price")
;(check-expect (in-budget? hotel4 5) "no such hotel")

(define (in-budget? h p)
  (if (<= (hotel-price h) p)
      #true
      #false))

;Exercise 3c
;The hotel business is tough these days, and less-expensive options are certainly more attractive than pricey ones...
;Design a function called make-cheaper that consumes a Hotel, called h, and produces a Hotel that has one fewer "$" than h,
;adds the string "Cheaper " to the beginning of the hotel name, and has the same star rating. However, if the price of h is exactly "$", the function should produce h itself.

;same-star-cheaper: hotel hotel -> hotel
;Find out which hotel is cheaper with same star rating. If same price chose first hotel.
(check-expect (same-star-cheaper hotel1 hotel2) hotel2)
(check-expect (same-star-cheaper hotel1 hotel4) hotel1)

(define (same-star-cheaper h1 h2)
  (if (and (=(hotel-rating h1)(hotel-rating h2)) (=(hotel-price h1)(hotel-price h2)))
      h1
      h2))


;make-cheaper: hotel -> hotel
;Consumes a Hotel, called h, and produces a Hotel that has one fewer "$" than h and has the same star rating, adds the string "Cheaper " to the beginning of the hotel name
;However, if the price of h is exactly "$" and has the same star rating, the function should produce h itself

(check-expect (make-cheaper hotel1) hotel2)
(check-expect (make-cheaper hotel5) hotel5)

(define (make-cheaper h)
  (same-star-cheaper h hotel2))

;Exercise 3d
;panel: hotel -> img
;Produce image of hotel name, star rating & price rating

(define (panel x)
   (above/align "left"
             (text (hotel-name x) 20 "black")
             (cond
               [(= (hotel-rating x) 1) (star 10 "solid" "red")]
               [(= (hotel-rating x) 2)(beside/align "bottom"
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red"))]
               [(= (hotel-rating x) 3)(beside/align "bottom"
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red"))]
               [(= (hotel-rating x) 4)(beside/align "bottom"
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red"))]
               [(= (hotel-rating x) 5)(beside/align "bottom"
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red")
                                             (star 10 "solid" "red"))])           
              (cond
                [(= (hotel-price x) 1) (text "$" 20 "black")]
                [(= (hotel-price x) 2)(beside/align "bottom"
                                             (text "$" 20 "black")
                                             (text "$" 20 "black"))]
                [(= (hotel-price x) 3)(beside/align "bottom"
                                             (text "$" 20 "black")
                                             (text "$" 20 "black")
                                             (text "$" 20 "black"))]
                [(= (hotel-price x) 4)(beside/align "bottom"
                                             (text "$" 20 "black")
                                             (text "$" 20 "black")
                                             (text "$" 20 "black")
                                             (text "$" 20 "black"))])))
               
   






