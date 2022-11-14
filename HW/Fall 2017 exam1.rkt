;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Fall 2017 exam1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Problem 1
Suppose you ran the program below in DrRacket. What would the output
be? For partial credit (in case you produce the wrong output), explain your work.
|#
(define-struct answer [to the question])

(define EX1
(make-answer "Lois" 42 (string=? "CS2500"
(string-append "2500" "CS"))))

(define EX2
(make-answer "Clark" 654 #false))

(define (check-it ans)
(cond
[(> (answer-the ans) 50) (answer-to ans)]
[(not (answer-question ans)) "Greetings"]
[else (string-append "Hello, "
(answer-to ans))]))

(check-it EX1)

#|
(define EX1
(make-answer "Lois" 42 (string=? "CS2500"
(string-append "2500" "CS"))))

(define EX1
(make-answer "Lois" 42 #false))

(define (check-it EX1)
(cond
[(> (answer-the EX1) 50) (answer-to EX1)]
[(not (answer-question EX1)) "Greetings"]
[else (string-append "Hello, "
(answer-to EX1))]))

(define (check-it EX1)
(cond
[(> 42 50) "Lois"]
[(not #false) "Greetings"]
[else (string-append "Hello, "
"Lois")]))

(define (check-it EX1)
(cond
[#false "Lois"]
[#true "Greetings"]
[else (string-append "Hello, "
"Lois")]))

answer: "Geetings"
|#


;Problem 2
;Take a look at this data definition:
(define-struct sling [shot])
(define-struct high [land tops])

; A Singer is one of:
; - (make-sling Boolean)
; - (make-high Number String)
; - #false
; Intepretation not needed for this problem

;Provide three completely distinct data examples for Singer.
(define GUN (make-sling #false))
(define POWER-S (make-high 7 "A powerful sling shot model"))
(define WEAPON #false)

;Problem 3
;Here are definitions:
(define-struct image-tweet [message picture])
(define-struct retweet [other-tweet])

; A Tweet is one of:
; - String
; - (make-image-tweet String Image)
; - (make-retweet String)
; Interpretation: Represents a tweet, which is either a message (String),
; a message along with a picture (make-image-tweet), or a retweet of
; another message (make-retweet)

;Develop the template(s) for any data definition(s) you see here

(define (tweet-temp x)
  (...
  (cond
    [(string=? x s)s]
    [(image-tweet? x)(...(image-tweet-message x)
                          (image-tweet-picture x)...)]
    [(retweet? x)(...(tweet-temp(retweet-other-tweet x))...)])))


;Develop test cases for tweet->text, a function that takes a Tweet and returns whatever
;text is inside it.

;tweet->text: Tweet -> String
;as above
;(check-expect (tweet->text "2017 exam")"2017 exam")
;(check-expect (tweet->text (make-image-tweet "2021 firesales" img))"2021 firesales")
;(check-expect (tweet->text (make-retweet "2021 firesales"))"2021 firesales")

;Problem 4
;Find two bugs in this function. For each one, explain what the problem is, and give a
;test case that does not pass (i.e., either fails or crashes) to demonstrate the problem.

;Take a look at these data defintitions:
; A PayRecord is a (make-record String Paycheck)
(define-struct record1 [name salary])
; Interpretation: a record of a payment to an employee

; A Paycheck is one of
; - PositiveNumber
; - (make-bonus PositiveNumber NonNegativeNumber)
(define-struct bonus [base-pay extra])
; Interpretation: A paycheck is either just some number of dollars, or
; includes both an employee's normal pay and some extra (both in dollars)

;Consider the following very badly-designed function:
; Returns the name of the employee with the greater paycheck
; PayRecord PayRecord -> String
(define (max-earner emp1 emp2)
(if (< (record1-salary emp1) (record1-salary emp2))
(record1-name emp1)
(record1-name emp2)))

(define PAY-001 (make-bonus 3000 0))
(define PAY-002 (make-bonus 1800 800))
(define PR001 (make-record1 "Ali" PAY-001))
(define PR002 (make-record1 "Babu" PAY-002))

;(check-expect (max-earner PR001 PR002)"Ali")
#|
Bug1: The function is comparing the structure bonus and not the constructor-base-pay and extra.


Bug2: The sign less than is wrong since the function should select the great paycheck. So the sign should be more than.
|#

;Problem 5
;Here is a data definition:
; A DogSled is one of:
; - "sled"
; - (make-team DogSled Number)
; Interpretation: A sled pulled by a team of dogs. The numbers
; represent each dog's maximum speed.
(define-struct team [sled speed])

#|
Design the function sled-speed.
This function takes in a DogSled and returns a Number
representing the fastest the sled and team can go. Assume that the sled itself can go
no faster than 100 miles per hour, or it would break apart, and assume that no dog can
run faster than its maximum speed. Show all steps of the design recipe, including any
appropriate templates.
Note: you may use any BSL functions dealing with numbers (number?, min, +, sqrt,
abs, etc) that you may find convenient.
|#

(define (dogsled-temp x)
  (...
   (cond
     [(string=? x "sled")...]
     [(team? x)(...(team-sled x)
                   (team-speed x)...)])))

(define TIGER (make-team "fire" 30))
(define LION (make-team "fire" 20))
(define CAT (make-team "fire" 10))
(define A (make-team "wind" 50))
(define B (make-team "wind" 20))
(define C (make-team "wind" 40))
(define dogsled1 (list TIGER LION CAT))
(define dogsled2 (list A B C))


;sled-speed: DogSled -> Number
;as above
(check-expect (sled-speed dogsled1)60)
(check-expect (sled-speed dogsled2)100)

(define (sled-speed x)
   (cond
     [(empty? x)0]
     [(cons? x)(if (> (foldr + 0 (map (λ(n)(team-speed n))x))
                      100)
                      100
                      (foldr + 0 (map (λ(n)(team-speed n))x)))]))




