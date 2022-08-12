;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A RRT (RelayRaceTeam) is one of:
; - "Rebecca"
; - (make-runner String Number RRT)
(define-struct runner [name time team])
; and represents the fastest runner
; or a team containing her (and possibly other runners) with a name and fastest time in seconds.

;Exercise (Reviewed) 1
;Define examples and design a template for a RRT.

(define RRT0 "Rebecca")
(define RRT1 (make-runner "Bell" 1 RRT0))
(define RRT2 (make-runner "Ruby" 3 RRT1))
(define RRT3 (make-runner "Kim" 2 RRT2))
(define RRT4 (make-runner "Ruby" 3 (make-runner "Kim" 2 RRT1)))

(define (RRT-temp x)
  (...
   (cond
     [(string? x) ...]
     [(runner? x)...
      (runner-name x)...
      (runner-time x)...
      (RRT-temp(runner-team x))...])))

;Exercise (Reviewed) 2
;Design a function which gives a RRT’s total race time, assuming all the members run their fastest time (where "Rebecca" has a fastest time of 0).

;total-time: RRT -> Number
;Gives a RRT’s total race time, assuming all the members run their fastest time.

(check-expect (total-time RRT0) 0)
(check-expect (total-time RRT1) 1)
(check-expect (total-time RRT2) 4)
(check-expect (total-time RRT3) 6)

(define (total-time x)
  (cond
     [(string? x) 0]
     [else 
      (+ (total-time (runner-team x)) (runner-time x))]))

;Exercise 3
;Design a function which determines if a runner with a particular name is in a RRT.

;on-team? : String RRT -> Boolean
;Determines if a runner with a particular name is in a RRT

(check-expect (on-team? "Rebecca" RRT3) #true)
(check-expect (on-team? "Kim" RRT2) #false)
(check-expect (on-team? "Ming" RRT3) #false)

(define (on-team? n r)
  (cond
     [(string=? n "Rebecca") #true]
     [(runner? r)(or (string=? n (runner-name r))(on-team? n (runner-team r)))]
     [else #false]))


;Exercise 4
;Design a function which takes a RRT and decreases all of its runner’s times sizes by 1 second.

;cut-one: RRT -> Number
;Takes a RRT and decreases all of its runner’s times sizes by 1 second

(check-expect (cut-one RRT0) "Cannot be less than zero")
(check-expect (cut-one RRT1) (- 1 1))
(check-expect (cut-one RRT2) (- 4 1))
(check-expect (cut-one RRT3) (- 6 1))

(define (cut-one x)
  (if (= (total-time x) 0)
      "Cannot be less than zero"
      (- (total-time x) 1)))

;Exercise 5
;Design a function which takes a RRT and a number and removes all runners slower than the given time.

;removes: RRP Number -> RRP
;takes a RRT and a number and removes all runners slower than the given time.

(check-expect (removes RRT3 3) RRT3)
(check-expect (removes RRT3 2) (make-runner
 "Kim"
 2
 (make-runner
  " "
  " "
  (make-runner "Bell" 1 "Rebecca"))))
(check-expect (removes RRT3 1) (make-runner
 " "
 " "
 (make-runner
  " "
  " "
  (make-runner "Bell" 1 "Rebecca"))))

(define (removes t n)
  (cond
     [(string? t) "Rebecca"]
     [(<= (runner-time t) n)
      (make-runner (runner-name t)(runner-time t)(removes (runner-team t) n))]
     [else (make-runner " " " " (removes (runner-team t) n))]))

;Exercise 6 (INCOMPLETE)
;Design the function in-order? which ensures a RRT has its runners in order of slowest, second-slowest, third-slowest, and so on, ending with "Rebecca". 

;in-order?: RRT -> Boolean
;Ensures a RRT has its runners in order of slowest, second-slowest, third-slowest, and so on, ending with "Rebecca"

;error: unable to determine the way to evaluate boolean when the recursion fold back.
;(cond((> 3 (cond ((> 2 (cond ((> 1 0) #true) (else #false))) #true) (else #false))) #true)(else #false))#true)
;(cond ((> 3 (cond ((> 2 #true) #true) (else #false))) #true) (else #false)) #true)

;(check-expect (in-order? RRT3) #false)
;(check-expect (in-order? RRT4) #true)

;(define (in-order? t)
;  (cond
;     [(string? t) 0]
;     [(> (runner-time t)(in-order?(runner-team t)))#true]
;     [else #false]))

;Exercise 8
;Design the template for Nats.

; A Nat is one of:
; - 0
; - (add1 Nat)
 
(define NAT-0 0)
(define NAT-1 (add1 NAT-0))
(define NAT-2 (add1 NAT-1))
(define NAT-3 (add1 NAT-2))
(define NAT-4 (add1 NAT-3))


(define (nat-temp x)
  (...
  (cond
   [(zero? x)...]
   [(positive? x)...(nat-temp(sub1 x))])))

;Exercise 9
;Design the function double which doubles a Nat. As we are following our Nat template,
;you may only use the functions mentioned above, cond, and double (no + or *).

;double: Nat -> Nat
;Doubles a Nat without using + or *

(check-expect (double NAT-0) 0)
(check-expect (double NAT-1) (add1 NAT-1))
(check-expect (double NAT-2) (add1 (add1 NAT-2)))
(check-expect (double NAT-3) (add1 (add1 (add1 NAT-3))))

(define (double x)
  (cond
   [(zero? x) 0]
   [(positive? x) (add1 (add1 (double (sub1 x))))]))

;Exercise 10 (INCOMPLETE)
;Design the function even?/nat which deterimes if a Nat is even.
;You may only use the functions mentioned above, as well as cond, not, and even?/nat.
;Your code must follow the template exactly, and therefore two successive calls to sub1 is forbidden.

;even?/nat: Nat -> Boolean
;Determines if a Nat is even

;(check-expect (even?/nat NAT-0) #true)
;(check-expect (even?/nat NAT-1) #false)
;(check-expect (even?/nat NAT-2) #true)
;(check-expect (even?/nat NAT-3) #false)

;(define (even?/nat x)
;  (cond
;   [(zero? x) 0]
;   [(positive? x)(sub1(sub1(even?/nat(sub1 x))))]))

;Unable to find way to deduct the number by 2 until only 0 or l left where O is even and 1 is odd.

;Exercise 11
;Design the function nat+ which sums two Nats. You may only use the functions mentioned above, as well as cond and nat+ (no +).

;nat+: Nat Nat -> Nat

(check-expect (nat+ NAT-0 NAT-0)NAT-0)
(check-expect (nat+ NAT-1 NAT-1)NAT-2)
(check-expect (nat+ NAT-2 NAT-2)NAT-4)
(check-expect (nat+ NAT-0 NAT-1)NAT-1)
(check-expect (nat+ NAT-1 NAT-2)NAT-3)
(check-expect (nat+ NAT-1 NAT-3)NAT-4)

(define (nat+ x y)
  (cond
   [(and (zero? x)(zero? y)) 0]
   [(and (positive? x)(positive? y)) (add1(add1 (nat+ (sub1 x)(sub1 y))))]
   [(or (zero? x)(zero? y)) (abs (- x y))]
   [(or (positive? x)(positive? y)) (add1(add1 (nat+ (sub1 x)(sub1 y))))]))









       
















