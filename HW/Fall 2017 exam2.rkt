;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Fall 2017 exam2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Problem 1 [INCOMPLETE: Don't know how to use predicate as argument in functions]
Design the function take-while, that takes a list and a predicate, and returns all the elements from the front of the list for which the predicate returns true: that
is, all the elements up until the first element that does not pass the predicate. Do not
use any list abstractions to implement this function, including length. And, be sure to
give the best signature you can for this function.
|#

;take-while: [List-Of X] Boolean -> [List-Of X]
(check-expect (take-while (list 2 4 5)5)(list 2 4))
(check-expect (take-while (list 2 4 5)0)(list 2 4 5))
(check-expect (take-while (list 2 4 5)2)empty)

(define (take-while x y)
  (cond
    [(empty? x)x]
    [(cons? x)(if (not (equal? (first x) y))
                  (cons (first x)(take-while (rest x) y))
                  empty)]))
#|
Problem 2
Design a function counts-of-multiples, that takes a list of Naturals and a
Natural, and produces a Count of how many numbers in the given list are a multiple
of the given number and how many numbers in the list are not. For instance, if the
given number is 2, then counts-of-multiples returns the number of even numbers
and number of odd numbers in the list. Reminder: (modulo m n) returns the remainder when m is divided by n.

Use local and list abstractions (figures 95 and 96 from the book, which are reproduced
at the end of the exam) to design this function
|#

; A Count is a (make-count Natural Natural)
; INTERPRETATION: the number of exact multiples of a number
; and the number of non-multiples in some collection of numbers
(define-struct count [multiples leftovers])

(define (count-temp x)
  (...(count-multiples x)
      (count-leftovers x)...))

(check-expect (counts-of-multiples (list 2 3 4 7 8)0)(make-count 5 0))
(check-expect (counts-of-multiples (list 2 3 4 7 8)1)(make-count 5 0))
(check-expect (counts-of-multiples (list 2 3 4 7 8)2)(make-count 3 2))
(check-expect (counts-of-multiples (list 0 1 4 7 8)2)(make-count 3 2))

;counts-of-multiples: [List-Of Nat] Nat -> Count
;as above
(define (counts-of-multiples lon x)
  (local [;counts-of-multiples/v2: [List-Of Nat] Nat -> Count
          ;Filter out the list.
          (define (counts-of-multiples/v2 lon x)
            (if (or (= x 0) (= x 1))
                lon
                (filter
                 (λ(lon)(= (modulo lon x) 0))
                 lon)))

          (define MULTIPLES (counts-of-multiples/v2 lon x))]            
  (make-count (length MULTIPLES) (- (length lon) (length MULTIPLES)))))
   
;Problem 3
;Consider the following data definition:
; A Shrub is one of
; - Number
; - [List-of Shrub]
; INTERPRETATION: Describes a branching garden plant, either
; the size of a leaf (in inches), or a fork with an arbitrary
; number of branches coming off it
(define (shrub-temp x)
  (...
   (cond
     [(empty? x)...]
     [(cons? x)(...(first x)
                   (shrub-temp(rest x))...)])))
;Design the function max-branches, that computes the largest number of branches coming out of a fork in a Shrub.
;You may (but do not have to) use list abstractions for this problem.

(define SHRUB-0 empty)
(define SHRUB-1 1)
(define SHRUB-2 2)
(define SHRUB-3 3)
(define SHRUB-4 4)
(define BRANCH-1 (list SHRUB-1 SHRUB-2))
(define BRANCH-2 (list SHRUB-1 SHRUB-2 SHRUB-3))
(define BRANCH-3 (list SHRUB-1 SHRUB-2 SHRUB-3 SHRUB-4))
(define SHRUB101 (list  BRANCH-1  BRANCH-2))
(define SHRUB102 (list  BRANCH-3))

;max-branches: SHRUB -> Number
;as above
(check-expect (max-branches SHRUB-0)0)
(check-expect (max-branches SHRUB101)6)
(check-expect (max-branches SHRUB102)10)

(define (max-branches x)
  (local [;add: SHRUB -> Number
          ;add the list
          (define (add x)
            (cond
              [(empty? x)0]
              [(cons? x)(+ (first x)
                            (add (rest x)))]))
          (define LON (map add x))]
    (foldr max 0 LON)))
  
;Problem 4
;Consider the following definition:
; A StringExpr is one of
; - String
; - (list StringExpr '+ StringExpr)
(define (stringexpr-temp x)
  (...
   (cond
     [(empty? x)...]
     [(cons? x)(...(first x)
                   (stringexpr-temp(rest x))...)])))

;Design the function combine, that takes a StringExpr and concatenates all the Strings
;inside it.

(define SE-1 "The election")
(define SE-2 "is coming soon.")
(define SE-3 "Who will win?")
(define SE-4 "Let's wait and see.")
(define STORY (list (list SE-1 '+ SE-2)(list SE-3 '+ SE-4)))

;combine: StringExpr -> String
;as above
(check-expect (combine STORY)"The election is coming soon. Who will win? Let's wait and see. ")

(define (combine x)
  (local [;take-string:StringExpr -> String
          ;make the list-of StringExpr into list of string          
          (define (add x)
            (cond
              [(empty? x)(string-append " ")]
              [(cons? x)(string-append (if (string? (first x))
                                           (first x)
                                           " ")
                                      (add (rest x)))]))
                           
          (define LOS (map add x))]
    (foldr string-append "" LOS)))


