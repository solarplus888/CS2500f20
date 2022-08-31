;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise (Reviewed) 1

;Consider the following two data definitions:
; A ListOfString (LOS) is one of:
; - empty
; - (cons String LOS)
;Interpretation: A list of strings.
 
; A ListOfNumber (LON) is one of:
; - empty
; - (cons Number LON)
;Interpretation: A list of numbers.

;Design a data definition which abstracts these two definitions. Redefine ListOfString and ListOfNumber using your abstraction.

;A List Of X (LOX) is one of:
; - empty
; - (cons X LOX)
;Interpretation: A list of elements of type X

;Exercise 2

;Consider the following two data definitions:
; A MaybeString is one of:
; - #false
; - String
 
; A MaybePosn is one of:
; - #false
; - Posn

;Design a data definition which abstracts these two definitions. Redefine MaybeString and MaybePosn using your abstraction.

; A [Maybe X] is one of:
; - #false
; - X

;Exercise (Reviewed) 3
;Consider the following two function definitions:

; matching-x-posn : [List-of Posn] Number Posn -> Posn
; Find the first Posn in the list with the given x-coordinate or return the given Posn
; if no such position can be found
(check-expect (matching-x-posn '() 10 (make-posn 0 0)) (make-posn 0 0))

(check-expect
 (matching-x-posn
  (cons (make-posn 1 2) (cons (make-posn 3 4) '())) 3 (make-posn 5 6))
 (make-posn 3 4))

(define (matching-x-posn lop desired-x default)
  (find-first-match lop desired-x default posn-x))

#|
(cond [(empty? lop) default]
        [(cons? lop)
         (if (= (posn-x (first lop)) desired-x)
             (first lop)
             (matching-x-posn (rest lop) desired-x default))]))
|#

; string-with-length : [List-of String] Nat -> String
; Returns the first String in the given list with the given length or "no such string" if no
; such string can be found
(check-expect (string-with-length '() 10) "no such string")

(check-expect (string-with-length (cons "hi" (cons "hello" (cons "aloha" '()))) 5) "hello")

(define (string-with-length los desired-length)
  (find-first-match los desired-length "no such string" string-length))
#|
(cond [(empty? los) "no such string"]
        [(cons? los)
         (if (= (string-length (first los)) desired-length)
             (first los)
             (string-with-length (rest los) desired-length))]))
|#
;Design the function find-first-match which abstracts these two functions.
;Be sure to redefine matching-x-posn and string-with-length using your abstraction.

;find-first-match: [List-Of-X] X X f -> X
;Find first match of a list

(check-expect (find-first-match (cons 4 (cons 3 (cons 2 '()))) 3 "nil" multiply1)3)
(check-expect (find-first-match (cons 4 (cons 3 (cons 2 '()))) 8 "nil" multiply1)"nil")

(define (find-first-match loX desired default f)
  (cond [(empty? loX) default]
        [(cons? loX)
         (if (= (f (first loX)) desired)
             (first loX)
             (find-first-match (rest loX) desired default f))]))

(define (multiply1 x)
  (* 1 x))

;Exercise 4
;Create a template for the data definition given above. If you are struggling with this please see the design recipe page on the course website.

; A Nat is one of:
; - 0
; - (add1 Nat)

(define NAT-0 0)
(define NAT-1 (add1 NAT-0))
(define NAT-2 (add1 NAT-1))
(define NAT-3 (add1 NAT-2))

(define (NAT-temp x)
  (...
   (cond
     [(zero?)...]
     [(positive?) add1 x])))

;Exercise 5
;Design the function even-nat? which takes a Nat and returns #true if the Nat is an even number. Yes, zero is even.
;You may not use even? or odd? to write this function.

;even-nat?: Number -> Boolean
(check-expect (even-nat? 0)#true)
(check-expect (even-nat? 1)#false)
(check-expect (even-nat? 2)#true)

(define (even-nat? x)
  (cond
     [(zero? x)#true]
     [(= (modulo (add1 x) 2) 1) #true]
     [else #false]))

;Exercise 6
;Design the function nat+ which takes two Nats and returns their sum. You may not use + to write this function.

;nat+: Nat Nat -> Nat
;Takes two Nats and returns their sum
(check-expect (nat+ NAT-0 NAT-0)0)
(check-expect (nat+ NAT-0 NAT-1)1)
(check-expect (nat+ NAT-1 NAT-3)4)
(check-expect (nat+ NAT-2 NAT-3)5)

(define (nat+ x y)
  (cond
     [(and (zero? x)(zero? y))0]
     [(and (zero? x)(positive? y))y]
     [(and (positive? x)(zero? y))x]
     [(and (positive? x)(positive? y))
      (add1(add1(nat+ (sub1 x) (sub1 y))))]))

;Starter Code: Consider the following data definitions and functions:

; A Pair is a (make-pair String PosInt)
; and represents an element with a non-zero count
(define-struct pair [element value])
(define (pair-temp p)
  (... (pair-element p) ... (pair-value p) ...))
 
; A Counter is one of:
; - '()
; - (cons Pair Counter)
; and represents a multiset (a set of elements where
; an element can appear more than once)
(define (counter-temp counter)
  (cond [(empty? counter) ...]
        [(cons? counter) (... (pair-temp (first counter))
                              ... (counter-temp (rest counter))
                              ...)]))
 
(define marble-bag (list (make-pair "green" 2) (make-pair "red" 5)))
; marble-bag represents a bag with 2 "green" marbles and 5 "red" ones

; get : Counter String -> PosInt
; Get the count of the given element
(check-error (get (list (make-pair "cats" 3)) "dogs") "not found")
(check-expect (get (list (make-pair "cats" 3) (make-pair "dogs" 4)) "dogs") 4)

(define (get counter element)
  (cond [(empty? counter) (error "not found")]
        [else (if (counts-element? (first counter) element)
                  (pair-value (first counter))
                  (get (rest counter) element))]))


; counts-element? : Pair String -> Boolean
; Does the pair hold a count for the element?
(check-expect (counts-element? (make-pair "green" 2) "green") #true)
(check-expect (counts-element? (make-pair "red" 5) "blue") #false)

(define (counts-element? pair element)
  (string=? element (pair-element pair)))


;Sample Problem Design the function add-to-counter, which given a Counter and an element will add 1 to the previously associated count.
;If that element was not present, its count will be 1.

; add-to-counter : Counter String -> Counter
; Add one to the count associated with element or set it to 1
; if it hasn't been seen before
(check-expect (add-to-counter '() "blue") (list (make-pair "blue" 1)))
(check-expect (add-to-counter marble-bag "red")
              (list (make-pair "green" 2) (make-pair "red" 6)))
(check-expect (add-to-counter marble-bag "green")
              (list (make-pair "green" 3) (make-pair "red" 5)))

(define (add-to-counter counter element)
  (cond
    [(empty? counter) (list (make-pair element 1))]
    [(cons? counter) (if (counts-element? (first counter) element)
                         (cons (increment-value (first counter))
                               (rest counter))
                         (cons (first counter)
                               (add-to-counter (rest counter) element)))]))

; increment-value : Pair -> Pair
; Increment the value in pair
(check-expect (increment-value (make-pair "green" 2)) (make-pair "green" 3))
(check-expect (increment-value (make-pair "red" 5)) (make-pair "red" 6))

(define (increment-value pair)
  (make-pair (pair-element pair) (add1 (pair-value pair))))

;Exercise 7 Design the function total-size, which grabs the total count of elements in a Counter (there are 7 in marble-bag).

;total-size: Counter -> PosInt
;Grabs the total count of elements in a Counter
(check-expect (total-size marble-bag)7)

(define (total-size counter)
  (cond [(empty? counter) 0]
        [(cons? counter) (+ (pair-value (first counter))
                         (total-size (rest counter)))]))

;Exercise 8
;Design the function initiate-counter, which given a String creates a Counter with one copy of that element. Be sure to follow the data definition.

;initiate-counter: String -> Counter
;Given a String creates a Counter with one copy of that element
(check-expect (initiate-counter "green")(list(make-pair "green" 1)))

(define (initiate-counter counter)
  (list (make-pair counter 1)))
                        
;Exercise 9
;Design the function all-elements, which outputs a ListOfString containing all of the elements a Counter has counted so far.
;For example, the output of (all-elements marble-bag) would be (list "green" "green" "red" "red" "red" "red" "red").
;Hint: Notice that every PosInt is also a Nat; this may be helpful.

;all-elements: Counter -> ListOfString
;Outputs a ListOfString containing all of the elements a Counter has counted so far
(check-expect (all-elements marble-bag) (list "green" "green" "red" "red" "red" "red" "red"))

(define (all-elements counter)
  (cond [(empty? counter) '()]
        [(cons? counter) (append(make-list (pair-value (first counter)) (pair-element (first counter)))   
                                (all-elements (rest counter)))]))

;Exercise 10
;Design the function highest-count, which returns the highest count for a single element in a Counter. If the counter is empty, return 0.

;highest-count: Counter -> Number
;Returns the highest count for a single element in a Counter. If the counter is empty, return 0.
(check-expect (highest-count '())0)
(check-expect (highest-count marble-bag)5)
(check-expect (highest-count (list (make-pair "green" 2) (make-pair "red" 6)))6)

(define (highest-count counter)
  (cond [(empty? counter) 0]
        [(cons? counter) (max (pair-value (first counter))(highest-count (rest counter)))]))





