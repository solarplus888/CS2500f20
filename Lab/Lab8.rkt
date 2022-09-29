;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab8) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require "mimic.rkt")

; A [Counter X] is a [Mapping X PosInt]
; and represents a multiset (a set of elements where an element can appear more than once)
 
(define MARBLE-BAG (list (make-pair "green" 2) (make-pair "red" 5)))
; MARBLE-BAG represents a bag with 2 "green" marbles and 5 "red" ones

;Exercise (Reviewed) 1
;Design the function add-to-counter, which given a [Counter X] and an X will add 1 to the previously associated count.
;If that element was not present, its count will be 1. Hint: do any of the functions provided by "mimic.rkt" seem helpful?

;add-to-counter: [Mapping X PosInt] X -> [Mapping X PosInt]
;Refer above

(check-expect (add-to-counter MARBLE-BAG "green")(list (make-pair "green" (+ 2 1)) (make-pair "red" 5)))
(check-expect (add-to-counter MARBLE-BAG "yellow")(list (make-pair "green" 2) (make-pair "red" 5)(make-pair "yellow" 1)))

(define (add-to-counter lom element)
  (update-mapping lom element add1 1))

;Exercise (Reviewed) 2
;Design the function total-size, which grabs the total count of elements in a counter (there are 7 in MARBLE-BAG).
;Use foldr.

#|
(check-expect (loe MARBLE-BAG)(list 2 5))

(define (element lom)
            (pair-y lom))

(define (loe lom)
            (map element lom))

|#

;total-size: [X Y][X Y -> Y] Y [List-Of X] -> Y
;As above

(check-expect (total-size MARBLE-BAG)7)

(define (total-size lom)
  (local [;element: lom -> Number
          ;Get the element of the list
          (define (element lom)
            (pair-y lom))

          ;[X Y][X -> Y][List-Of X] -> [List-Of Y]    
          (define (loe lom)
            (map element lom))]
  (foldr + 0 (loe lom))))

;Exercise 3
;Design the function initiate-counter, which given an X creates a counter with one copy of that element.
;Be sure to follow the data definition.

;initiate-counter: LoM String -> LoM
;As above

(check-expect (initiate-counter '() "blue")(make-pair "blue" 1))
(check-expect (initiate-counter MARBLE-BAG "blue")(list (make-pair "blue" 1)(make-pair "green" 2) (make-pair "red" 5)))

(define (initiate-counter m x)
  (cond [(empty? m) (make-pair x 1)]
        [else (cons (make-pair x 1) m)]))

;Exercise 4
;Define expected-counts, which given a counter and a natural number n, outputs a list of numbers representing how many times we would expect to see each element of the counter
;if we randomly selected from the counter n times (with replacement for you probability whizzes). The provided tests should clarify this exercise.
;Use map. You should also locally define some constant that will make sure you only compute the total size of the counter once.

; expected-counts : {X, Y} [Counter X] Nat -> [List-of Number]
; Expected counts of elements when grabbing from the counter n times
(check-expect (expected-counts '() 100) '())
(check-expect (expected-counts MARBLE-BAG 1000)
              (list (* 2/7 1000) (* 5/7 1000)))

(define (expected-counts lom x)
  (local[(define SIZE (total-size lom))
          
          ;f:[Counter X] -> Nat
          ;Produce the probability of X
          (define (f lom)            
            (* (/ (pair-y lom) SIZE) x))]              
  (map f lom)))

;Exercise 5
;Define count, that will take a [List-of X] and an X and determine how often it appears in the list.
;Use filter and length.
;The signature, purpose, and tests, have been provided for you below.
;Note you will have to use equal? here, as we don’t know what X is ahead of time.

; count : {X} [List-of X] X -> Nat
; How many times does x appear in the list?
(check-expect (count '() "b") 0)
(check-expect (count (list "a" "b" "a") "a") 2)

(define (count lox x)
  (local [;f: [List-Of X] -> Boolean
          ;Check whether x is in the list
          (define (f lox)
            (if (equal? lox x)
                        #true
                        #false))]
  ;[X][X -> Boolean] [List-Of X] ->  [List-Of X]
  (length(filter f lox))))

;Exercise 6 
;Define count-grabs, which given a [Counter X] and a [List-of X], sees how many times the elements from that counter appear in the list.
;Use map. The signature, purpose, and tests have been provided for you below.


;unique: [Counter x] -> [List-Of-X]
;Get the unique element in the Counter.

(check-expect (unique MARBLE-BAG)(list "green" "red"))
(check-expect (unique (list (make-pair "green" 1)(make-pair "green" 2) (make-pair "red" 5)))(list "green" "green" "red"))

(define (unique counter)
            (cond
              [(empty? counter) '()]
              [else (cons (pair-x(first counter))(unique(rest counter)))]))

; count-grabs : {X} [Counter X] [List-of X] -> [List-of Number]
; See how many times the elements from this counter are in this list
(check-expect (count-grabs '() '()) '())
(check-expect (count-grabs MARBLE-BAG (list "green" "red" "red" "red")) (list 1 3))


(define (count-grabs counter lox)
  (local           
          [(define (count/v2 counter)
            (count lox (pair-x counter)))]
                
  ;[X Y][X->Y][List-Of-X] -> [List-Of-Y]
  (map count/v2 counter)))

;Exercise 7
; grab-random : {X} [Counter X] -> X
; Randomly grab from this counter
;(grab-random MARBLE-BAG) should have a 2/7 chance of returning "green" and a 5/7 chance of returning "red"
(define (grab-random c)
  (local (; grab : Nat [Counter X] -> X
          ; Grab the first element in c if its count is larger than n,
          ; otherwise reduce n by the count and recur
          (define (grab n c)
            (cond [(< n (pair-y (first c))) (pair-x (first c))]
                  [else (grab (- n (pair-y (first c))) (rest c))])))
    (grab (random (total-size c)) c)))

;Define grab-n which given a counter and a natural number n builds up a list in which we randomly grab from the counter n many times.
;Use build-list.
;The signature and purpose statement for the function have been provided below. Remember by convention, if a function ignores its input, we name that argument _.

; grab-n : {X} [Counter X] Nat -> [List-of X]
; Grab from the counter n times

(check-within (count-grabs MARBLE-BAG (grab-n MARBLE-BAG 10000))
              (expected-counts MARBLE-BAG 10000)
              100)

(define (grab-n c n)
  (local[(define (f i)
          (if (<= i n)
              (grab-random c)
              empty))]
   (build-list n f)))

;Exercise 8

; A WritingStyle is a [Mapping String [Counter String]]
; and represents:
; - how often some words follow another,
; - along with what words start (The empty string is associated with words that start a sentence)
; - and end a sentence. ( how many times a word ends a sentence can be determined by the count of "." in its associated Counter.)
; A Sentence is a [List-of String]

;Design the function add-to-ws, which given a WritingStyle ws and two Strings, first-word and second-word,
;updates ws to indicate that first-word was followed by second-word once more than indicated in ws.
;Look at the data definition for WritingStyle and use the previously defined and provided functions!

;add-to-ws: WS String String -> WS
;(list (list "how" "are" "you") (list "how" "am" "i") (list "i" "am" "great"))

(define STYLE-EXAMPLE
  (list
    (list "great" (list (list "." 1)))
    (list "am" (list (list "great" 1) (list "i" 1)))
    (list "i" (list (list "am" 1) (list "." 1)))
    (list "" (list (list "i" 1) (list "how" 2)))
    (list "how" (list (list "am" 1) (list "are" 1)))
    (list "you" (list (list "." 1)))
    (list "are" (list (list "you" 1)))))

(check-expect (add-to-ws STYLE-EXAMPLE "how" "am")
              (list (list "great" (list (list "." 1)))
                    (list "am" (list (list "great" 1) (list "i" 1)))
                    (list "i" (list (list "am" 1) (list "." 1)))
                    (list "" (list (list "i" 1) (list "how" 2)))
                    (list "how" (list (list "am" 2) (list "are" 1)))
                    (list "you" (list (list "." 1)))
                    (list "are" (list (list "you" 1)))))

(define (add-to-ws m x y)
 (local [;map chose the first in the list:(list "great" (list (list "." 1))) Use f to compare the first: "great".
         (define (f m)
           (if (string=? (first m) x)
               (cons (first m)
                     (local[;map chose the first in the rest of list:(list (list "." 1))) Use g to compare the first: ".".
                            (define (g m)
                              (if (string=? (first(first m)) y)
                                  (update-mapping m y add1 0)
                                  m))]                                  
                       (map g (rest m))))
               m))]
   (map f m)))

;Exercise 9
;Design the function update-ws, which given a Sentence and a WritingStyle,
;updates the writing style to indicate that the consecutive pairs of words in the list follow each other.
;Note that if the list is less than two words long, nothing should change.
;Don’t worry if your function does not produce the same ordering as in STYLE-EXAMPLE; there are several ways to approach this problem.

;add-to-ws/v2: WS String String -> WS
;Put in a new list for second if not on the list or update the list counter.
(check-expect (add-to-ws/v2 STYLE-EXAMPLE "great" "hot")
              (list (list "great" (list (list "." 1)(list "hot" 1)))
                    (list "am" (list (list "great" 1) (list "i" 1)))
                    (list "i" (list (list "am" 1) (list "." 1)))
                    (list "" (list (list "i" 1) (list "how" 2)))
                    (list "how" (list (list "am" 1) (list "are" 1)))
                    (list "you" (list (list "." 1)))
                    (list "are" (list (list "you" 1)))))

(check-expect (add-to-ws/v2 STYLE-EXAMPLE "great" ".")
              (list (list "great" (list (list "." 2)))
                    (list "am" (list (list "great" 1) (list "i" 1)))
                    (list "i" (list (list "am" 1) (list "." 1)))
                    (list "" (list (list "i" 1) (list "how" 2)))
                    (list "how" (list (list "am" 1) (list "are" 1)))
                    (list "you" (list (list "." 1)))
                    (list "are" (list (list "you" 1)))))

(define (add-to-ws/v2 m x y)
 (local [;map chose the first in the list:(list "great" (list (list "." 1))) Use f to compare the first: "great".
         (define (f m)
           (if (string=? (first m) x)
               (cons (first m)
                     (local[;map chose the first in the rest of list:(list (list "." 1))) Use g to compare the first: ".".
                            (define (g m)
                              (if (string=? (first(first m)) y)
                                  (update-mapping m y add1 0)
                                  (update-mapping m y add1 1)))]                                  
                       (map g (rest m))))
               m))]
   (map f m)))

;add-to-ws/v3: WS String String -> WS
;Update "" if not on the list or update the list counter.
(check-expect (add-to-ws/v3 STYLE-EXAMPLE "Accounts" "hot")
              (list (list "great" (list (list "." 1)))
                    (list "am" (list (list "great" 1) (list "i" 1)))
                    (list "i" (list (list "am" 1) (list "." 1)))
                    (list "" (list (list "i" 1) (list "how" 2)(list "Accounts" 1)))
                    (list "how" (list (list "am" 1) (list "are" 1)))
                    (list "you" (list (list "." 1)))
                    (list "are" (list (list "you" 1)))))

(define (add-to-ws/v3 m x y)
 (local [;map chose the first in the list:(list "great" (list (list "." 1))) Use f to compare the first: "great".
         (define (f m)
           (if (string=? (first m) "") ;first new
               (cons (first m)
                     (local[;map chose the first in the rest of list:(list (list "." 1))) Use g to compare the first: ".".
                            (define (g m)
                              (if (string=? (first(first m)) x)
                                  (update-mapping m x add1 0)       ;first existing
                                  (update-mapping m x add1 1)))]    ;first new                              
                       (map g (rest m))))
                m))]
   (map f m)))

;update-ws: S WS -> WS
;As above
;(new, new)
(check-expect (update-ws (list "Accounts" "completed")
(list
  (list "great" (list (list "." 1)))
  (list "am" (list (list "great" 1) (list "i" 1)))
  (list "i" (list (list "am" 1) (list "." 1)))
  (list "" (list (list "i" 1) (list "how" 2)))
  (list "how" (list (list "am" 1) (list "are" 1)))
  (list "you" (list (list "." 1)))
  (list "are" (list (list "you" 1)))))
              
(list
 (list "great" (list (list "." 1)))
 (list
  "am"
  (list (list "great" 1) (list "i" 1)))
 (list
  "i"
  (list (list "am" 1) (list "." 1)))
 (list
  ""
  (list
   (list "i" 1)
   (list "how" 2)
   (list "Accounts" 1)))
 (list
  "how"
  (list (list "am" 1) (list "are" 1)))
 (list "you" (list (list "." 1)))
 (list "are" (list (list "you" 1)))
 (list "Accounts" (list "completed" 1))
 (list "completed" (list "." 1))))
                                                       
;(new, existing ".") (incomplete as "" not updated with "Accounts")
(check-expect (update-ws (list "Accounts" "you")
(list
  (list "great" (list (list "." 1)))
  (list "am" (list (list "great" 1) (list "i" 1)))
  (list "i" (list (list "am" 1) (list "." 1)))
  (list "" (list (list "i" 1) (list "how" 2)))
  (list "how" (list (list "am" 1) (list "are" 1)))
  (list "you" (list (list "." 1)))
  (list "are" (list (list "you" 1)))))
              
(list
 (list "great" (list (list "." 1)))
 (list
  "am"
  (list (list "great" 1) (list "i" 1)))
 (list
  "i"
  (list (list "am" 1) (list "." 1)))
 (list
  ""
  (list (list "i" 1) (list "how" 2))) ;(list "Accounts" 1) missing
 (list
  "how"
  (list (list "am" 1) (list "are" 1)))
 (list "you" (list (list "." 2)))
 (list "are" (list (list "you" 1)))
 (list "Accounts" (list "you" 1))))
                       
;(existing, new)
(check-expect (update-ws (list "great" "hot")
(list
  (list "great" (list (list "." 1)))
  (list "am" (list (list "great" 1) (list "i" 1)))
  (list "i" (list (list "am" 1) (list "." 1)))
  (list "" (list (list "i" 1) (list "how" 2)))
  (list "how" (list (list "am" 1) (list "are" 1)))
  (list "you" (list (list "." 1)))
  (list "are" (list (list "you" 1)))))

(list
 (list
  "great"
  (list (list "." 1) (list "hot" 1)))
 (list
  "am"
  (list (list "great" 1) (list "i" 1)))
 (list
  "i"
  (list (list "am" 1) (list "." 1)))
 (list
  ""
  (list (list "i" 1) (list "how" 2)))
 (list
  "how"
  (list (list "am" 1) (list "are" 1)))
 (list "you" (list (list "." 1)))
 (list "are" (list (list "you" 1)))
 (list "hot" (list "." 1))))
             
;(existing, existing) ? "you "." not updated"
(check-expect (update-ws (list "are" "you")
(list
  (list "great" (list (list "." 1)))
  (list "am" (list (list "great" 1) (list "i" 1)))
  (list "i" (list (list "am" 1) (list "." 1)))
  (list "" (list (list "i" 1) (list "how" 2)))
  (list "how" (list (list "am" 1) (list "are" 1)))
  (list "you" (list (list "." 1)))
  (list "are" (list (list "you" 1)))))

(list
 (list "great" (list (list "." 1)))
 (list
  "am"
  (list (list "great" 1) (list "i" 1)))
 (list
  "i"
  (list (list "am" 1) (list "." 1)))
 (list
  ""
  (list (list "i" 1) (list "how" 2)))
 (list
  "how"
  (list (list "am" 1) (list "are" 1)))
 (list "you" (list (list "." 1)))   ;(list "you" (list (list "." 2)))
 (list "are" (list (list "you" 2)))))

(check-expect (update-ws (list "are")
(list
  (list "great" (list (list "." 1)))
  (list "am" (list (list "great" 1) (list "i" 1)))
  (list "i" (list (list "am" 1) (list "." 1)))
  (list "" (list (list "i" 1) (list "how" 2)))
  (list "how" (list (list "am" 1) (list "are" 1)))
  (list "you" (list (list "." 1)))
  (list "are" (list (list "you" 1)))))

(list
  (list "great" (list (list "." 1)))
  (list "am" (list (list "great" 1) (list "i" 1)))
  (list "i" (list (list "am" 1) (list "." 1)))
  (list "" (list (list "i" 1) (list "how" 2)))
  (list "how" (list (list "am" 1) (list "are" 1)))
  (list "you" (list (list "." 1)))
  (list "are" (list (list "you" 1)))))              

(define (update-ws s ws)
        (cond
              [(and (>= (length s) 2) ;(new, new)
                    (not(member?(first s)(local[(define (g ws)(first ws))](map g ws))))
                    (not(member?(second s)(local[(define (g ws)(first ws))](map g ws)))))                    
               (append(add-to-ws/v3 ws (first s)(second s))(list(list(first s)(list (second s) 1))(list (second s) (list "." 1))))]

              [(and (>= (length s) 2) ;(new, existing ".") (incomplete as "" not updated with "Accounts")
                    (not(member?(first s)(local[(define (g ws)(first ws))](map g ws))))
                    (member?(second s)(local[(define (g ws)(first ws))](map g ws))))                    
               (append(add-to-ws ws (second s) ".")(list (list(first s)(list (second s) 1))))]
              
              [(and (>= (length s) 2) ;(existing, new)
                    (member?(first s)(local[(define (g ws)(first ws))](map g ws)))
                    (not(member?(second s)(local[(define (g ws)(first ws))](map g ws)))))               
               (append(add-to-ws/v2 ws (first s)(second s))(list (list(second s)(list "." 1))))]

              [(and (>= (length s) 2) ;(existing, existing) ? "you "." not updated"
                    (member?(first s)(local[(define (g ws)(first ws))](map g ws)))
                    (member?(second s)(local[(define (g ws)(first ws))](map g ws))))               
               (add-to-ws/v2 ws (first s)(second s))]
                                       
                    
               [else ws]))

;Exercise 10
;Design the function style, which given a [List-of Sentence] generates the writing style given by those sentences.
;Don’t forget to put "" at the beginning and "." at the end of each sentence. Use foldr: [X Y] [X Y -> Y] Y [List-of X] -> Y

;style: [List-of Sentence] -> WS
;As above

(define LOS (list (list "1" "2")(list "3" "4"))) 

(define SX1
  (list
    (list "great" (list (list "." 1)))
    (list "am" (list (list "great" 1) (list "i" 1)))
    (list "i" (list (list "am" 1) (list "." 1)))
    (list "" (list (list "i" 1) (list "how" 2)))
    (list "how" (list (list "am" 1) (list "are" 1)))
    (list "you" (list (list "." 1)))
    (list "are" (list (list "you" 1)))))

(check-expect (style LOS)
(list
 (list "great" (list (list "." 1)))
 (list
  "am"
  (list (list "great" 1) (list "i" 1)))
 (list
  "i"
  (list (list "am" 1) (list "." 1)))
 (list
  ""
  (list
   (list "i" 1)
   (list "how" 2)
   (list "3" 1)
   (list "1" 1)))
 (list
  "how"
  (list (list "am" 1) (list "are" 1)))
 (list "you" (list (list "." 1)))
 (list "are" (list (list "you" 1)))
 (list "3" (list "4" 1))
 (list "4" (list "." 1))
 (list "1" (list "2" 1))
 (list "2" (list "." 1))))

(define (style los)
  (local[(define (f los sw)
           (update-ws los sw))]
  (foldr f SX1 los)))

;Exercise 11 (INCOMPLETE. maybe need to use foldr)
;Design the function imitate, which given a WritingStyle, outputs a Sentence that could have been written in that style.
;To accomplish this, locally define a function next-word, which takes in a String current-word and outputs a Sentence.
;If current-word is equal to ".", next-word outputs the empty list.
;Otherwise, next-word will cons current-word onto the result of next-word called with a randomly selected String that follows
;current-word according to the writing style. Look at the data definition for WritingStyle and use the previously defined functions!
;Initially, call next-word with "" to indicate the start of a sentence. Take the rest of the result to discard the empty string.

;imitate: WS -> S
;As above
#|
(define STYLE-EXAMPLE2
  (list (list "great" (list (list "." 1)))
                    (list "am" (list (list "great" 1) (list "i" 1)))
                    (list "i" (list (list "am" 1) (list "." 1)))
                    (list "" (list (list "i" 1) (list "how" 2)(list "Accounts" 1)))
                    (list "how" (list (list "am" 1) (list "are" 1)))
                    (list "you" (list (list "." 1)))
                    (list "are" (list (list "you" 1)))))

(check-expect (imitate STYLE-EXAMPLE2)0)

(define (imitate ws)
  (local [;next-word: String -> S
          ;Takes in a String and outputs a Sentence
          (define (next-word m)
           ;(if (string=? (first m) "") 
               (cons (first m)                   
                     (if (string=? (grab-random (second m)) ".")
                         (list empty)       
                         (list(grab-random (second m))))) 
                )]
  (map next-word ws)))
|#



























      


              












