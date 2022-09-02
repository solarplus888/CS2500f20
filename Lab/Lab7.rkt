;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Lab7) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise (Reviewed) 3
;Below is the definition of foldr.

;(define (foldr combine base lst)
;  (cond [(empty? lst) base]
;        [(cons? lst) (combine (first lst)
;                              (foldr combine base (rest lst)))]))

;Determine the signature for foldr from this definition.

;[X Y] [X Y -> Y] Y [List-of-X] -> Y

;Exercise 4
#|
Determine the best pre-defined list abstraction to use for each of the following tasks.
Have a staff member look over your answers before you continue by visiting them in their group call.

Create a list of the string-lengths of every string in a list.
;build-list

Determine whether any Posn in a list is within a certain area.
;andmap

Given a list of strings, create a list of only the strings that contain only numbers.
;filter

Check whether every number in a list is within a certain range.
;andmap

Given a list of Posns, create a single Posn whose x-coordinate is the sum of all the x-coordinates in the list and whose y-coordinate is the sum of all the y-coordinates in the list.
foldr
|#

;Exercise 5
;Design the function accumulate-posn which, given a list of Posns,
;creates a single Posn whose x-coordinate is the sum of all the x-coordinates in the list,
;and whose y-coordinate is the sum of all the y-coordinates in the list (this is the last example from the previous exercise).

;add-posn: POSN POSN -> POSN
;Add the posn of x and y
(check-expect (add-posn (make-posn 2 4) (make-posn 1 1))(make-posn 3 5))

(define (add-posn x y)
  (make-posn (+ (posn-x x)(posn-x y))(+ (posn-y x)(posn-y y))))

;accumulate-posn: LoP -> POSN
;creates a single Posn whose x-coordinate is the sum of all the x-coordinates in the list,
;and whose y-coordinate is the sum of all the y-coordinates in the list
(define LOP-1 (list (make-posn 2 4) (make-posn 1 1)(make-posn 1 1)))
(check-expect (accumulate-posn LOP-1)(make-posn 4 6))

(define (accumulate-posn lst)
  (foldr add-posn (make-posn 0 0) lst))

#|
;Exercise 6 (INCOMPLETE)
;Determine the signature of the following function:
(define (f x y z)
  (or (andmap string-numeric?
              (map x (filter positive? (foldr append '() y))))
      z))

[X Y Z][List-Of-X] Y Z -> Boolean

[X Y][Y X ->Y] X [List-Of-Y] -> Y
[X Y][X -> Y] [List-Of-Y] -> [List-Of-X]
[X][X -> Boolean][List-Of-X]-> Boolean
[Z] Boolean Z -> Boolean
|#

;All Hands On Duck (OUTSTANDING)