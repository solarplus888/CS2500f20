;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Exercise 2

Design the function print-sexpr, that takes in an SExpr and produces a string that looks like ISL code:
Numbers are rendered using number->string

Strings are rendered as themselves, surrounded by double-quotes. (For simplicity, you may assume that any strings in your s-expressions do not themselves contain any quotes or other special characters.)

Booleans are rendered as #true or #false

Symbols are rendered using symbol->string, and preceded by a single quote

Lists are rendered as the string "(list", followed by a space-separated string of rendering the expressions, followed by a closing ")". (The empty list would be rendered as "(list)" without any spaces.)
|#

; An Atom is one of
; - Number
; - String
; - Boolean
; - Symbol
; 
; An SExpr is one of
; - Atom
; - ListOfSExpr
; 
; A ListOfSExpr is one of
; - '()
; - (cons SExpr ListOfSExpr)

(define ATOM-N 1)
(define ATOM-ST "solar")
(define ATOM-B1 #true)
(define ATOM-B2 #false)
(define ATOM-SB 'super)
(define LOSE (list ATOM-N ATOM-ST ATOM-B1 ATOM-SB))

(define (SExpr-temp x)
  (...
   (cond
     [(number? x)...]
     [(string? x)...]
     [(boolean? x)...]
     [(symbol? x)...]
     [(list? x)(lose-temp x)])))

(define (lose-temp x)
  (...
   (cond
     [(empty? x)...]     
     [(cons? x)(...(SExpr-temp(first x))
                   (lose-temp(rest x))...)])))

;print-sexpr: SExpr -> String
;As above
(check-expect (print-sexpr LOSE)(list "(list" "1 solar #true 'super )"))
(check-expect (print-sexpr empty)"(list)")

(define (print-sexpr x)
  (cond
     [(number? x)(number->string x)]
     [(string? x)x]
     [(boolean? x)(if (boolean=? #true x) "#true" "#false")]
     [(symbol? x)(string-append "'"(symbol->string x))]
     [(list? x)(if (empty? x)
                   "(list)"
                   (list "(list"(lose-p x)))]))

(define (lose-p x)
  (cond
     [(empty? x)")"]     
     [(cons? x)(string-append(print-sexpr(first x))" "(lose-p(rest x)))]))

#|
Exercise 3

Consider the following data definition:
; A [Wide-tree X] is one of
; - X
; - [List-of [Wide-tree X]]

(define (wide-tree-temp x)
 (...
  (cond
   [(X? x)...]
   [(list? x)(lwt-temp x)])))

(define (lwt-temp x)
  (...
   (cond
     [(empty? x)...]     
     [(cons? x)(...(wide-tree-temp(first x))
                   (lwt-temp(rest x))...)])))

Design a function sexpr-map that takes in an SExpr and four functions (with signatures [Number -> X], [String -> X], [Boolean -> X] and [Symbol -> X]),
and produces a [Wide-tree X] that is the same shape as the original s-expression, but where each Atom in the s-expression has been transformed by the appropriate function.
|#

; A [Wide-tree String] is one of
; - String
; - [List-of [Wide-tree String]]

(define (f-number n)
  (number->string n))

(define (f-string n)
  n)

(define (f-boolean n)
 (if (boolean=? #true n) "#true" "#false"))

(define (f-symbol n)
  (string-append "'"(symbol->string n)))

(check-expect (sexpr-map LOSE f-number f-string f-boolean f-symbol)                           
              (list "1" "solar" "#true" "'super"))

;sexpr-map: SExpr [Number -> X] [String -> X] [Boolean -> X] [Symbol -> X] -> [Wide-tree X]
;As above

(define (sexpr-map x f1 f2 f3 f4)
  (cond
    [(number? x)(f1 x)]
    [(string? x)(f2 x)]
    [(boolean? x)(f3 x)]
    [(symbol? x)(f4 x)]
    [(list? x)(lose-x x f1 f2 f3 f4)]))

(define (lose-x x f1 f2 f3 f4)
  (cond
     [(empty? x)empty]     
     [(cons? x)(cons (sexpr-map (first x) f1 f2 f3 f4)(lose-x (rest x) f1 f2 f3 f4))]))

;Exercise 4
;Design a function all-numbers? to check whether every Atom in an SExpr is a number.

;all-numbers?: SExpr -> Boolean
;As above

(check-expect (all-numbers? LOSE)#false)
(check-expect (all-numbers? (list 1 2))#true)


(define (all-numbers? x)
  (cond
     [(number? x)#true]
     [(string? x)#false]
     [(boolean? x)#false]
     [(symbol? x)#false]
     [(list? x)(lose-num? x)]))

(define (lose-num? x)
  (cond
     [(empty? x)#true]     
     [(cons? x)(and (all-numbers?(first x))
                   (lose-num?(rest x)))]))


;Exercise 5
;Design a second function to check whether every Atom in an SExpr is the string "hi".

;all-string?: SExpr -> Boolean
;As above

(check-expect (all-string? LOSE)#false)
(check-expect (all-string? (list "hi" "hi"))#true)
(check-expect (all-string? (list "hi" "i"))#false)


(define (all-string? x)
  (cond
     [(number? x)#false]
     [(string? x) (if (string=? x "hi")#true #false)]
     [(boolean? x)#false]
     [(symbol? x)#false]
     [(list? x)(lose-s? x)]))

(define (lose-s? x)
  (cond
     [(empty? x)#true]     
     [(cons? x)(and (all-string?(first x))
                   (lose-s?(rest x)))]))

;Generalize this function and all-numbers? to a new abstraction for working with s-expressions.
;Rewrite your all-numbers? implementation to use this new abstraction.
;(Hint: it’s very much analogous to a list abstraction you’ve already seen.)

;all-numbers?/v2: SExpr [Number -> X] [String -> X] [Boolean -> X] [Symbol -> X] -> Boolean
;As above

(check-expect (all-numbers?/v2 LOSE f-number/v2 f-string/v2 f-boolean/v2 f-symbol/v2)#false)
(check-expect (all-numbers?/v2 (list 1 2) f-number/v2 f-string/v2 f-boolean/v2 f-symbol/v2)#true)

(define (f-number/v2 n)
  #true)

(define (f-string/v2 n)
  #false)

(define (f-boolean/v2 n)
 #true)

(define (f-symbol/v2 n)
  #false)

(define (all-numbers?/v2 x f1 f2 f3 f4)
  (cond
    [(number? x)(f1 x)]
    [(string? x)(f2 x)]
    [(boolean? x)(f3 x)]
    [(symbol? x)(f4 x)]
    [(list? x)(lose-x/v2 x f1 f2 f3 f4)]))

(define (lose-x/v2 x f1 f2 f3 f4)
  (cond
     [(empty? x)#true]     
     [(cons? x)(and (all-numbers?/v2 (first x) f1 f2 f3 f4)(lose-x/v2 (rest x) f1 f2 f3 f4))]))




