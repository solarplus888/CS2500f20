;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise 1a
;You should only have to define two new data types: Material and CharmBracelet

(define-struct Material [silver gold pewter])

;Material is (make-material String String String)
;Interpretation:
; - Material-silver: represents the metal of silver
; - Material-gold: represents the metal of gold 
; - Material-pewter: represents the metal of pewter

(define (Material-temp x)
  (...(Material-silver x)
      (Material-gold x)
      (Material-pewter x)...))

(define MaterialS (make-Material "silver" "" ""))
(define MaterialG (make-Material "" "gold" ""))
(define MaterialP (make-Material "" "" "pewter"))

(define-struct clasp [end])
(define-struct Charm [des mat oth])

;A CharmBracelet is one of:
; - (make-clasp Number)
; - (make-Charm String Material CharmBracelet)

;Interpretation:
;Representing a Pandora-style charm bracelet.
;The bracelet will end with a clasp
;clasp is (make-clasp Number)
; - clasp-end: use number zero
;Charm is (make-charm String Material CharmBracelet)
; - Charm-des: description of the ornamental figure
; - Charm-mat: represent the three possible metals: silver, gold, or pewter
; - Charm-oth: a self-referential field that represents the rest of the bracelet

(define CLASP (make-clasp 0))
(define CharmBracelet0 CLASP)
(define CharmBracelet1 (make-Charm "Unicorn" MaterialS CLASP))
(define CharmBracelet2 (make-Charm "Heart" MaterialG CLASP))
(define CharmBracelet3 (make-Charm "Cupid" MaterialP CLASP))
(define CharmBracelet4 (make-Charm "Cupid" MaterialP CharmBracelet3))

(define (CharmBracelet-temp x)
  (...
  (cond
    [(clasp? x)(...(clasp-end)...)]
    [(Charm? x)
        (...(Charm-des x)...
        (...(Material-temp(Charm-mat x))...   
        (CharmBracelet-temp(Charm-oth x))...))])))

;Exercise 1b
;Design the function bracelet-cost, that takes a CharmBracelet parameter and returns the sale price for the bracelet,
;based on a cost of $15 for each gold charm, $12 for each silver charm, and $10 for each pewter charm.
;Create at least 4 interesting test cases for this function.

;bracelet-cost: CharmBracelet -> Number
;Takes a CharmBracelet parameter and returns the sale price for the bracelet.
;based on a cost of $15 for each gold charm, $12 for each silver charm, and $10 for each pewter charm

(check-expect (bracelet-cost CharmBracelet0) 0)
(check-expect (bracelet-cost CharmBracelet1) 12)
(check-expect (bracelet-cost CharmBracelet2) 15)
(check-expect (bracelet-cost CharmBracelet3) 10)
(check-expect (bracelet-cost CharmBracelet4) 20)

(define (bracelet-cost x)
  (cond
    [(clasp? x)0]
    [(Charm? x) (+ (material-cost(Charm-mat x))
               (bracelet-cost(Charm-oth x)))]))

;material-cost: Material -> Number
;Takes a Material parameter and returns the cost price for the material.
;based on a cost of $15 for each gold charm, $12 for each silver charm, and $10 for each pewter charm

(check-expect (material-cost MaterialS) 12)
(check-expect (material-cost MaterialG) 15)
(check-expect (material-cost MaterialP) 10)

(define (material-cost x)
  (cond
    [(string=? (Material-silver x) "silver") 12]
    [(string=? (Material-gold x) "gold") 15]
    [(string=? (Material-pewter x) "pewter") 10]))


;Exercise 2a
;Design the data type FancyBracelet, where the links can be either:
;a charm (described exactly as in Exercise 1); or
;a colored bead, for which you will define a struct with color and size, which are a simple string and number, respectively.

(define-struct bead [color size])

;A FancyBracelet is one of:
; - (make-bead String Number)
; - (make-Charm String Material FancyBracelet)

;Interpretation:
;A bead is (make-bead String Number)
; - bead-color: color of bead
; - bead-size: size of bead
;A charm is (make-Charm String Material FancyBracelet)
; - Charm-des: description of the ornamental figure
; - Charm-mat: represent the three possible metals: silver, gold, or pewter
; - Charm-oth: a self-referential field that represents the rest of the FancyBracelet

(define (FancyBracelet x)
  (...
   (cond
     [(bead? x) (...(bead-color x)
                    (bead-size x)...)]
     [(Charm? x) (...(Charm-des x)
                 ((Material-temp(Charm-mat x))   
                 (FancyBracelet(Charm-oth x))...))])))

(define bead1 (make-bead "red" 5))
(define bead2 (make-bead "yellow" 10))
(define bead3 (make-bead "green" 8))
(define FancyBracelet0 (make-bead "green" 8))
(define FancyBracelet1 (make-Charm "Unicorn" MaterialS bead1))
(define FancyBracelet2 (make-Charm "Heart" MaterialG FancyBracelet1))
(define FancyBracelet3 (make-Charm "Cupid" MaterialP bead2))

;Exercise 2b
;Design the function count-charms that takes a FancyBracelet,
;and counts the number of charms (and not beads) on the bracelet.

;count-charms: FancyBracelet -> Number
;Counts the number of charms (and not beads) on the bracelet

(check-expect (count-charms FancyBracelet0)0)
(check-expect (count-charms FancyBracelet1)1)
(check-expect (count-charms FancyBracelet2)2)
(check-expect (count-charms FancyBracelet3)1)

(define (count-charms x)
  (cond
     [(bead? x) 0]
     [(Charm? x) (+ 1   
                 (count-charms(Charm-oth x)))]))

;Exercise 2c
;Design the function upgrade-bracelet that takes three parameters: a FancyBracelet, a bead color, and a charm figure,
;and exchanges all of the beads of the given color in the bracelet for silver charms with the requested figure.
;Any beads of other colors, as well as any current charms, should be left as-is.

;upgrade-bracelet: FancyBracelet b c -> FancyBracelet
;Takes three parameters: a FancyBracelet, a bead color, and a charm figure,
;and exchanges all of the beads of the given color in the bracelet for silver charms with the requested figure

(check-expect (upgrade-bracelet FancyBracelet0 "green" "Heart")(make-bead "green" 8))
(check-expect (upgrade-bracelet FancyBracelet1 "green" "Heart")(make-Charm "Unicorn" MaterialS (make-bead "green" 5)))
(check-expect (upgrade-bracelet FancyBracelet2 "green" "Heart")(make-Charm "Heart" MaterialS (make-Charm "Unicorn" MaterialS (make-bead "green" 5))))
(check-expect (upgrade-bracelet FancyBracelet3 "green" "Heart")(make-Charm "Cupid" MaterialP (make-bead "green" 10)))

(define (upgrade-bracelet x b c)
  (cond
     [(bead? x) (if (not(string=? (bead-color x) b))
                    (make-bead b (bead-size x))
                    (make-bead (bead-color x) (bead-size x)))]
     [(Charm? x) (if (string=? (Charm-des x) c)
                    (make-Charm (Charm-des x) MaterialS (upgrade-bracelet (Charm-oth x) b c))
                    (make-Charm (Charm-des x) (Charm-mat x) (upgrade-bracelet (Charm-oth x) b c)))]))

;Exercise 3a
;Design a data type for a ListOfStudents, building on the Student data type.
;In this exercise, you are building a real BSL list, so you should be using cons and empty to build up your list.

(define-struct student [firstname lastname gpa on-coop])
; A Student is a (make-student String String Number Boolean)
; Interpretation: A (make-student fn ln g c) represents a
; Northeastern student whose first name is fn and last name is ln, with 
; cumulative grade point average g, and for whom c is #true if they are
; currently doing a coop experience this term and #false otherwise.
(define student1 (make-student "Jane" "Smith" 4.0 #true))
(define student2 (make-student "Ashok" "Singhal" 0.0 #false))
(define student3 (make-student "Amy" "Moore" 3.0 #true))
(define (student-templ st)
  (... (student-firstname st) ...
       (student-lastname st) ...
       (student-gpa st) ...
       (student-on-coop st) ...))

;A ListOfStudents (LoS) is one of:
; - '()
; - (cons student LoS)
;Interpretation:
;Representing a list of students.

(define (ListOfStudents-temp x)
  (...
   (cond
     [(empty? x)...]
     [(cons? x)(student-firstname (first x))
                (student-lastname (first x)) 
                (student-gpa (first x))
                (student-on-coop (first x))
                (ListOfStudents-temp (rest x))...])))

(define LoS-0 '())
(define LoS-1 (cons(make-student "Jane" "Smith" 4.0 #true) LoS-0))
(define LoS-2 (cons(make-student "Ashok" "Singhal" 0.0 #false) LoS-1))
(define LoS-3 (cons(make-student "Amy" "Moore" 3.0 #true) LoS-2))

;Exercise 3b
;Design the function count-coop-students that takes a ListOfStudents,
;and returns the number of students who are currently doing their coop experience.

;count-coop-students: ListOfStudents -> Number
;Takes a ListOfStudents, and returns the number of students who are currently doing their coop experience

(check-expect (count-coop-students LoS-0)0)
(check-expect (count-coop-students LoS-1)1)
(check-expect (count-coop-students LoS-2)1)
(check-expect (count-coop-students LoS-3)2)

(define (count-coop-students x)
  (cond
     [(empty? x)0]
     [(cons? x)(if (boolean=? (student-on-coop (first x)) #true)
                 (+ 1 (count-coop-students (rest x)))
                 (count-coop-students (rest x)))]))

;Exercise 3c
;Design the function exchange-coop-students that takes a ListOfStudents and flips each Student’s coop status,
;so that students who are currently doing a coop will now be listed as back at school,
;and students who are currently not doing a coop will now be listed as doing a coop.
;(In other words, toggle the coop status field.) You are expected to implement all simple boolean simplifications in doing this actual flip.

;exchange-coop-students: ListOfStudents -> ListOfStudents
;Takes a ListOfStudents and flips each Student’s coop status

(check-expect (exchange-coop-students LoS-0)'())
(check-expect (exchange-coop-students LoS-1)(cons(make-student "Jane" "Smith" 4.0 #false) LoS-0))
(check-expect (exchange-coop-students LoS-2)(cons(make-student "Ashok" "Singhal" 0.0 #true)(cons(make-student "Jane" "Smith" 4.0 #false) LoS-0)))
(check-expect (exchange-coop-students LoS-3)(cons(make-student "Amy" "Moore" 3.0 #false)(cons(make-student "Ashok" "Singhal" 0.0 #true)(cons(make-student "Jane" "Smith" 4.0 #false) LoS-0))))

(define (exchange-coop-students x)
  (cond
     [(empty? x)'()]
     [(cons? x)(if (boolean=? (student-on-coop (first x)) #true)
                 (cons (make-student (student-firstname (first x))(student-lastname (first x)) (student-gpa (first x))#false) (exchange-coop-students (rest x)))
                 (cons (make-student (student-firstname (first x))(student-lastname (first x)) (student-gpa (first x))#true) (exchange-coop-students (rest x))))]))


                 
                
               
  






