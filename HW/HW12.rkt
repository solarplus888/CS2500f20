;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW12) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Exercise 1

In the graphs that we have seen so far, nodes have labels and edges are not labelled. However, there are situations where it is useful to assign label both nodes and edges. For example, the following picture depicts a graph with three nodes labelled A, B, and C, and and three directed edges labelled north, east, and northeast.

image

Design a data definition called ELGraph (edge-labelled graph), for graphs with labelled nodes and edges. You must include the data definition itself, an interpretation, and at least two examples. You may omit a function template.

You may either (1) assume that nodes and edges are labelled with strings, or (2) give your data definition two parameters (i.e., [ELGraph X Y]), which are the signatures for node and edge labels.

Note that your examples for this exercise must be distinct from the two examples in the next exercise.
|#

; A ELGraph (edge-labelled graph) is a (make-elg String String [List-of String])
(define-struct elg [id desc neighbors])
; - where id is the unique identifier for the given node
; - desc is the edge's description
; - and neighbors is a list of the ids of nodes this one is connected to
(define B (make-elg "B" " " (list "")))
(define A (make-elg "A" "east" (list B)))
(define C (make-elg "C" "north" (list A)))
(define C-NE (make-elg "C-NE" "northeast" (list B)))

; A Graph is a [List-of ELG]
(define GRAPH (list C C-NE B A))

;Exercise 2
;Construct an ELGraph called given-street-graph, which represents the streets that we have highlighted.
(define Forsyth-Way-and-Huntington (make-elg "Forsyth Way and Huntington" " east" (list "Forsyth-St-and-Huntington")))
(define Forsyth-St-and-Huntington (make-elg "Forsyth St and Huntington" "east" (list "Opera-and-Huntington")))
(define Opera-and-Huntington (make-elg "Opera and Huntington" "north" (list "Opera-and-St-Stephen")))
(define Opera-and-St-Stephen (make-elg "Opera and St Stephen" "east" (list "St-Stephen-and-Gainsborough")))
(define St-Stephen-and-Gainsborough (make-elg "St Stephen and Gainsborough" "north" (list "Gainsborough-and-Hemenway")))
(define Gainsborough-and-Hemenway (make-elg "Gainsborough and Hemenway" "west" (list "Forsyth-Way-and-Huntington")))

(define given-street-graph (list Forsyth-Way-and-Huntington
                                 Forsyth-St-and-Huntington
                                 Opera-and-Huntington
                                 Opera-and-St-Stephen
                                 St-Stephen-and-Gainsborough
                                 Gainsborough-and-Hemenway))

;Exercise 3 [INCOMPLETE: Not familiar with how to extract list from constructor]
;Design a function called driving-directions that consumes (1) a graph of streets and directions (e.g., from the previous exercise), (2) the starting point, which is an intersection, and (3) the destination, which is another intersection. The function should produce a list of directions.

;neighbors: Node Graph -> [List-Of Node]
;It consumes a Node n and a Graph g and produces the list of immediate neighbors of n in g.
(check-expect (neighbors "Forsyth Way and Huntington" given-street-graph)
              (list(list "Forsyth-St-and-Huntington")))

(define (neighbors n g)
  (cons(elg-neighbors (first(local [(define (same g)
          (equal? (elg-id g) n)
          )]
   (filter same g))))empty))



;find-path/list: [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (driving-directions G (first lo-Os) D)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

;driving-directions: Graph Node Node -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
;(check-expect
;  (driving-directions given-street-graph
;                      "Forsyth Way and Huntington"
;                      "St Stephen and Gainsborough")
;  '("east to Forsyth St and Huntington"
;    "east to Opera and Huntington"
;    "north to Opera and St Stephen"
;    "east to St Stephen and Gainsborough"))

(define (driving-directions G origination destination)
  (cond
    [(equal? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

#|
Exercise 4 [INCOMMPLETE: Unable to complete Exe 3]
Exercise 5 & 6 [INCOMMPLETE: Unfamiliar with accumulator]
|#

