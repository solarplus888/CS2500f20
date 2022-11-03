;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab11) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise (Reviewed) 1
;Finish the data design for the above data definitions.

(require 2htdp/image)
(define WIDTH 500)
(define HEIGHT 500)
(define MARGIN-SIZE 25)
(define GRAPH-SIZE (- WIDTH (* MARGIN-SIZE 2)))
(define OFFSET (+ MARGIN-SIZE (/ GRAPH-SIZE 2)))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define FONT-SIZE 15)
(define NODE-SIZE 20)

; A Node is a (make-node Nat Color [List-of Nat])
(define-struct node [id color neighbors])
; - where id is the unique identifier for the given node
; - color is the node's current color
; - and neighbors is a list of the ids of nodes this one is connected to
(define NODE1 (make-node 1 "purple" (list 2)))
(define NODE2 (make-node 2 "green" empty))
(define NODE1-UPDATE (make-node 1 "green" (list 2)))
(define NODE7 (make-node 7 "red" (list 8 9)))
(define NODE8 (make-node 8 "orange" (list 7)))
(define NODE9 (make-node 9 "orange" (list 7)))
(define NODE7-UPDATE (make-node 7 "orange" (list 8 9)))
(define NODE8-UPDATE (make-node 8 "red" (list 7)))
(define NODE9-UPDATE (make-node 9 "red" (list 7)))

(define (node-temp x)
  (..(node-id x)
     (node-color x)
     (node-neighbors x)...))

; A Graph is a [List-of Node]
(define GRAPH-12 (list NODE1 NODE2))
(define GRAPH-12-UPDATE (list NODE1-UPDATE NODE2))
(define GRAPH-789 (list NODE7 NODE8 NODE9))
(define GRAPH-789-UPDATE (list NODE7-UPDATE NODE8-UPDATE NODE9-UPDATE))

(define (graph-temp x)
  (...
   (cond
     [(empty? x)...]
     [(cons? x)...
      (...(node-temp(first x))
          (graph-temp(rest x))...)])))

; get-node-by-id : Graph Nat -> Node
; Get the node with the given ID
(check-error (get-node-by-id '() 5))
(check-expect (get-node-by-id GRAPH-789 8) NODE8)
(define (get-node-by-id graph id)
 (cond [(empty? graph)
        (error (string-append "Could not find node with id " (number->string id)))]
       [(cons? graph)
        (if (same-id? id (first graph))
            (first graph)
            (get-node-by-id (rest graph) id))]))
 
; same-id? : Nat Node -> Boolean
; Does the given node have the given ID?
(check-expect (same-id? 7 NODE7) true)
(check-expect (same-id? 8 NODE9) false)
(define (same-id? id n)
 (= id (node-id n)))

;Exercise (Reviewed) 2
;Design the function can-reach-in-time? which takes a Graph, the IDs of two nodes in the Graph, and a natural number.
;It determines whether you can get from the node with the first ID to the node with the second ID in the given number of steps.
;A step is when you move from a node to its neighbor. To help, we’ve provided a couple of helper functions:

;neighbors: Node Graph -> [List-Of Node]
;It consumes a Node n and a Graph g and produces the list of immediate neighbors of n in g.
;(check-expect (neighbors NODE7 GRAPH-789)
;              (list NODE8 NODE9))

;(check-expect (neighbors NODE1 GRAPH-789)
;              empty)

(check-expect (neighbors 'A sample-graph)
              (list 'B 'E))

(check-expect (neighbors 'D sample-graph)
              empty)

(define (neighbors n g)  
 (rest(first(filter (λ(l)(equal? (first l) n)) g))))

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

; A Node is a Symbol.
; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 

; find-path: Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
;find-path/list: [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

;can-reach-in-time?: Graph Node Node Natural -> Boolean
;as above
(check-expect (can-reach-in-time? sample-graph 'C 'D 1)#true)
(check-expect (can-reach-in-time? sample-graph 'C 'D 2)#true)
(check-expect (can-reach-in-time? sample-graph 'C 'G 2)#false)
(check-expect (can-reach-in-time? sample-graph 'E 'D 2)#true)
(check-expect (can-reach-in-time? sample-graph 'E 'D 1)#false)

(define (can-reach-in-time? G origination destination n)
  (if (<=
       (- (length
              (if (list? (find-path origination destination G))
                  (find-path origination destination G)
                  G)) ; assume G has the max length, this may not be correct.
                        
           1)
        n)
      #true
      #false))
#|
Exercise 3
Write down the signatures and purpose statements for the handler functions you need. This is your "wishlist" of functions that you will need to create.
If you need help, please check in with a staff member.
|#

(require 2htdp/universe)
; moody-graph : Graph -> Graph
; Draw the graph and change its nodes to have the color of the majority of their neighbors
(define (moody-graph graph)
  (big-bang graph
    [to-draw draw-graph]
    ;[on-tick update-graph 0.5]
    ))

; draw-graph : Graph -> Image
 ; Draw all the nodes in the graph and their connections
 (check-expect (draw-graph '()) BACKGROUND)
 (check-expect
  (draw-graph GRAPH-12)
  (place-image
   (overlay (text "1" FONT-SIZE "white") (circle NODE-SIZE "solid" "purple"))
   475 250
   (place-image
    (overlay (text "2" FONT-SIZE "white") (circle NODE-SIZE "solid" "green"))
    25 250
    (add-line BACKGROUND 475 250 25 250 "black"))))
 (define (draw-graph all-nodes)
   (local [(define node-coordinates (find-node-coordinates (length all-nodes)))]
     (draw-nodes-as-dots
      all-nodes node-coordinates
      (draw-all-connections all-nodes node-coordinates))))
 
 ; find-node-coordinates : Nat -> [List-of Posn]
 ; Produce coordinates for the given number of nodes
 (check-expect (find-node-coordinates 0) empty)
 (check-expect
  (find-node-coordinates 2)
  (list (make-posn 475 250) (make-posn 25 250)))
 (define (find-node-coordinates num-nodes)
   (build-list num-nodes (λ (index) (get-coordinates-at-index index num-nodes))))
 
 ; get-coordinates-at-index : Nat Nat -> Posn
 ; Find the coordinates for a node at the given index in a list of the given length
 (check-expect (get-coordinates-at-index 10 20) (make-posn 25 250))
 (check-expect (get-coordinates-at-index 7 8) (make-posn 409 91))
 (define (get-coordinates-at-index index total)
   (make-posn
    (inexact->exact (round (+ (* (cos (* (/ index total) 2 pi)) 1/2 GRAPH-SIZE) OFFSET)))
    (inexact->exact (round (+ (* (sin (* (/ index total) 2 pi)) 1/2 GRAPH-SIZE) OFFSET)))))
 
 ; draw-nodes-as-dots : [List-of Node] [List-of Posn] Image -> Image
 ; Draw each of the nodes as a dot on the given image
 (check-expect (draw-nodes-as-dots '() '() (circle 2 "solid" "blue")) (circle 2 "solid" "blue"))
 (check-expect
  (draw-nodes-as-dots GRAPH-12 (list (make-posn 30 40) (make-posn 70 20)) BACKGROUND)
  (place-image (overlay (text "1" FONT-SIZE "white") (circle NODE-SIZE "solid" "purple")) 30 40
               (place-image (overlay (text "2" FONT-SIZE "white") (circle NODE-SIZE "solid" "green"))
                            70 20 BACKGROUND)))
 (define (draw-nodes-as-dots all-nodes all-coordinates bg-img)
   (foldr (λ (n c sofar) (draw-circle c (node-id n) (node-color n) sofar))
          bg-img all-nodes all-coordinates))
 
 ; draw-circle : Posn Nat Color Image -> Image
 ; Draw a circle of the given color at the given position on the given background
 (check-expect
  (draw-circle (make-posn 30 40) 7 "red" (empty-scene 200 300))
  (place-image (overlay (text "7" FONT-SIZE "white") (circle NODE-SIZE "solid" "red"))
               30 40 (empty-scene 200 300)))
 (check-expect
  (draw-circle (make-posn 0 0) 100 "purple" BACKGROUND)
  (place-image (overlay (text "100" FONT-SIZE "white") (circle NODE-SIZE "solid" "purple"))
               0 0 BACKGROUND))
 (define (draw-circle p id color bg)
   (place-image (overlay (text (number->string id) FONT-SIZE "white")
                         (circle NODE-SIZE "solid" color))
                (posn-x p) (posn-y p) bg))
 
 ; draw-all-connections : [List-of Node] [List-of Posn] -> Image
 ; Draw the lines between each node and its neighbors
 (check-expect (draw-all-connections '() '()) BACKGROUND)
 (check-expect
  (draw-all-connections GRAPH-12 (list (make-posn 1 2) (make-posn 30 40)))
  (add-line BACKGROUND 1 2 30 40 "black"))
 (define (draw-all-connections all-nodes all-coordinates)
   (local [; get-neighbor-coordinates : Node -> [List-of Posn]
; Get the coordinates of all of this node's neighbors
(define (get-neighbor-coordinates n)
(map (λ (id) (get-coordinates-for-node id all-nodes all-coordinates))
     (node-neighbors n)))]
     (foldr (λ (n c sofar) (draw-connections c (get-neighbor-coordinates n) sofar))
            BACKGROUND all-nodes all-coordinates)))
 
 ; get-coordinates-for-node : Nat [List-of Node] [List-of Posn] -> Posn
 ; Get the coordinates for the node with the given ID
 (check-error (get-coordinates-for-node 10 '() '()))
 (check-expect
  (get-coordinates-for-node 8 GRAPH-789 (list (make-posn 1 2) (make-posn 30 40) (make-posn 100 100)))
  (make-posn 30 40))
 (define (get-coordinates-for-node id all-nodes all-coordinates)
   (cond [(empty? all-nodes)
          (error (string-append "Could not find node with ID " (number->string id)))]
         [(cons? all-nodes)
          (if (same-id? id (first all-nodes))
              (first all-coordinates)
              (get-coordinates-for-node id (rest all-nodes) (rest all-coordinates)))]))
 
 ; draw-connections : Posn [List-of Posn] Image -> Image
 ; Draw lines from the given position to each of the coordinates in the list
 (check-expect
  (draw-connections (make-posn 1 2) empty (circle 5 "solid" "red"))
  (circle 5 "solid" "red"))
 (check-expect
  (draw-connections (make-posn 40 30) (list (make-posn 30 40) (make-posn 100 100)) BACKGROUND)
  (add-line (add-line BACKGROUND 40 30 30 40 "black") 40 30 100 100 "black"))
 (define (draw-connections p to-connect bg)
   (foldr (λ (p2 sofar) (line-between p p2 sofar)) bg to-connect))
 
 ; line-between : Posn Posn Image -> Image
 ; Add a line between the two coordinates to the given image
 (check-expect
  (line-between (make-posn 1 2) (make-posn 30 40) BACKGROUND)
  (add-line BACKGROUND 1 2 30 40 "black"))
 (check-expect
  (line-between (make-posn 100 100) (make-posn 0 0) (empty-scene 700 200))
  (add-line (empty-scene 700 200) 100 100 0 0 "black"))
 (define (line-between p1 p2 img)
   (add-line img (posn-x p1) (posn-y p1) (posn-x p2) (posn-y p2) "black"))

#|
Exercise 4 [INCOMLETE: Don't know how to process 2 lists together]
Consider the data definition given below:

; A [Count X] is a (make-count X Nat)
; and represents a count of the number of times an element has been seen in a list
Design the function count-all-colors which, given a [List-of Color], produces a [List-of [Count Color]]
which contains the count of how many times each color appeared in the given list.

(define-struct count [element times])

(define (count-temp x)
  (...(count-element x)
      (count-times x)...))

(check-expect (count-all-colors (list "red" "red" "orange"))
              (list (make-count "red" 2)(make-count "orange" 1)))

(define (count-all-colors x)
  (local [;list-all-colors:[List-Of Color] -> [List-Of Count]
          ;produce a list of all color with time = 1
          ;(check-expect ((list-all-colors (list "red" "red" "orange"))
          ;(list(make-count "red" 1)(make-count "red" 1)(make-count "orange" 1)))
            (define (list-all-colors x)
            (cond
              [(empty? x)empty]
              [(cons? x)(cons(make-count (first x) 1)
                             (list-all-colors (rest x)))]))

            (define LISTS (list-all-colors x))

            ;times: [List-Of Count] -> [List-Of [Count Color]]
            (define (times x)
              (if (equal? (count-element x)(count-element x))
                  (make-count (count-element x)(+ 1 (count-times x)))
                  (make-count (count-element x)(count-times x))))]
    (map times LISTS)))
|#

;Exercise 5 [INCOMPLETE:]
;Now design the function majority-color which takes a [List-of Node] and produces the Color that is most common.
;If there are no Nodes in the list your function should produce false.

;majority-color: [List-of Node] -> String
;as above

;1. Select the color from the list of Node.
;2. Process above list of color with (count-all-colors x)
;3. Chose the node with the biggest count-times

;Exercise 6 
;Design the function get-neighbor-nodes which takes a Graph and a Node and produces all the nodes in the graph that are neighbors of the given node.

;get-neighbor-nodes: Node Graph -> [List-Of Node]
;It consumes a Node n and a Graph g and produces the list of immediate neighbors of n in g.

(check-expect (get-neighbor-nodes NODE7 GRAPH-789)
              (list NODE8 NODE9))

(check-expect (get-neighbor-nodes NODE1 GRAPH-789)
              empty)

(define (get-neighbor-nodes n g)  
 (cond
     [(empty? g)empty]
     [(cons? g)
      (if (equal?(first g) n)
          (rest g)
          (get-neighbor-nodes n (rest g)))]))

#|
Exercise 7 [INCOMPLETE:]
Now use the above functions to define update-graph which takes a Graph and produces a Graph where each node in the original graph now has the majority color of its neighbors. If it has no neighbors, the color of the node should remain unchanged.
|#