;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise (Reviewed) 1
;Write the template for a [3Tree-of X].

(define-struct leaf [])
(define-struct node [value left middle right])
; A [3Tree-of X] is one of:
; - (make-leaf)
; - (make-node X [3Tree-of X] [3Tree-of X] [3Tree-of X])
; Interpretation:
; A [3Tree-of X] is either a leaf (which contains no information)
; or a (make-node v l m r) that has some value and left, middle,
; and right subtrees.
(define leaf-tree (make-leaf))
(define tree1 (make-node 1
                         (make-leaf)
                         (make-node 2 (make-leaf) (make-leaf) (make-leaf))
                         (make-node 3 (make-leaf) (make-leaf) (make-leaf))))

(define (3Tree-temp x)
  (...
   (cond
     [(leaf? x)empty]
     [(node? x)(node-value x)...
               (3Tree-temp(node-left x))...
               (3Tree-temp(node-middle x))...
               (3Tree-temp(node-right x))...
               ])))
#|
Exercise 2 & 3
Trees are just like lists but with a more interesting and flexible structure. So it stands to reason that we can create abstractions over trees like we do for lists.
Write just the signatures for the following functions:

3tree-map: Given a tree and a function to transform the data at each node, return a tree with the given function applied to the value at every node.

3tree-ormap: Given a tree and a predicate, return whether any node’s value in the tree passes the given predicate.
|#

;3tree-map: [3Tree-of X] [X -> Y] -> [3Tree-of Y]
;As above
(check-expect (3tree-map tree1 add1)
              (make-node (+ 1 1)
                         (make-leaf)
                         (make-node (+ 1 2) (make-leaf) (make-leaf) (make-leaf))
                         (make-node (+ 1 3) (make-leaf) (make-leaf) (make-leaf))))

(define (3tree-map x f)
  (cond
     [(leaf? x)(make-leaf)]
     [(node? x)(make-node
               (f (node-value x))
               (3tree-map(node-left x)f)
               (3tree-map(node-middle x)f)
               (3tree-map(node-right x)f))
               ]))

;3tree-ormap: [3Tree-of X] Boolean -> Boolean
;As above
(check-expect (3tree-ormap tree1 (λ(n) (> n 2)))#true)

(define (3tree-ormap x f?)
  (cond
     [(leaf? x)#false]
     [(node? x)(or(if (f?(node-value x))
                   #true
                   #false)
                  (3tree-ormap(node-left x)f?)
                  (3tree-ormap(node-middle x)f?)
                  (3tree-ormap(node-right x)f?))
               ]))

;Exercise 4
;Design a function 3tree-grab-x-coords which, given a [3Tree-of Posn] returns a tree with just the x-coordinates of each node.

;3tree-grab-x-coords: [3Tree-of Posn] -> [3Tree-of X]
;As above
(define tree2 (make-node (make-posn 8 1)
                         (make-node (make-posn 7 2) (make-leaf) (make-leaf) (make-leaf))
                         (make-node (make-posn 6 3) (make-leaf) (make-leaf) (make-leaf))
                         (make-node (make-posn 5 4) (make-leaf) (make-leaf) (make-leaf))))

(check-expect (3tree-grab-x-coords tree2)
              (make-node 8
                         (make-node 7 (make-leaf) (make-leaf) (make-leaf))
                         (make-node 6 (make-leaf) (make-leaf) (make-leaf))
                         (make-node 5 (make-leaf) (make-leaf) (make-leaf))))

(define (3tree-grab-x-coords x)
  (3tree-map x (λ(n)(posn-x n))))

;Exercise 5
;Design a function 3tree-count, which accepts a tree and a predicate and counts the number of nodes in the tree whose values pass the predicate.
;Hint: use the [3Tree-of X] template.              

;3tree-count: [3Tree-of X] Boolean -> Number
;As above
(check-expect (3tree-count tree1 (λ(n) (> n 0)))3)

(define (3tree-count x f?)
  (cond
     [(leaf? x)0]
     [(node? x)(+ (if (f?(node-value x))
                   1
                   0)
                  (3tree-count(node-left x)f?)
                  (3tree-count(node-middle x)f?)
                  (3tree-count(node-right x)f?))
               ]))

;Exercise 6
;Write the template for a FileSystemComponent. Check in with your tutor/TA before moving forward.

(define-struct file [name data])
(define-struct dir [name contents])
; A FSC (FileSystemComponent) is one of:
; - (make-file String String)
; - (make-dir String [List-of FSC])
; INTERPRETATION:
; a (make-file n d) represents a file with name n and data d
; a (make-dir n fscs) represents a directory with name n and contents fscs
(define FSC-1 (make-file "eng1" "I am number one."))
(define FSC-2 (make-file "eng2" "I am number two."))
(define FSC-3 (make-dir "eng" (list FSC-1 FSC-2)))
(define FSC-4 (make-file "math1" "I am number 1."))
(define FSC-5 (make-file "math2" "I am number 2."))
(define FSC-6 (make-dir "math" (list FSC-4 FSC-5)))
(define FSC-7 (make-dir "school" (list FSC-3 FSC-6)))

(define (fsc-temp x)
  (...
   (cond
     [(file? x)(file-name x)...(file-data x)]
     [(dir? x)(dir-name x)...
              (dir-temp(dir-contents x))])))

(define (dir-temp x)
  (...
   (cond
     [(empty? x)...]
     [(con? x)(fsc-temp(first x))...
              (dir-temp(rest x))...])))

;Exercise 7
;Design the function file-count, which determines the total number of files in a FileSystemComponent.

;file-count: FSC -> Number
;As above
(check-expect (file-count FSC-7)4)

(define (file-count x)
  (cond
     [(file? x)1]
     [(dir? x)(dir-file(dir-contents x))]))

(define (dir-file x)
  (cond
     [(empty? x)0]
     [(cons? x)(+ (file-count(first x))
                 (dir-file(rest x)))]))

;Exercise 8
;Design the function find-deepest-directory-depth which given a FSC determines the deepest number of layers a directory is nested anywhere in the filesystem.

;find-deepest-directory-depth: FSC -> Number
;As above
(check-expect (find-deepest-directory-depth FSC-7)2)

(define (find-deepest-directory-depth x)
  (cond
     [(file? x)0]
     [(dir? x)(add1 (dir-count(dir-contents x)))]))

(define (dir-count x)
  (cond
     [(empty? x)0]
     [(cons? x)(max (find-deepest-directory-depth(first x))
                 (dir-count(rest x)))]))

#|
Exercise 9 [INCOMPLETE: Don't know how to use foldr on mutually recursive structure.]
Recall the definition of a [Maybe X].
; A [Maybe X] is one of:
; - #false
; - X
Design the function find-full-path-to-file which accepts a FSC and a file name and
returns a [Maybe String] representing the full path to the file, and #false if a file with the given name does not exist.
For example, a file called foo.rkt in the directory homework in the directory documents has the path "documents/homework/foo.rkt"


;find-full-path-to-file: FSC String -> [Maybe String]
;As above
(check-expect (find-full-path-to-file FSC-7 "eng1")"school/eng/eng1")

(define (find-full-path-to-file x s)
  (cond
     [(file? x)(if (string=? (file-name x) s)                    
                   (string-append "/"(file-name x)".")
                   #false)]
     [(dir? x)(fd(dir-contents x)s)]))

(define (fd x s)
  (cond
     [(empty? x)"!"]
     [(cons? x)(list(find-full-path-to-file(first x) s)
                    (fd(rest x)s))]))

|#

#|
Exercise 10 [INCOMPLETE]
Design the function draw-filesystem which draws an image representation of a given filesystem.
The exact aesthetics are up to you, but the image should clearly convey the structure of the filesystem, along with the names of the directories and files.
Include how many files are contained within a directory. Here’s a beautiful example of what this could look like:
|#




