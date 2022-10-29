;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW11) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise 1
;Design the predicate function list-prefix? that accepts two lists of Numbers, and returns #true if the first list is a prefix of the second list,
;meaning all the elements of the first list occur, in that specific order, as the first n elements of the second list.

;list-prefix?: [List-Of Number][List-Of Number] -> Boolean
;as above

(check-expect (list-prefix? (list 1 2 3)(list 2 3 4))#true)
(check-expect (list-prefix? (list 1 1 1)(list 0 0 0))#false)
(check-expect (list-prefix? (list 2 3 4)(list 1 2 3))#false)
(check-expect (list-prefix? (list 3 3 3)(list 4 4 4))#true)
(check-expect (list-prefix? (list 2 3 4)(list 3 4 4))#false) 

(define (list-prefix? x y)
  (cond
    [(or (empty? x)(empty? y)) #true]
    [else(and (< (first x) (first y))(list-prefix? (rest x) (rest y)))]
    ))

#|
Exercise 2
Design the function max-splice which given two lists of Numbers, will create the shortest resulting list which begins with all of the elements of the first list,
in original order, and ends with all of the elements of the second list, again in original order, taking advantage of any overlap to shorten the result.

Here are a couple of illustrative examples:
(check-expect (max-splice '(1 2 3 4) '(2 3 4 5)) '(1 2 3 4 5))
; but:
(check-expect (max-splice '(1 2 3 4) '(2 2 3 4 5)) '(1 2 3 4 2 2 3 4 5))
In your implementation, you may use your solution for list-prefix? from the previous exercise.


;max-splice: [List-Of Number][List-Of Number] -> [List-Of Number]
;as above


(check-expect (max-splice '() '(2 3 4 5)) '(2 3 4 5))
(check-expect (max-splice '(1 2 3 4) '()) '(1 2 3 4))
(check-expect (max-splice '(1 2 3 4) '(2 2 3 4 5)) '(1 2 3 4 2 2 3 4 5))
(check-expect (max-splice '(1 2 3 4) '(2 3 4 5)) '(1 2 3 4 5))

(define (max-splice x y)
  (cond
    [(or (empty? x)(empty? y))(if (empty? x) y x)]
    [else (append x
                  (if (list-prefix? x y)
                      (local [;select:[List-Of Number] -> [List-Of Number]
                              ;Interpretation: select item in second list that is not in the first list
                              ;(check-exect (select '(2 3 4 5))'(5))
                              (define (select n)
                                (cond
                                  [(empty? n)empty]
                                  [(cons? n)(if (not(member? (first n) x))
                                                (cons (first n) (select (rest n)))
                                                (select (rest n)))]))]
                        (select y))
                      y))]))
|#
#|
Exercise 3 [INCOMPLETE: unable to process the local functions]
Design the predicate function valid-results?, which is given three lists of equal length:
a list of input values

a list of functions

a list of results

For each member i of the first list, it will apply the function at the corresponding position in the second list to i,
and compare the output with the result in the corresponding position in the third list. valid-results? should return #true
if all of the function call outputs match the expected results.
|#

(define (f-plus x)
  (+ x 2))

(define (f-minus x)
  (- x 2))

(define (f-multiply x)
  (* x 2))

;valid-results?: [List-Of X][X -> Y][List-Of Y] -> Boolean
(check-expect (valid-results? (list 1 2 3)(list f-plus f-minus f-multiply)(list 0 0 0)) #false)
(check-expect (valid-results? (list empty)(list f-plus f-minus f-multiply)(list (+ 1 2)(- 2 2)(* 3 2)))#false)
;(check-expect (valid-results? (list 1 2 3)(list f-plus f-minus f-multiply)(list (+ 1 2)(- 2 2)(* 3 2)))#true)

(define (valid-results? lox lof lor)
  (cond
    [(and (empty? lox)
          (empty? lof)
          (empty? lor))
         #true]
    [(and (cons? lox)
          (cons? lof)
          (cons? lor))
          
          (and (equal? (local[(define (first-f x) ; unable to process the local functions
                                (first lof))]
                         (first-f (first lox)))
                       (first lor))
               (valid-results?(rest lox)(rest lof)(rest lor)))]
    [else #false]))

#|
Exercise 4
Design the function assign, which matches up members of a list of roles, each of type Symbol, with corresponding members of a list of people,
which are of type String; if the list of people is shorter, unfilled roles are paired with #false (so the second field should be a [Maybe String];
however, if the list of people is longer, the extra people (beyond the number of roles) should be ignored.
The result should be a [List-of Assignment], defined here:
|#

(define-struct assignment (role person))
; An Assignment is a (make-assignment Symbol [Maybe String])

(define (assignment-temp x)
  (...(assignment-role x)
      (assignment-person x)...))

;assign:[List-Of X][List-Of Y] -> [List-Of Assignment]
;As above
(check-expect (assign (list 'male-main 'female-main 'male-support 'female-support)
                      (list "ali" "aminah" "budin" "bibi"))
              (list (make-assignment 'male-main "ali")(make-assignment 'female-main "aminah")(make-assignment 'male-support "budin")(make-assignment 'female-support "bibi")))

(check-expect (assign (list 'male-main 'female-main 'male-support 'female-support)
                      (list "ali" "aminah" "budin" "bibi" "citi"))
              (list (make-assignment 'male-main "ali")(make-assignment 'female-main "aminah")(make-assignment 'male-support "budin")(make-assignment 'female-support "bibi")))

(check-expect (assign (list 'male-main 'female-main 'male-support 'female-support)
                      (list "ali" "aminah"))
              (list (make-assignment 'male-main "ali")(make-assignment 'female-main "aminah")(make-assignment 'male-support #false)(make-assignment 'female-support #false)))


(define (assign los lop)
  (cond
    [(and(empty? los)(empty? lop))empty]
    [(and (empty? los)(cons? lop))empty]
    [(and (cons? los)(empty? lop))(cons(make-assignment (first los) #false)(assign (rest los)lop))]
    [(and (cons? los)(cons? lop))(cons(make-assignment (first los)(first lop))(assign (rest los)(rest lop)))]))

#|
Exercise 5
For this problem, you will design the function tree-equiv, which compares two trees for alignment, with an important twist:
it allows any two trees to be considered equivalent if each contains subtrees equivalent to one of the subtrees in the corresponding other tree.
So, these two trees would be reported as equivalent:

; 
;          a                          a
;       /     \                    /     \
;      b       c                  c       b  
;     / \     / \                / \     / \
;    d   e   f   g              f   g   e   d
; 

Another way to think of it is: for a family tree, what if instead of a father and mother, you just identified two equivalent parents?
(For this, you will need more than the typical number of test cases.)

For the next set of exercises, you will be using a very simple version of a data type for representing a binary tree:
|#
(define-struct bt [value left right])
; A BT (Binary Tree) is one of:
; - 'none
; - (make-bt Symbol BT BT)

(define BT-0 empty)
(define BT-D (make-bt 'D BT-0 BT-0))
(define BT-E (make-bt 'E BT-0 BT-0))
(define BT-F (make-bt 'F BT-0 BT-0))
(define BT-G (make-bt 'G BT-0 BT-0))
(define BT-Y-ex7 (make-bt 'BT-Y-ex7 BT-0 BT-0))
(define BT-X-ex7 (make-bt 'BT-X-ex7 BT-F BT-0))
(define BT-B (make-bt 'B BT-D BT-E))
(define BT-C (make-bt 'C BT-F BT-G))
(define BT-A (make-bt 'A BT-B BT-C))

(define BT-B-x (make-bt 'BT-B-x BT-E BT-D))
(define BT-A-1 (make-bt 'BT-A-1 BT-C BT-B-x))
(define BT-B-ex7 (make-bt 'BT-B-ex7 BT-D BT-Y-ex7))
(define BT-A-ex7 (make-bt 'BT-A-ex7 BT-B-ex7 BT-X-ex7))
(define BT-B-ex7-r1 (make-bt 'BT-B-ex7-r1 BT-D BT-0))
(define BT-A-ex7-r1 (make-bt 'BT-A-ex7-r1 BT-B-ex7-r1 BT-0))
(define BT-B-ex7-r2 (make-bt 'BT-B-ex7-r2 BT-0 BT-0))
(define BT-A-ex7-r2 (make-bt 'BT-A-ex7-r2 BT-B-ex7-r2 BT-0))

(define (BT-temp x)
  (...
   (cond
     [(empty? x)...]
     [(bt? x)(...(bt-value x)
                 (bt-left x)
                 (bt-right x)...)])))

;tree-equiv: BT BT -> Boolean
;as above

(check-expect (tree-equiv BT-A BT-A-1)#true)
(check-expect (tree-equiv BT-A BT-A)#true)
(check-expect (tree-equiv BT-A BT-B)#false)

(define (tree-equiv x y)
  (cond
     [(and (empty? x)(empty? y))
      0]
     [(and (bt? x)(bt? y))
      (local [;level: BT -> Number
              ;Get the height of the left side and right side of the tree
              (define (level l)
                (cond
                  [(empty? l)0]
                  [(bt? l)(+ (+ 1 (level (bt-left l))
                             (+ 1 (level (bt-right l)))))]))


              ;cross: BT BT -> Boolean
              ;Check whether the left side of BT1 is the same as the right side of BT2 and vice versa
              (define (cross x y)
                (cond
                  [(and (empty? x)(empty? y))
                   #true]
                  [(and (bt? x)(bt? y))
                   (or (equal? (bt-left x)(bt-right y))
                       (equal? (bt-right x)(bt-left y))
                       (cross (bt-left x)(bt-right y))
                       (cross (bt-right x)(bt-left y)))]))]
        (and (equal? (level x) (level y))(cross x y)))]
     [else #false]))

#|
Exercise 6
Design the function find-subtree, which given two parameters: a tree, as well as a subtree to scan for (both of type BT),
will search the main tree (the first parameter) for the matching subtree. Here, we are looking for an exact match, all the way down to the leaves (the value 'none),
and not a flippable one as in the previous exercise.
Note that values stored at various nodes can repeat, so you cannot simply look for a matching root node and assume the subtree therefore matches.
Also, you can stop at the first full match, in the case where the subtree occurs multiple times in the main tree.
|#

;find-substree: BT BT -> Boolean
;As above
(check-expect (find-substree BT-A BT-B-x) #false)
(check-expect (find-substree BT-A BT-A-1) #false)
(check-expect (find-substree BT-A BT-A) #true)
(check-expect (find-substree BT-A-1 BT-B-x) #true)

(define (find-substree x y)
  (cond
     [(and (empty? x)(empty? y))
      #true]
     [(and (bt? x)(bt? y))
      (or (equal? (make-bt (bt-value x)(bt-left x)(bt-right x))(make-bt (bt-value y)(bt-left y)(bt-right y)))
          (equal? (bt-left x)(make-bt (bt-value y)(bt-left y)(bt-right y)))
          (equal? (bt-right x)(make-bt (bt-value y)(bt-left y)(bt-right y))))
           ]))

#|
Exercise 7 [INCOMPLETE: Unable to jump into the correct conditional branch]
Design a function max-common-tree, which given two binary trees (again, continuing with the data type given earlier in this homework, of type BT),
will compare the two trees starting with the root of each, returning a tree that shares the maximum number of nodes with both source trees, starting from the root.
Since the resulting tree must also be a BT, it will have leaves with the symbol 'none—those do not have to match the respective nodes in the source trees
however, all make-bt nodes in the returned common tree must match the corresponding nodes in both source trees, both in position within the tree and in their stored value.
Your function must find the maximum such common tree.

An example: if your two source trees are as follows (where ’n’ in the figures indicates 'none):
; 
;  t1:     A                 t2:      A
;       /     \                    /     \
;      B       C                  B       X  
;     / \     / \                / \     / \
;    D   E   F   G              D   Y   F   n
;   / \ / \ / \ / \            / \ / \ / \
;   n n n n n n n n            n n n n n n 
 
;            result1:   A         result2:    A
;                    /     \               /     \
;                   B       n             B       n
;                  / \                   / \
;                 D   n                 n   n
;                / \
;               n   n
; 
The tree result1 is the correct answer. Note that the node 'F doesn’t appear in the common tree, even though it appears in the same position, and with the same value,
in both source trees, because the node 'C in tree t1 does not match corresponding node 'X in tree t2, cutting off anything below them.
While result2 is in fact a common tree of both t1 and t2, it is not the maximal such tree, and is therefore incorrect.
|#

;max-common-tree: BT BT -> BT
;As above
(check-expect (max-common-tree BT-0 BT-0)BT-0)
(check-expect (max-common-tree BT-A BT-A)BT-A)
;(check-expect (max-common-tree BT-A BT-A-ex7)BT-A-ex7-r1)

(define (max-common-tree x y)
     (cond
        [(or (empty? x)(empty? y))
         BT-0]
        [(and (equal?(bt-left x)(bt-left y))
              (equal?(bt-right x)(bt-right y)))
         (make-bt (bt-value x)
                   (max-common-tree (bt-left x)(bt-left y))
                   (max-common-tree (bt-right x)(bt-right y)))]
        [(and (not(equal?(bt-left x)(bt-left y)))
              (equal?(bt-right x)(bt-right y)))
         (make-bt (bt-value y)
                   BT-0
                   (max-common-tree (bt-right x)(bt-right y)))]
        [(and (equal?(bt-left x)(bt-left y))
              (not(equal?(bt-right x)(bt-right y))))
         (make-bt (bt-value x)
                  (max-common-tree (bt-left x)(bt-left y))
                  BT-0)]
        [else "aaa"]))


#|
Exercise 8
To find an existing value in a Binary Search Tree (BST), you have to turn left or right correctly at every node as you descend a BST,
by examining the value stored at each node you visit, and then deciding to go down the left or right branch.
Your goal for this exercise is to design a function that can validate a search algorithm’s decisions as it descends a BST,
as well as the correctness of the BST’s structure, at least the part you traverse.

You will design a function valid-bst-path? , which is passed a binary search tree, and a number that is stored in that tree, as well as a search path,
defined as a [List-of Dir], i.e., a sequence of 'left and 'right turns. Your function must validate every left/right decision in the given path,
as well as whether you end up at the correct node.
It should return #true if the passed path makes the correct branching decision at each node, as well as arriving at the desired node.
Note that just arriving at the correct node at the end is not sufficient, since you are also validating parts of what might be a malformed tree.
You do not have to validate the whole BST: just opportunistically check the parts along the path.

You can assume that a Dir data type is already defined, as follows:
; A Dir is one of:
; - 'left
; - 'right

Note that while you should typically call out to a helper function to handle the Dir data type in the [List-of Dir],
here you are allowed to handle it directly in the main function as we did in the lecture example involving a FamilyTree and AncestorPath,
but only if the design is still sufficiently simple and clear.
|#

;valid-bst-path?: BT Number [List-of Dir] -> Boolean
;Get the node according to the direction
(check-expect (valid-bst-path? BT-A 'A (list 'right 'left))#false) 
(check-expect (valid-bst-path? BT-A 'D (list 'left 'left))#true) 
(check-expect (valid-bst-path? BT-A-ex7 'A (list 'left 'right))#false)

(define (valid-bst-path? x s lod)
  (local [;find: BT Number [List-of Dir] -> BT
          ;Get the node according to the direction
          ;(check-expect (find BT-A 'A (list 'right 'left))(make-bt 'F '() '())) 
          ;(check-expect (find BT-A 'D (list 'left 'left))(make-bt 'D '() '())) 
          ;(check-expect (find BT-A-ex7 'A (list 'left 'right))(make-bt 'BT-Y-ex7 '() '()))
          (define (find x s lod)
            (local [;reverse-dir: lod -> lod
                    ;reverse the list
                    ;(check-expect (reverse-dir (list 'right 'left))(list 'left 'right))
                    (define (reverse-dir lod)
                      (cond
                        [(empty? lod)empty]
                        [(cons? lod)(append(reverse-dir(rest lod))(cons(first lod)empty))]))]
              (cond
                [(empty? lod)x]
                [(cons? lod)
                 (if (symbol=? (first (reverse-dir lod))'right)
                     (bt-right (find x s (rest (reverse-dir lod))))
                     (bt-left (find x s (rest (reverse-dir lod)))))])))]
    (if (symbol=? s (bt-value (find x s lod)))
        #true
        #false)))

#|
Exercise 9 [INCOMPLETE: unable to produce the correct listing due to recursion]
Design the function merge, that takes two ordered lists containing elements of the same type (not necessarily numbers!), and produces a single resulting ordered list.
Note that the user passes in a comparison function lt-func, which takes two parameters, and will return #true if the first parameter is "less than",
i.e., should come before, the second. You are not allowed to call DrRacket’s sort function!


;lt-func: X X -> Boolean
;as above
(check-expect (lt-func 2 3)#true)
(check-expect (lt-func 3 2)#false)
(check-expect (lt-func 3 3)#false)

(define (lt-func x y)
  (if (< x y)
      #true
      #false))

;merge: [List-Of X][List-Of X] -> [List-Of X]
;as above
(check-expect (merge (list 1 2 3)(list 4 5 6))(list 1 2 3 4 5 6))
(check-expect (merge (list 1 1 1)(list 2 2 2))(list 1 1 1 2 2 2))
(check-expect (merge (list 1 2 3)(list 4 4 4))(list 1 2 3 4 4 4))
(check-expect (merge (list 4 4 4)(list 1 2 3))(list 1 2 3 4 4 4))
(check-expect (merge (list 1 2 3)(list 1 2 3))(list 1 2 3 1 2 3))

(define (merge x y)
  (cond
    [(and(empty? x)(empty? y))empty]
    [(and(cons? x)(cons? y))
     (if
       (lt-func (first x)(first y))
        (append (cons(first x)empty)(merge (rest x)(rest y))y)
        (append (cons(first y)empty)(merge (rest y)(rest x))x))]))
|#      