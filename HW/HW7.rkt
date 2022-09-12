;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW7) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])
; 
; Interpretation: A list of items, where every item is an X.
 
; list-template : [List-of X] -> ?
(define (list-template l)
  (cond
    [(empty? l) ...]
    [(cons? l) (... (first l) ...
                    (list-template (rest l)) ...)]))

;Exercise 2
;Design a data definition that holds information about a single social media item, which may be a Twitter tweet, Facebook post, or Medium story.
;For a tweet, the data definition should hold the text of the tweet (at most 280 characters), the number of likes, and the number of retweets.
;For a Facebook post, the data definition should hold the text of the post, and the number of likes.
;For a Medium story, the data definition should hold the text of the story and the number of page views.

(define-struct tweet [text likes retweets])  
;A tweet is make-tweet (String Number Number)
; - tweet-text: text of the tweet (at most 280 characters)
; - tweet-likes: the number of likes
; - tweet-retweets: the number of retweets
;Interpretation:
;The information about a tweet of tweeter

(define (tweet-temp x)
  (... (tweet-text x)
       (tweet-likes x)
       (tweet-retweets x)...))

(define tweet1 (make-tweet "I am very happy today" 2 0))
(define tweet2 (make-tweet "Football game at stadium is the best" 1 0))
(define tweet3 (make-tweet "The rock is coming!" 10 0))
(define tweet4 (make-tweet "tttttttttttttttttttttt" 4 1))
(define tweet5 (make-tweet "tttttttttttttttttttttttttt" 0 0))
(define tweet6 (make-tweet "tttttttttttttttttttt" 1 0))

(define-struct post [text likes])
;A post is (make-post String Number)
; - post-text: the text of the post
; - post-likes: the number of likes
;Interpretation:
;The information about a post of facebook

(define (post-temp x)
  (...(post-text x)
      (post-likes x)...))

(define post1 (make-post "A very hot and humid afternoon, what should I do?" 4))
(define post2 (make-post "The badminton game is onging for more than 2hours and the crowd is huge." 14))
(define post3 (make-post "Bangkok trip: 5 days 4 night staying at the best hotel in the country" 24))
(define post4 (make-post "fffffffffffffffffffffffffffffffffffffffffffff" 6))
(define post5 (make-post "ffffffffffffffffffffffffff" 8))
(define post6 (make-post "fffffffffffffffffffffffffff" 0))

(define-struct story [text views])
;A story is make-story (String Number)
; - story-text: the text of the story
; - story-views: the number of page views
;Interpretation:
;The information about a story of medium

(define (story-temp x)
  (...(story-text x)
      (story-views x)...))

(define story1 (make-story "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called." 5))
(define story2 (make-story "Like many hopeful parents, we’d had our share of painful false starts during our time on adoption lists, so I hesitated to allow myself to believe in this one." 10))
(define story3 (make-story "They were perfect, beautiful, heart-melting, a girl and a boy whom we named Penelope Rose and Joseph August. Born premature, each weighed around five pounds." 3))
(define story4 (make-story "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" 5))
(define story5 (make-story "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" 0))
(define story6 (make-story "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" 6))
(define story7 (make-story "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 5))

(define LOSM-1 (list tweet1 tweet2 tweet3 post1 post2 post3 story1 story2 story3 tweet4 tweet5 tweet6 post4 post5 post6 story4 story5 story6))
(define LOSM-2 (list tweet1 tweet2 tweet3 post1 post2 post3 story1 story2 story3))
(define LOSM-3 (list tweet1 post1 story7))
(define LOSM-4 (list (make-post "I am very happy today" 0)
                                      (make-story "I am very happy today" 0)
                                      (make-tweet "A very hot and humid afternoon, what should I do?" 0 0)
                                      (make-story "A very hot and humid afternoon, what should I do?" 0)
                                      (make-tweet "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0 0)
                                      (make-post "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0)))


;Exercise 3
;Design a function that consumes a list of social media items and produces only the tweets in that list that have at least one retweet.
;You must use list abstractions to solve this problem.

;at-least1-retweet: [X][X -> Boolean][List-Of-X] -> [List-Of-X]
;Consumes a list of social media items and produces only the tweets in that list that have at least one retweet

(check-expect (at-least1-retweet LOSM-1)(list(make-tweet "tttttttttttttttttttttt" 4 1)))
(check-expect (at-least1-retweet LOSM-2)'())

(define (at-least1-retweet? l)
  (cond
    [(tweet? l)(if (> (tweet-retweets l)0)
                   #true
                   #false)]
    [(post? l) #false]
    [(story? l) #false]))
 
(define (at-least1-retweet l)
  (filter at-least1-retweet? l))

;Exercise 4
;Design a function that takes a list of social media items and produces only the items that have never been viewed, shared, liked, or retweeted.
;(These items were clearly utter failures!)

;failures: [X][X -> Boolean][List-Of X] -> [List-Of X]
;Takes a list of social media items and produces only the items that have never been viewed, shared, liked, or retweeted.

(check-expect (failures LOSM-1)(list tweet5 post6 story5))
(check-expect (failures LOSM-2)'())

(define (failures? l)
  (cond
    [(tweet? l)(if (and (= (tweet-likes l)0)(= (tweet-retweets l)0))
                   #true
                   #false)]
    [(post? l)(if (= (post-likes l)0)
                  #true
                  #false)]
    [(story? l)(if (= (story-views l)0)
                   #true
                   #false)]))
 
(define (failures l)
  (filter failures? l))

;Exercise 5
;Clients want to know how much engagement they are getting on social media.
;Design a function that takes a list of social media items and produces a single number that is the sum of the likes, retweets, and page views in that list.
;You know that it does not make sense to add tweets and page views. Sadly, not all your celebrity clients understand types and signatures as well as you do.

;sum:[X Y] [X -> Y] [List-of X] -> [List-of Y]
;List the likes, retweets, and page views in that list

(check-expect (sum LOSM-1)(list 2 1 10 4 14 24 5 10 3 5 0 1 6 8 0 5 0 6))
(check-expect (sum LOSM-2)(list 2 1 10 4 14 24 5 10 3))

(define (sm l)
             (cond
                  [(tweet? l)(+ (tweet-likes l) (tweet-retweets l))]
                  [(post? l)(post-likes l)]
                  [(story? l)(story-views l)]))

(define (sum l)
  (map sm l))

;engagement: [X Y][X Y -> Y] Y [List-Of X] -> Y
;Takes a list of social media items and produces a single number that is the sum of the likes, retweets, and page views in that list.

(check-expect (engagement LOSM-1)(+ 19 56 29))
(check-expect (engagement LOSM-2)(+ 13 42 18))

(define (engagement l)
  (foldr + 0 (sum l)))

;Exercise 6
;You know that it is critical to "cross post" anything you share on one platform to another platform, to maximize engagement.
;You have several older clients who use Facebook exclusively, and it is your job to copy their Facebook posts to Twitter and Medium.
;Similarly, you have clients that use Twitter exclusively, and other clients that use Medium exclusively. You can write a single function to handle all of these cases.
;Design a function called crosspost that consumes a list of social media items, and produces a list that has two items for each item in the given list.
;Each given Facebook post must produce a Tweet with the same text (limited to 280 characters) and zero retweets, and a Medium story with the same text and zero page views.
;Follow the same principle for tweets and stories in the original list, keeping in mind that tweets are character-limited.
;You must use the list template to solve this problem.
;Hint: See the append function, which is built in to ISL.

;crosspost:List-Of X -> List-Of Y
;Each given Facebook post must produce:
;- a Tweet with the same text (limited to 280 characters) and zero retweets.
;- a Medium story with the same text and zero page views.
;Follow the same principle for tweets and stories in the original list, keeping in mind that tweets are character-limited.

;(define tweet1 (make-tweet "I am very happy today" 2 0))
;(define post1 (make-post "A very hot and humid afternoon, what should I do?" 4))
;(define story7 (make-story "One sunny afternoon a year ago, I answered the phone and my whole life changed.
;                           I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxx
;                           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 5))

(check-expect (crosspost LOSM-3)(list (make-post "I am very happy today" 0)
                                      (make-story "I am very happy today" 0)
                                      (make-tweet "A very hot and humid afternoon, what should I do?" 0 0)
                                      (make-story "A very hot and humid afternoon, what should I do?" 0)
                                      (make-tweet "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0 0)
                                      (make-post "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0)))

(define (crosspost l)
  (cond
    [(empty? l)'()]
    [(cons? l) 
                (cond
                  [(tweet? (first l))(cons(make-post (tweet-text(first l)) 0)(cons(make-story (tweet-text(first l)) 0)(crosspost (rest l))))]
                  [(post? (first l))(cons(make-tweet (if (> (string-length(post-text(first l))) 280)
                                                         (substring (post-text(first l)) 0 279)
                                                         (post-text(first l)))
                                                         0 0)
                                         (cons(make-story (post-text(first l)) 0)(crosspost (rest l))))]
                  [(story? (first l))(cons(make-tweet (if (> (string-length(story-text(first l))) 280)
                                                          (substring (story-text(first l)) 0 279)
                                                          (story-text(first l)))
                                                          0 0)
                                          (cons(make-post (story-text(first l)) 0)(crosspost (rest l))))])]))

;Exercise 7
;Design a function called append-apply-to-all that consumes a function f and a list l.
;Your function will expect f to produce a list of results when applied to an element of l.
;The append-apply-to-all will append these lists together, and produce a single list as its result.
;For example, suppose (f x) produces (list a1 a2 a3) and (f y) produces (list b1 b2).
;In this case, (append-apply-to-all f (list x y)) must produce (list a1 a2 a3 b1 b2).
;You may solve this problem using any technique you like.

;append-apply-to-all: [X Y][X -> Y][List-Of X] -> List-Of Y
;Expect f to produce a list of results when applied to an element of l.

(check-expect (map append-apply-to-all (list 1 2 3)(list 4 5 6)) (list 5 7 9))

(define (append-apply-to-all x y)
  (+ x y))

;Exercise 8
;Solve the crosspost problem again, using append-apply-to-all as a helper function.
;Do not update your existing crosspost solution, but create a new version called crosspost/v2.
;You must use list abstractions to solve this problem.

;crosspost/v2: [X Y][X -> Y][List-Of X] -> List-Of Y
;Solve the crosspost problem again, using append-apply-to-all/v2 as a helper function.

(check-expect (crosspost/v2 LOSM-3)(list
 (list
  (make-post "I am very happy today" 0)
  (make-story
   "I am very happy today"
   0))
 (list
  (make-tweet
   "A very hot and humid afternoon, what should I do?"
   0
   0)
  (make-story
   "A very hot and humid afternoon, what should I do?"
   0))
 (list
  (make-tweet
   "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   0
   0)
  (make-post
   "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   0))))
              
(define (crosspost/v2 l)
  (map append-apply-to-all/v2 l))

(define (append-apply-to-all/v2 l)
    (cond
                  [(tweet? l)(cons(make-post (tweet-text l) 0)(cons(make-story (tweet-text l) 0)empty))]
                  [(post? l)(cons(make-tweet (if (> (string-length(post-text l)) 280)
                                                         (substring (post-text l) 0 279)
                                                         (post-text l))
                                                         0 0)
                                 (cons(make-story (post-text l) 0)empty))]
                  [(story? l)(cons(make-tweet (if (> (string-length(story-text l)) 280)
                                                          (substring (story-text l) 0 279)
                                                          (story-text l))
                                                          0 0)
                                  (cons(make-post (story-text l) 0)empty))]))

;Exercise 9
;Let us assume that we are working with social media items that are ordered by date:
;the first item in the list was the first post that was made, and the last item is the most recent post.
;Design a function called items-since-tweet that consumes a list of social media items (in order),
;and the text of a single tweet, and produces all the social media items that were made after the given tweet, and include the given tweet itself.
;You must use the list template to solve this problem.

;items-since-tweet: [List-Of X] Tweet -> [List-Of X]
;Consumes a list of social media items (in order),
;and the text of a single tweet, and produces all the social media items that were made after the given tweet, and include the given tweet itself.

(check-expect (items-since-tweet LOSM-4 "A very hot and humid afternoon, what should I do?")
              (list (make-tweet "A very hot and humid afternoon, what should I do?" 0 0)
                    (make-story "A very hot and humid afternoon, what should I do?" 0)
                    (make-tweet "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0 0)
                    (make-post "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0)))


(define (items-since-tweet l t)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (and (tweet? (first l))(string=? (tweet-text(first l)) t))
                   (cons (first l)(rest l))
                   (items-since-tweet (rest l) t))]))

;Exercise 10
;Design a function called items-since-10-likes that consumes a list of social media items (in order),
;and produces all the social media posts that were made after the first Facebook post that received 10 or more likes.
;You must include that Facebook post itself in the produced list.
;You must use the list template to solve this problem.

;items-since-10-likes: [List-Of X] -> [List-Of X]
;Produces all the social media posts that were made after the first Facebook post that received 10 or more likes.Include that Facebook post itself in the produced list.

(check-expect (items-since-10-likes (list (make-tweet "A very hot and humid afternoon, what should I do?" 0 0)
                                          (make-post "xxxxxxxxxxxxxxx" 11)
                                          (make-story "mmmmmmmmmmmmmmmmmmmm" 5)
                                           (make-post "wwwwwwwwwwwwwwwwwww" 2)))
                                    (list (make-post "xxxxxxxxxxxxxxx" 11)
                                          (make-story "mmmmmmmmmmmmmmmmmmmm" 5)
                                          (make-post "wwwwwwwwwwwwwwwwwww" 2)))


(define (items-since-10-likes l)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (and (post? (first l))(>= (post-likes(first l)) 10))
                   (cons (first l)(rest l))
                   (items-since-10-likes (rest l)))]))

;Exercise 11
;Design a function called suffix-from-2500 that consumes a list of numbers, and produces the suffix of that list that begins from the first 2500 that occurs in the given list.
;The produced list must include the first 2500.
;You must use the list template to solve this problem.

;suffix-from-2500; LoN -> LoN
;Consumes a list of numbers, and produces the suffix of that list that begins from the first 2500 that occurs in the given list.The produced list must include the first 2500.

(check-expect (suffix-from-2500 (list 31652222 31652500 45613333 78942500))(list 31652500 45613333 78942500))

(define (suffix-from-2500 l)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (= (string->number(substring (number->string (first l)) 4 8)) 2500)
                   (cons (first l)(rest l))
                   (suffix-from-2500 (rest l)))]))

;Exercise 12
;Design a function – a list abstraction – that abstracts the common pattern that appears in suffix-from-2500, items-after-10-likes, and posts-after-tweet.
;You must use the list template to solve this problem.

#|
(define (items-since-10-likes l)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (and (post? (first l))(>= (post-likes(first l)) 10))
                   (cons (first l)(rest l))
                   (items-since-10-likes (rest l)))]))              

(define (suffix-from-2500 l)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (= (string->number(substring (number->string (first l)) 4 8)) 2500)
                   (cons (first l)(rest l))
                   (suffix-from-2500 (rest l)))]))

(define (items-since-tweet l t)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (and (tweet? (first l))(string=? (tweet-text(first l)) t))
                   (cons (first l)(rest l))
                   (items-since-tweet (rest l) t))]))
|#

(define (abstract p? l)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (p? l)
                (cons(first l)(rest l))
                (abstract p? (rest l)))]))

;Exercise 13
;Using the list abstraction that you designed in the previous problem, write items-since-tweet/v2, items-since-10-likes/v2, and suffix-from-2500/v2.

(check-expect (items-since-10-likes/v2 (list (make-tweet "A very hot and humid afternoon, what should I do?" 0 0)
                                             (make-post "xxxxxxxxxxxxxxx" 11)
                                             (make-story "mmmmmmmmmmmmmmmmmmmm" 5)
                                             (make-post "wwwwwwwwwwwwwwwwwww" 2)))
                                       (list (make-post "xxxxxxxxxxxxxxx" 11)
                                             (make-story "mmmmmmmmmmmmmmmmmmmm" 5)
                                             (make-post "wwwwwwwwwwwwwwwwwww" 2)))

(define (like? l)
  (and (post? (first l))(>= (post-likes(first l)) 10)))

(define (items-since-10-likes/v2 l)
  (abstract like? l))


(check-expect (suffix-from-2500/v2 (list 31652222 31652500 45613333 78942500))(list 31652500 45613333 78942500))

(define (suffix? l)
  (= (string->number(substring (number->string (first l)) 4 8)) 2500))

(define (suffix-from-2500/v2 l)
  (abstract suffix? l))


(check-expect (items-since-tweet/v2 LOSM-4)
              (list (make-tweet "A very hot and humid afternoon, what should I do?" 0 0)
                    (make-story "A very hot and humid afternoon, what should I do?" 0)
                    (make-tweet "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0 0)
                    (make-post "One sunny afternoon a year ago, I answered the phone and my whole life changed. I was in the middle of a work trip, preparing to take off for the third state so far that day when my husband, Chasten, called.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 0)))

(define (text? l)
  (and (tweet? (first l))(string=? (tweet-text(first l)) "A very hot and humid afternoon, what should I do?"))
  )

(define (items-since-tweet/v2 l)
  (abstract text? l))
