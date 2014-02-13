;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bieber) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;; Assignment 5, Question 1
;; (bieber)
;; Daniel Holmes, 20418854
;; *******************************************************************
;; a.)

;; CONSTANT DEFINITIONS:
;; bieber-list: Contains the target list of symbols for the
;; starts-with-bieber? function.
(define bieber-list (list #\B #\i #\e #\b #\e #\r)) 

;; starts-with-bieber? (listof Character) -> Boolean
;; Purpose: Determines if the character sequence "Bieber" is present
;; in the given list of characters (list-of-char). This is defined
;; as "#\B #\i #\e #\b #\e #\r". Produces true if found, false if not.
;; Accomplishes all of the work of this task via delegation to the helper
;; function contains-bieber?.
;; Examples:
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b #\e #\r))
              true)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b #\e #\g))
              false)

(define (starts-with-bieber? list-of-char)
  (contains-bieber? list-of-char bieber-list))

;; Tests
(check-expect (starts-with-bieber? (list #\B))
              false)
(check-expect (starts-with-bieber? (list #\B #\i))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b #\e))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b #\e #\r))
              true)
(check-expect (starts-with-bieber? (list #\a #\i #\e #\b #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\B #\g #\e #\b #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\k #\b #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\x #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b #\t #\r))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b #\e #\z))
              false)
(check-expect (starts-with-bieber? (list #\B #\i #\e #\b #\e #\r))
              true)
(check-expect (starts-with-bieber? (list #\i #\e #\b #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\e #\b #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\e #\b #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\b #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\e #\r))
              false)
(check-expect (starts-with-bieber? (list #\r))
              false)

;; contains-bieber? (listof Character) (listof Character) -> Boolean
;; Purpose: Determines if the character sequence "Bieber" is present
;; in the given list of characters (list-of-char). This is defined
;; as "#\B #\i #\e #\b #\e #\r". Produces true if found, false if not.
;; Examples:
(check-expect (contains-bieber? (list #\B #\i #\e #\b #\e #\r) 
                                bieber-list)
              true)
(check-expect (contains-bieber? (list #\a #\i #\e #\b #\e #\r) 
                                bieber-list)
              false)

(define (contains-bieber? list-of-char list-of-bieber)
  (cond [(empty? list-of-bieber)
         true]
        [(empty? list-of-char)
         false]
         [(char=? (first list-of-char)
                    (first list-of-bieber))
         (contains-bieber? (rest list-of-char) 
                          (rest list-of-bieber))]
         [else false]))

;; Tests
(check-expect (contains-bieber? (list #\B) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\e) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\e #\b) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\e #\b #\e) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\g #\e #\b #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\k #\b #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\e #\x #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\e #\b #\t #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\e #\b #\e #\z) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\B #\i #\e #\b #\e #\r) 
                                bieber-list)
              true)
(check-expect (contains-bieber? (list #\i #\e #\b #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\e #\b #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\e #\b #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\b #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\e #\r) 
                                bieber-list)
              false)
(check-expect (contains-bieber? (list #\r) 
                                bieber-list)
              false)
;; *****************************************************************
;; b.)
;; add1-if-number: (Num or String) -> (Num or String)
;; Purpose: If the arguement is a number, the function produces the 
;; sum of the arguement and 1. Otherwise, it returns the original 
;; arguement.
;; Examples:
(check-expect (add1-if-number 1)
              2)
(check-expect (add1-if-number "one")
              "one")
(check-expect (add1-if-number 10)
              11)

(define (add1-if-number num-or-string)
  (cond [(number? num-or-string)
         (+ 1 num-or-string)]
        [else num-or-string]))

;; Tests:
(check-expect (add1-if-number 110000000000)
              110000000001)
(check-expect (add1-if-number 4)
              5)
(check-expect (add1-if-number -10)
              -9)
(check-expect (add1-if-number 0)
              1)
(check-expect (add1-if-number "RainyMood.com")
              "RainyMood.com")
(check-expect (add1-if-number "Is this enough tests yet?")
              "Is this enough tests yet?")
;; *****************************************************************
;; c.)
;; find-bieber: String -> (Int or String)
;; Purpose: Produces the location of the first letter of "Bieber"
;; found in a string, with the count starting at 1. 
;; Examples: 
(check-expect (find-bieber "Bieber")
              1)
(check-expect (find-bieber "*Bieber")
              2)
(check-expect (find-bieber "**Bieber")
              3)
(check-expect (find-bieber "Bi_ber")
              "No Biebers here!")

(define (find-bieber string-to-search)
  (find-bieber-repeater (string->list string-to-search)))

;; Tests
(check-expect (find-bieber "***Bieber")
              4)
(check-expect (find-bieber "****Bieber")
              5)
(check-expect (find-bieber "*****Bieber")
              6)
(check-expect (find-bieber "Dispose of Bieber")
              12)
(check-expect (find-bieber "_ieber")
              "No Biebers here!")
(check-expect (find-bieber "B_eber")
              "No Biebers here!")
(check-expect (find-bieber "Bie_er")
              "No Biebers here!")
(check-expect (find-bieber "Bieb_r")
              "No Biebers here!")
(check-expect (find-bieber "Biebe_")
              "No Biebers here!")
(check-expect (find-bieber "Biebe")
              "No Biebers here!")
(check-expect (find-bieber "Biebe r")
              "No Biebers here!")
(check-expect (find-bieber "Biebe Biebe Biebe r ffwefwefwefwef Bieber")
              36)
;; find-bieber-repeater: (listof Character) -> (Int or String)
;; Purpose: To perform the work for find-bieber. Determines the presence
;; and location of "Bieber" in the given list of characters, list-to-search.
;; Presence is inferred when the location of "Bieber" in the list is returned.
;; Non-presence is confirmed if "No Biebers here!" is returned.
;; Examples:
(check-expect (find-bieber-repeater (string->list "***Bieber"))
              4)
(check-expect (find-bieber-repeater (string->list "****Bieber"))
              5)

(define (find-bieber-repeater list-to-search)
  (cond [(empty? list-to-search)
         "No Biebers here!"]
        [(starts-with-bieber? list-to-search)
         1]
        [else (add1-if-number 
               (find-bieber-repeater (rest list-to-search)))]))

;; Tests
(check-expect (find-bieber-repeater (string->list "*****Bieber"))
              6)
(check-expect (find-bieber-repeater (string->list "Dispose of Bieber"))
              12)
(check-expect (find-bieber-repeater(string->list  "_ieber"))
              "No Biebers here!")
(check-expect (find-bieber-repeater (string->list "B_eber"))
              "No Biebers here!")
(check-expect (find-bieber-repeater (string->list "Bie_er"))
              "No Biebers here!")
(check-expect (find-bieber-repeater (string->list "Bieb_r"))
              "No Biebers here!")
(check-expect (find-bieber-repeater (string->list "Biebe_"))
              "No Biebers here!")
(check-expect (find-bieber-repeater (string->list "Biebe"))
              "No Biebers here!")
(check-expect (find-bieber-repeater (string->list "Biebe r"))
              "No Biebers here!")
(check-expect (find-bieber-repeater (string->list "Biebe Biebe Biebe r ffwefwefwefwef Bieber"))
              36)
;; *****************************************************************
;; d.)
;; worth-my-time?: (String or (listof Character)) -> Boolean
;; Purpose: determines if any character in the given target string
;; or character list more than three times in a row. Returns true 
;; if that is the case, and false otherwise.
;; Examples:
(check-expect (worth-my-time? "Twilight is greattt omfg!!1!!1!")
              false)
(check-expect (worth-my-time? "The TSE is down today by 25 points")
              true)

(define (worth-my-time? target)
  (cond [(string? target)
         (worth-my-time? (string->list target))]
        [(or (empty? target)
             (empty? (rest target))
             (empty? (rest (rest target))))
         true]
        [(and (equal? (first target)
                      (second target))
              (equal? (second target)
                      (third target))
              (equal? (first target)
                      (third target)))
         false]
        [else (worth-my-time? (rest target))]))

;; Tests
(check-expect (worth-my-time? "")
              true)
(check-expect (worth-my-time? "a")
              true)
(check-expect (worth-my-time? "aa")
              true)
(check-expect (worth-my-time? "aa ")
              true)
(check-expect (worth-my-time? "aa a")
              true)
(check-expect (worth-my-time? "a aa")
              true)
(check-expect (worth-my-time? "abaa")
              true)
(check-expect (worth-my-time? "aa aa aa aa aa aa aa_aa")
              true)
(check-expect (worth-my-time? "aAaAaAaAaAaAaAaaAAaA")
              true)
(check-expect (worth-my-time? "OoMFGgGgGgG")
              true)
(check-expect (worth-my-time? "This function kinda sSsSsSsuckSsSsSsS")
              true)
(check-expect (worth-my-time? "The TSE went up over 9000 points today.")
              false)
(check-expect (worth-my-time? "TWILlLlLYyyYY!! ! OmmMmmg!! !")
              true)
(check-expect (worth-my-time? "Challenge Accepted!")
              true)