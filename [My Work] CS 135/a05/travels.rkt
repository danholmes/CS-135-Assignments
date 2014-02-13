;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname travels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;; Assignment 5, Question 3
;; (travel lists and structs)
;; Daniel Holmes, 20418854
;; *******************************************************************
;; 3. a.) i.
;; CONSTANT DEFINITIONS
;; sample-triplist: a triplist made to be used in the examples
(define sample-triplist (list (list "Toronto" "sky dome" 4)
                              (list "Ottawa" "parliament" 3)
                              (list "London" "Western" 4)
                              (list "St. Catherines" "Beautiful girls" 3)
                              (list "Waterloo" "Great Profs and TAs" 5)))

;; A TripList is one of:
;; * empty
;; * (cons (list String String Nat) TripList)

;; add-city-list: String String Nat (listof list)
;; Purpose: Takes the components (city liked score) of a new list 
;; component of a TripList as well as a TripList and adds those 
;; components to the beginning of that TripList.
;; Examples:
(check-expect (add-city-list "Sparta!" "This is..." 5
                             sample-triplist)
              (cons (list "Sparta!" "This is..." 5)
                    (list (list "Toronto" "sky dome" 4)
                          (list "Ottawa" "parliament" 3)
                          (list "London" "Western" 4)
                          (list "St. Catherines" "Beautiful girls" 3)
                          (list "Waterloo" "Great Profs and TAs" 5))))
(check-expect (add-city-list "Tokyo" "Great culture" 5 sample-triplist)
              (cons (list "Tokyo" "Great culture" 5) sample-triplist))

(define (add-city-list city liked score triplist)
  (cons (list city liked score) triplist))
;; Tests:


;; 3. a.) ii.
;; things-I-liked: (listof TripList) -> (listof list)
;; Purpose: Takes a list-of-triplist and creates a new
;; list with the 'city' and 'liked' Strings associated with
;; all cities in the given list-of-triplist with a score
;; greater than 4.
;; Examples:
(define (things-I-liked list-of-triplist)
  (cond [(empty? list-of-triplist)
         empty]
        [(>= (third (first list-of-triplist))
             4)
         (cons (list (first (first list-of-triplist))
                     (second (first list-of-triplist)))
               (things-I-liked (rest list-of-triplist)))]))
;; Tests:

;; *******************************************************************
;; 3. b.) 
(define-struct trip (city liked score))
;; A Trip = (make-trip String String Nat)

;; i.
;; add-city-struct: String String Nat (listof Trip) -> (listof Trip)
;; Purpose: Takes the components of a Trip struct (a 'city' String,
;; a 'liked' String, and a 'score' Nat) as well as a list of Trip
;; structs (or empty list) and creates a new Trip then adds it to
;; aforementioned list of Trip's.
;; Examples:

(define (add-city-struct city liked score list-of-trip)
  (cons (make-trip city liked score) list-of-trip))
;; Tests:

;; ii.
;; redeeming-qualities: (listof Trip) -> (listof List)
;; Purpose: Takes a list-of-trip and creates a list with 
;; only the 'city' String and 'liked' String from all entries
;; in the given list-of-trip with a score equal-to-or-less-than
;; 2.
;; Examples:
(define (redeeming-qualities list-of-trip)
  (cond [(<= (trip-score (first list-of-trip))
             2)
         (cons (list (trip-city (first list-of-trip))
                     (trip-liked (first list-of-trip)))
               (redeeming-qualities (rest list-of-trip)))]))
                     
;; Tests:

;; *******************************************************************
;; 3. c.) 
;; travel-tips: String (listof (union TripList Trip)) -> (listof String)
;; Purpose: Takes a mixed-list of types TripList and Trip as well as a 
;; city (String). Creates a new list with all associated 'likes' to that city
;; for all entries in the mixed-list. (ie. If an entry is for that city, the
;; 'like' String will be added to the list to be outputted).
;; Examples:

(define (travel-tips city mixed-list)
  (cond [(empty? mixed-list)
         empty]
        [(and (trip? (first mixed-list))
              (equal? (trip-city (first mixed-list))
                      city))
         (cons (trip-liked (first mixed-list)) 
               (travel-tips city (rest mixed-list)))]
        [(equal? (first (first mixed-list))
                 city)
         (cons (first (first mixed-list))
               (travel-tips city (rest mixed-list)))]
        [else (travel-tips city (rest mixed-list))]))
;; Tests:

                     
                     
                     
                     
                     