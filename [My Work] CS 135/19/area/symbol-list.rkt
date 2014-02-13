;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname symbol-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;; Assignment 4, Question 3
;; (symbol list functions)
;; Daniel Holmes, 20418854
;; *******************************************************************
;; Constant Definitions:
;; sample-cities-list: A sample list of cities for testing purposes.
(define sample-cities-list (cons 'london 
                                 (cons 'tokyo 
                                       (cons 'bangkok 
                                             (cons 'shanghai 
                                                   empty)))))
;; a.)
;; next-city: (listof Symbol) Symbol -> Symbol
;; Purpose: This function takes a list-of-cities and the current-city,
;; the former being a list of symbols and the latter being a symbol.
;; The function either produces which city is after the current city 
;; in the list - ie. the city next to be visited - or it produces
;; 'none if the current city is the last city to be visited, or -lastly-
;; it produces 'not-in-route if the city currently visited is not in the list
;; of cities to be visited.
;; Examples:
(check-expect (next-city sample-cities-list
              'tokyo)
              'bangkok)
(check-expect (next-city sample-cities-list
                         'shanghai)
              'none)
(check-expect (next-city sample-cities-list 
                         'toronto)
              'not-in-route)

(define (next-city list-of-cities current-city)
  (cond [(empty? list-of-cities)
         'not-in-route]
        [(and (symbol=? current-city (first list-of-cities))
         (empty? (rest list-of-cities)))
         'none]
        [(symbol=? current-city (first list-of-cities))
         (first (rest list-of-cities))]
        [else (next-city (rest list-of-cities) current-city)]))

;; Tests
(check-expect (next-city sample-cities-list
                         'bangkok)
              'shanghai)
(check-expect (next-city sample-cities-list 
                         'tokyo)
              'bangkok)
(check-expect (next-city sample-cities-list 
                         'london)
              'tokyo)
(check-expect (next-city sample-cities-list
                         'london)
              'tokyo)
(check-expect (next-city sample-cities-list
                         'sparta)
              'not-in-route)
(check-expect (next-city empty 
                         'sparta) 
              'not-in-route)
        
;; b.)
;; count-cities-before: (listof Symbol) Symbol -> Nat
;; Purpose: This takes a list-of-cities (list of Symbols) and a target-city
;; (Symbol). It produces the number (Nat) of cities in the aforementioned list
;; before the target city. Produces zero if list is empty or target-city is not
;; present in the list of cities.
;; Examples:
(check-expect (count-cities-before sample-cities-list
                                   'bangkok)
              2)

(define (count-cities-before list-of-cities target-city)
  (cond [(or (not (city-in-list? list-of-cities 
                                 target-city))
             (empty? list-of-cities)
             (symbol=? target-city (first list-of-cities)))
         0]
        [else (+ 1 (count-cities-before (rest list-of-cities) target-city))]))

;; Tests
(check-expect (count-cities-before empty
                                   'bangkok)
              0)
(check-expect (count-cities-before sample-cities-list
                                   'london)
              0)
(check-expect (count-cities-before sample-cities-list
                                   'sparta)
              0)
(check-expect (count-cities-before sample-cities-list
                                   'tokyo)
              1)
(check-expect (count-cities-before sample-cities-list
                                   'shanghai)
              3)

;; city-in-list?: (listof Symbol) Symbol -> Boolean
;; Purpose: Determines whether or not the target-city (symbol) is in the
;; list-of-cities (list of symbols). Returns true if the function found
;; the city (symbol) in the list, or false if not.
;; Examples:
(check-expect (city-in-list? sample-cities-list
                             'bangkok)
              true)

(define (city-in-list? list-of-cities target-city)
  (cond [(empty? list-of-cities)
         false]
        [(symbol=? target-city
                   (first list-of-cities))
         true]
        [else (city-in-list? (rest list-of-cities) 
                             target-city)]))
;; Tests:
(check-expect (city-in-list? empty 
                             'bangkok)
              false)
(check-expect (city-in-list? sample-cities-list
                             'sparta)
              false)

;; c.)
;; last-city: (listof Symbol) -> Symbol
;; Purpose: Produces the last city in the given list-of-cities.
;; Produces 'empty-route if the list-of-cities is empty.
;; Examples:
(check-expect (last-city sample-cities-list) 
              'shanghai)
(check-expect (last-city empty) 
              'empty-route)

(define (last-city list-of-cities)
  (cond [(empty? list-of-cities)
         'empty-route]
        [(empty? (rest list-of-cities))
         (first list-of-cities)]
        [else (last-city (rest list-of-cities))]))

;; Tests:
(check-expect (last-city (cons 'new-york empty))
              'new-york)
(check-expect (last-city (cons 'new-york 
                               (cons 'toronto 
                                     empty)))
              'toronto)
(check-expect (last-city (cons 'new-york 
                               (cons 'toronto 
                                     (cons 'london 
                                           empty))))
              'london)
(check-expect (last-city (cons 'new-york 
                               (cons 'toronto 
                                     (cons 'london 
                                           (cons 'bangkok 
                                                 empty)))))
              'bangkok)
     
;; d.)
;; update-route: (listof Symbol) Symbol -> (listof Symbol)
;; Purpose: The function checks for the presence of the city-to-add
;; in the list-of-cities and either produces a new list with the 
;; city-to-add added to the end if it didn't find it in the list, or
;; the original list-of-cities if it did find it in the list.
;; Examples:
(check-expect (update-route sample-cities-list 'sparta)
              (cons 'london 
                    (cons 'tokyo 
                          (cons 'bangkok 
                                (cons 'shanghai 
                                      (cons 'sparta empty))))))
(check-expect (update-route sample-cities-list 'bangkok)
              sample-cities-list)
             
(define (update-route list-of-cities city-to-add)
  (cond [(empty? list-of-cities)
         (cons city-to-add empty)]
        [(symbol=? city-to-add (first list-of-cities))
         list-of-cities]
        [else (cons (first list-of-cities)
                    (update-route (rest list-of-cities) city-to-add))]))

;; Tests:
(check-expect (update-route empty 'sparta)
              (cons 'sparta empty))
(check-expect (update-route sample-cities-list 'london)
              sample-cities-list)
(check-expect (update-route sample-cities-list 'tokyo)
              sample-cities-list)
(check-expect (update-route sample-cities-list 'shanghai)
              sample-cities-list)