;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname selection-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;; Assignment 5, Question 2
;; (sorting)
;; Daniel Holmes, 20418854
;; *******************************************************************
;; a.)
;; min-list: (listof Int) -> Int
;; Purpose: Finds and returns the minimum/smallest value in the given
;; list-of-ints.
;; Examples:
(check-expect (min-list (list 2 3 1))
              1)
(check-expect (min-list (list 9 2 4 3 1 39 4 9 0 1 39 4999 29))
              0)
(check-expect (min-list (list -10 1 2 0 39 -2 3000 4304309 23048))
              -10)
(define (min-list list-of-ints)
  (find-min-int list-of-ints (first list-of-ints)))

(define (find-min-int list-of-ints min-int)
  (cond [(empty? list-of-ints)
         min-int]
        [(>= min-int
            (first list-of-ints))
         (find-min-int (rest list-of-ints)
                       (first list-of-ints))]
        [(< min-int
            (first list-of-ints))
         (find-min-int (rest list-of-ints)
                       min-int)]))

;; ******************************************************************
;; b.)
;; remove-num: (listof Int) Int -> (listof Int)
;; Purpose: Finds and removes the first instance of the target-int from
;; the given-list. If the target-int is not found in the given-list,
;; then the given-list will be returned.
;; Examples:
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          1)
              (list 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          2)
              (list 1 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          3)
              (list 1 2 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))

(define (remove-num given-list target-int)
  (cond [(empty? given-list)
         empty]
        [(= (first given-list)
            target-int)
         (rest given-list)]
        [else (cons (first given-list)
                    (remove-num (rest given-list)
                                target-int))]))

;; Tests:
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          99)
              (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          4)
              (list 1 2 3 5 6 7 8 9 1 2 3 4 5 6 7 8 9))
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          5)
              (list 1 2 3 4 6 7 8 9 1 2 3 4 5 6 7 8 9))
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          6)
              (list 1 2 3 4 5 7 8 9 1 2 3 4 5 6 7 8 9))
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          -1)
              (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))
(check-expect (remove-num (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
                          0)
              (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))

;; ******************************************************************
;; c.)
;; selection-sort: (listof Int)  -> (listof Int)
;; Purpose: Sorts the given-list into non-descending order and then
;; returns that sorted list.
;; Examples:
(check-expect (selection-sort (list 6 5 4 3 2 1))
              (list 1 2 3 4 5 6))
(check-expect (selection-sort (list 1 3 2 5 6 4))
              (list 1 2 3 4 5 6))
(check-expect (selection-sort (list 6 5 4 2 1))
              (list 1 2 4 5 6))

(define (selection-sort given-list)
  (cond [(empty? given-list)
         empty]
        [else 
         (cons (min-list given-list) 
               (selection-sort (remove-num given-list (min-list given-list))))]))

;; Tests:
(check-expect (selection-sort (list 6 5 4 3 3 2 1))
              (list 1 2 3 3 4 5 6))
(check-expect (selection-sort (list 5 1 0 -1 -5))
              (list -5 -1 0 1 5))
(check-expect (selection-sort (list 0 0 0))
              (list 0 0 0))
(check-expect (selection-sort (list 1 0 0))
              (list 0 0 1))
(check-expect (selection-sort (list 1 1 0))
              (list 0 1 1))

















