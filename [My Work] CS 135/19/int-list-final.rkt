;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;; Assignment 4, Question 2
;; (integer list functions)
;; Daniel Holmes, 20418854
;; *******************************************************************
;; a.) 
;; positive-total: (listof Int) -> Int
;; Purpose: Produces the sum of all the positive numbers in
;; the given list-of-int
;; Examples:
(check-expect (positive-total (cons -49 (cons -5 (cons -1 empty))))
              0)
(check-expect(positive-total (cons 10 (cons -30 (cons -60 empty))))
             10)

(define (positive-total list-of-int)
  (cond [(empty? list-of-int) 0]
        [(<= 0 (first list-of-int))
         (+ (first list-of-int)
            (positive-total (rest list-of-int)))]
        [(> 0 (first list-of-int))
         (positive-total (rest list-of-int))]))
;; Tests:
(check-expect(positive-total (cons -1 (cons -1 (cons -1 empty))))
             0)
(check-expect(positive-total (cons 0 (cons -1 (cons -1 empty))))
             0)
(check-expect(positive-total (cons 0 (cons 0 (cons -1 empty))))
             0)
(check-expect(positive-total (cons 0 (cons 0 (cons 0 empty))))
             0)
(check-expect(positive-total (cons 1 (cons 0 (cons 0 empty))))
             1)
(check-expect(positive-total (cons 1 (cons 1 (cons 0 empty))))
             2)
(check-expect(positive-total (cons 1 (cons 1 (cons 1 empty))))
             3)
(check-expect(positive-total (cons -1 (cons -1 (cons 1 empty))))
             1)
(check-expect(positive-total (cons -1 (cons 1 (cons 1 empty))))
             2)
(check-expect(positive-total (cons 1 (cons 1 (cons 1 empty))))
             3)


;; b.)
;; count-parity: (listof Int) Symbol -> Int
;; Purpose: If given 'even as a value for count-type, the function will 
;; produce the number of even numbers in the list. If count-type is 'odd,
;; the function will produce the number of odd numbers in the list.
;; Examples:
(check-expect (count-parity (cons 99 (cons -21 (cons 30 (cons 61 empty)))) 'even)
              1)
(check-expect (count-parity (cons 0 (cons -2 (cons 2 empty))) 'even)
              3)
(check-expect (count-parity empty 'odd) 
              0)

(define (count-parity list-of-int count-type)
  (cond [(empty? list-of-int)
         0]
        [(and (symbol=? count-type 'even)
              (even? (first list-of-int)))
         (+ 1 (count-parity (rest list-of-int) count-type))]
        [(and (symbol=? count-type 'odd)
              (odd? (first list-of-int)))
         (+ 1 (count-parity (rest list-of-int) count-type))]
        [else (+ 0 (count-parity (rest list-of-int) count-type))]))
;; Tests:
(check-expect (count-parity (cons 4 (cons 4 (cons 4 empty))) 'even)
              3)
(check-expect (count-parity (cons 4 (cons 4 (cons 4 empty))) 'odd)
              0)
(check-expect (count-parity (cons 3 (cons 4 (cons 4 empty))) 'even)
              2)
(check-expect (count-parity (cons 3 (cons 4 (cons 4 empty))) 'odd)
              1)
(check-expect (count-parity (cons 3 (cons 3 (cons 4 empty))) 'even)
              1)
(check-expect (count-parity (cons 3 (cons 3 (cons 4 empty))) 'odd)
              2)
(check-expect (count-parity (cons 3 (cons 3 (cons 3 empty))) 'even)
              0)
(check-expect (count-parity (cons 3 (cons 3 (cons 3 empty))) 'odd)
              3)
(check-expect (count-parity (cons 0 (cons 0 (cons 0 empty))) 'even)
              3)
(check-expect (count-parity (cons -4 (cons 4 (cons 0 empty))) 'even)
              3)
(check-expect (count-parity (cons -3 (cons 3 (cons 2 empty))) 'even)
              1)
(check-expect (count-parity (cons -3 (cons 3 (cons 2 empty))) 'odd)
              2)
(check-expect (count-parity (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 empty))))))
                            'even)
              6)
(define c (cons 2 empty))
(check-expect (count-parity c 'even) 1)
(check-expect (count-parity c 'odd) 0)

;; c.)
;; ascending-or-descending?: (listof Int) Symbol -> Boolean
;; Purpose: Determines if the given list of Ints (list-of-int) is ordered
;; according to the specified sorting method (list-type). The two possible
;; scenarios are an 'ascending or 'descending sorting method. The function
;; produces true if the list is ordered according to the sorting method,
;; false if it is not, and true if the list has one element or is empty.
;; Examples:
(check-expect (ascending-or-descending? (cons 1 (cons 2 (cons 3 empty))) 'ascending)
              true)
(check-expect (ascending-or-descending? (cons 3 (cons 2 (cons 1 empty))) 'descending)
              true)
(check-expect (ascending-or-descending? (cons -1 (cons 8 (cons -9 empty))) 'descending)
              false)

(define (ascending-or-descending? list-of-int list-type)
  (cond [(or (empty? list-of-int)
             (empty? (rest list-of-int))) 
         true]
        [(and (symbol=? list-type 'ascending)
              (< (first list-of-int) (first (rest list-of-int))))
         (and true (ascending-or-descending? (rest list-of-int) list-type))]
        [(and (symbol=? list-type 'descending)
              (> (first list-of-int) (first (rest list-of-int))))
         (and true (ascending-or-descending? (rest list-of-int) list-type))]
        [else false]))
;; Tests:
(check-expect (ascending-or-descending? (cons 1 (cons 2 (cons 3 empty))) 
                                        'ascending)
              true)
(check-expect (ascending-or-descending? (cons 1 (cons 2 (cons 3 empty))) 
                                        'descending)
              false)
(check-expect (ascending-or-descending? (cons 3 (cons 2 (cons 1 empty))) 
                                        'descending)
              true)
(check-expect (ascending-or-descending? (cons 3 (cons 2 (cons 1 empty))) 
                                        'ascending)
              false)
(check-expect (ascending-or-descending? (cons 0 (cons 0 (cons 0 empty))) 
                                        'descending)
              false)
(check-expect (ascending-or-descending? (cons 0 (cons 0 (cons 0 empty))) 
                                        'ascending)
              false)
(check-expect (ascending-or-descending? empty 
                                        'descending)
              true)
(check-expect (ascending-or-descending? empty 
                                        'ascending)
              true)
(check-expect (ascending-or-descending? (cons 1 empty) 
                                        'descending)
              true)
(check-expect (ascending-or-descending? (cons 1 empty) 
                                        'ascending)
              true)





 
 
 
 
 
 