;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname skyscrapers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                          CS 135 FALL 2012
;;                            Assignment 10
;;                            Daniel Holmes
;;                              20418854
;;                               skyscrapers
;; *******************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data definitions
;;
;; A Cell is a (union Nat[>0] '?)
;;
;; A Grid is a (ne-listof (ne-listof Cell))
;; A ClueSet is a (ne-listof Nat[>0])

(define-struct state (grid left right top bottom))
;; A State is a (make-state Grid ClueSet ClueSet ClueSet ClueSet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visibility-number: (listof Cell) -> Nat
;; Compute the visibility number of a list of Cells (loc).
;; The numeric entries should not have any duplicates.
;;
;; Examples:
(check-expect (visibility-number empty) 0)
(check-expect (visibility-number '(4 ? 6 5 ? 9)) 3)

(define (visibility-number loc)
  (local [ ; START local
          (define (get-listof-visible loc lov)
            (local [(define floc (filter (lambda (x) 
                                           (not (equal? '? x)))
                                         loc))]
              (cond [(empty? floc)
                     lov]
                    [(empty? lov)
                     (get-listof-visible (rest floc) (list (first floc)))]
                    [(> (first floc) (first lov))
                     (get-listof-visible (rest floc) (cons (first floc) lov))]
                    [else
                     (get-listof-visible (rest floc) lov)])))
          ] ; END local
          (length (get-listof-visible loc empty))))

;; Tests:
;; Assorted:
(check-expect (visibility-number '(? 1 2 3 4 5 6)) 6)
;; Movement of (max num) throughout
(check-expect (visibility-number '(1 2 3 4 5 6 7)) 7)
(check-expect (visibility-number '(1 2 3 4 5 7 6)) 6)
(check-expect (visibility-number '(1 2 3 4 7 5 6)) 5)
(check-expect (visibility-number '(1 2 3 7 4 5 6)) 4)
(check-expect (visibility-number '(1 2 7 3 4 5 6)) 3)
(check-expect (visibility-number '(1 7 2 3 4 5 6)) 2)
(check-expect (visibility-number '(7 1 2 3 4 5 6)) 1)
;; Swapping two (num) throughout
(check-expect (visibility-number '(3 2 1 4 5 6 7)) 5)
(check-expect (visibility-number '(1 4 3 2 5 6 7)) 5)
(check-expect (visibility-number '(1 2 5 4 3 6 7)) 5)
(check-expect (visibility-number '(1 2 3 6 5 4 7)) 5)
(check-expect (visibility-number '(1 2 3 4 7 6 5)) 5)
;; Singular replacement of (num) by ? throughout
(check-expect (visibility-number '(? 2 3 4 5 6 7)) 6)
(check-expect (visibility-number '(1 ? 3 4 5 6 7)) 6)
(check-expect (visibility-number '(1 2 ? 4 5 6 7)) 6)
(check-expect (visibility-number '(1 2 3 ? 5 6 7)) 6)
(check-expect (visibility-number '(1 2 3 4 ? 6 7)) 6)
(check-expect (visibility-number '(1 2 3 4 5 ? 7)) 6)
(check-expect (visibility-number '(1 2 3 4 5 6 ?)) 6)
;; Multiple replacement of (num) by ? throughout
(check-expect (visibility-number '(? 2 3 4 5 6 7)) 6)
(check-expect (visibility-number '(? ? 3 4 5 6 7)) 5)
(check-expect (visibility-number '(? ? ? 4 5 6 7)) 4)
(check-expect (visibility-number '(? ? ? ? 5 6 7)) 3)
(check-expect (visibility-number '(? ? ? ? ? 6 7)) 2)
(check-expect (visibility-number '(? ? ? ? ? ? 7)) 1)
(check-expect (visibility-number '(? ? ? ? ? ? ?)) 0)
(check-expect (visibility-number '(1 2 3 4 5 6 ?)) 6)
(check-expect (visibility-number '(1 2 3 4 5 ? ?)) 5)
(check-expect (visibility-number '(1 2 3 4 ? ? ?)) 4)
(check-expect (visibility-number '(1 2 3 ? ? ? ?)) 3)
(check-expect (visibility-number '(1 2 ? ? ? ? ?)) 2)
(check-expect (visibility-number '(1 ? ? ? ? ? ?)) 1)
;; Reverse of above
(check-expect (visibility-number (reverse '(? 2 3 4 5 6 7))) 1)
(check-expect (visibility-number (reverse '(? ? 3 4 5 6 7))) 1)
(check-expect (visibility-number (reverse '(? ? ? 4 5 6 7))) 1)
(check-expect (visibility-number (reverse '(? ? ? ? 5 6 7))) 1)
(check-expect (visibility-number (reverse '(? ? ? ? ? 6 7))) 1)
(check-expect (visibility-number (reverse '(? ? ? ? ? ? 7))) 1)
(check-expect (visibility-number (reverse '(? ? ? ? ? ? ?))) 0)
(check-expect (visibility-number (reverse '(1 2 3 4 5 6 ?))) 1)
(check-expect (visibility-number (reverse '(1 2 3 4 5 ? ?))) 1)
(check-expect (visibility-number (reverse '(1 2 3 4 ? ? ?))) 1)
(check-expect (visibility-number (reverse '(1 2 3 ? ? ? ?))) 1)
(check-expect (visibility-number (reverse '(1 2 ? ? ? ? ?))) 1)
(check-expect (visibility-number (reverse '(1 ? ? ? ? ? ?))) 1)
;; Addition of ? throughout
(check-expect (visibility-number '(? 1 2 3 4 5 6 7)) 7)
(check-expect (visibility-number '(1 ? 2 3 4 5 6 7)) 7)
(check-expect (visibility-number '(1 2 ? 3 4 5 6 7)) 7)
(check-expect (visibility-number '(1 2 3 ? 4 5 6 7)) 7)
(check-expect (visibility-number '(1 2 3 4 ? 5 6 7)) 7)
(check-expect (visibility-number '(1 2 3 4 5 ? 6 7)) 7)
(check-expect (visibility-number '(1 2 3 4 5 6 ? 7)) 7)
;; Singular replacement of (num) by ? throughout WITH
;; a (max num) moving throughout
(check-expect (visibility-number '(? 1 2 3 4 5 6 7)) 7)
(check-expect (visibility-number '(1 ? 2 3 4 5 7 6)) 6)
(check-expect (visibility-number '(1 2 ? 3 4 7 5 6)) 5)
(check-expect (visibility-number '(1 2 3 ? 7 4 5 6)) 4)
(check-expect (visibility-number '(1 2 7 3 ? 4 5 6)) 3)
(check-expect (visibility-number '(1 7 2 3 4 ? 5 6)) 2)
(check-expect (visibility-number '(7 1 2 3 4 5 ? 6)) 1) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; feasible-visibility? : (listof Cell) Nat -> Boolean
;; Given a partially filled in row lov and a natural number vis 
;; that counts the number of towers that are supposed to be
;; visible, is it possible to fill in the blanks in lov so that
;; the desired visibility number is attained?  Assume that the
;; numeric entries in lov are in a contiguous block at the start
;; or end of the list.

;; Examples:
(check-expect (feasible-visibility? '(3 ? ? ?) 2) true)
(check-expect (feasible-visibility? '(2 1 ? ?) 4) false)

(define (feasible-visibility? lov vis)
  (local [; START local
          (define flov (filter (lambda (x)
                                 (not (equal? x '?)))
                               lov))
          (define lo-1-to-n 
            (build-list (length lov) add1))
          (define lo-nonpresent
            (filter (lambda (x)
                      (not (member? x lov))) lo-1-to-n))
          (define rev-lo-nonpresent
            (reverse lo-nonpresent))
          (define (insert-into-lov ins-list)
            (cond [(equal? '? (first lov))
                   (append ins-list flov)]
                  [else
                   (append flov ins-list)]))
          (define vis-min-list (insert-into-lov rev-lo-nonpresent))
          (define vis-max-list (insert-into-lov lo-nonpresent))
          (define vis-min (visibility-number vis-min-list))
          (define vis-max (visibility-number vis-max-list))
          (define vis-within-bounds (or (and (<= vis-min vis)
                                             (<= vis vis-max))
                                        (empty? flov)))
          ]; END local
    vis-within-bounds))

;; Tests
;; All ?
(check-expect (feasible-visibility? '(? ? ? ?) 1) true)
(check-expect (feasible-visibility? '(? ? ? ?) 2) true)
(check-expect (feasible-visibility? '(? ? ? ?) 3) true)
(check-expect (feasible-visibility? '(? ? ? ?) 4) true)
;; First 4, rest Assorted
;(check-expect (feasible-visibility? '(4 ) 4) false)
(check-expect (feasible-visibility? '(4 ? ? ?) 1) true)
(check-expect (feasible-visibility? '(4 ? ? ?) 2) false)
(check-expect (feasible-visibility? '(4 1 ? ?) 1) true)
(check-expect (feasible-visibility? '(4 2 ? ?) 1) true)
(check-expect (feasible-visibility? '(4 3 ? ?) 1) true)
(check-expect (feasible-visibility? '(4 1 ? ?) 2) false)
(check-expect (feasible-visibility? '(4 2 ? ?) 2) false)
(check-expect (feasible-visibility? '(4 3 ? ?) 2) false)
(check-expect (feasible-visibility? '(4 1 ? ?) 3) false)
(check-expect (feasible-visibility? '(4 2 ? ?) 3) false)
(check-expect (feasible-visibility? '(4 3 ? ?) 3) false)
(check-expect (feasible-visibility? '(4 1 ? ?) 4) false)
(check-expect (feasible-visibility? '(4 2 ? ?) 4) false)
(check-expect (feasible-visibility? '(4 3 ? ?) 4) false)
(check-expect (feasible-visibility? '(4 3 2 1) 4) false)
(check-expect (feasible-visibility? '(4 3 2 1) 2) false)
(check-expect (feasible-visibility? '(4 3 2 1) 1) true)
(check-expect (feasible-visibility? '(4 2 3 1) 4) false)
(check-expect (feasible-visibility? '(4 2 3 1) 3) false)
(check-expect (feasible-visibility? '(4 2 3 1) 2) false)
(check-expect (feasible-visibility? '(4 2 3 1) 1) true)
(check-expect (feasible-visibility? '(4 1 2 3) 4) false)
(check-expect (feasible-visibility? '(4 1 2 3) 3) false)
(check-expect (feasible-visibility? '(4 1 2 3) 2) false)
(check-expect (feasible-visibility? '(4 1 2 3) 1) true)
;; First 3, rest Assorted
(check-expect (feasible-visibility? '(3 ? ? ?) 1) false)
(check-expect (feasible-visibility? '(3 ? ? ?) 2) true)
(check-expect (feasible-visibility? '(3 ? ? ?) 3) false)
(check-expect (feasible-visibility? '(3 1 ? ?) 1) false)
(check-expect (feasible-visibility? '(3 1 ? ?) 2) true)
(check-expect (feasible-visibility? '(3 1 ? ?) 3) false)
(check-expect (feasible-visibility? '(3 2 ? ?) 1) false)
(check-expect (feasible-visibility? '(3 2 ? ?) 2) true)
(check-expect (feasible-visibility? '(3 2 ? ?) 3) false)
(check-expect (feasible-visibility? '(3 4 ? ?) 1) false)
(check-expect (feasible-visibility? '(3 4 ? ?) 2) true)
(check-expect (feasible-visibility? '(3 4 ? ?) 3) false)
(check-expect (feasible-visibility? '(3 1 2 ?) 1) false)
(check-expect (feasible-visibility? '(3 1 2 ?) 2) true)
(check-expect (feasible-visibility? '(3 1 2 ?) 3) false)
(check-expect (feasible-visibility? '(3 2 1 ?) 1) false)
(check-expect (feasible-visibility? '(3 2 1 ?) 2) true)
(check-expect (feasible-visibility? '(3 2 1 ?) 3) false)
(check-expect (feasible-visibility? '(3 4 1 ?) 1) false)
(check-expect (feasible-visibility? '(3 4 1 ?) 2) true)
(check-expect (feasible-visibility? '(3 4 1 ?) 3) false)
(check-expect (feasible-visibility? '(3 4 2 ?) 1) false)
(check-expect (feasible-visibility? '(3 4 2 ?) 2) true)
(check-expect (feasible-visibility? '(3 4 2 ?) 3) false)
(check-expect (feasible-visibility? '(3 1 4 ?) 1) false)
(check-expect (feasible-visibility? '(3 1 4 ?) 2) true)
(check-expect (feasible-visibility? '(3 1 4 ?) 3) false)
(check-expect (feasible-visibility? '(3 1 2 4) 1) false)
(check-expect (feasible-visibility? '(3 1 2 4) 2) true)
(check-expect (feasible-visibility? '(3 1 2 4) 3) false)
(check-expect (feasible-visibility? '(3 2 1 4) 1) false)
(check-expect (feasible-visibility? '(3 2 1 4) 2) true)
(check-expect (feasible-visibility? '(3 2 1 4) 3) false)
(check-expect (feasible-visibility? '(3 4 1 2) 1) false)
(check-expect (feasible-visibility? '(3 4 1 2) 2) true)
(check-expect (feasible-visibility? '(3 4 1 2) 3) false)
(check-expect (feasible-visibility? '(3 4 2 1) 1) false)
(check-expect (feasible-visibility? '(3 4 2 1) 2) true)
(check-expect (feasible-visibility? '(3 4 2 1) 3) false)
(check-expect (feasible-visibility? '(3 1 4 2) 1) false)
(check-expect (feasible-visibility? '(3 1 4 2) 2) true)
(check-expect (feasible-visibility? '(3 1 4 2) 3) false)
;; First 2, rest Assorted
(check-expect (feasible-visibility? '(2 ? ? ?) 1) false)
(check-expect (feasible-visibility? '(2 ? ? ?) 2) true)
(check-expect (feasible-visibility? '(2 ? ? ?) 3) true)
(check-expect (feasible-visibility? '(2 ? ? ?) 4) false)
(check-expect (feasible-visibility? '(2 1 ? ?) 1) false)
(check-expect (feasible-visibility? '(2 1 ? ?) 2) true)
(check-expect (feasible-visibility? '(2 1 ? ?) 3) true)
(check-expect (feasible-visibility? '(2 1 ? ?) 4) false)
(check-expect (feasible-visibility? '(2 3 ? ?) 1) false)
(check-expect (feasible-visibility? '(2 3 ? ?) 2) false)
(check-expect (feasible-visibility? '(2 3 ? ?) 3) true)
(check-expect (feasible-visibility? '(2 3 ? ?) 4) false)
(check-expect (feasible-visibility? '(2 4 ? ?) 1) false)
(check-expect (feasible-visibility? '(2 4 ? ?) 2) true)
(check-expect (feasible-visibility? '(2 4 ? ?) 3) false)
(check-expect (feasible-visibility? '(2 4 ? ?) 4) false)
(check-expect (feasible-visibility? '(2 1 3 ?) 3) true)
(check-expect (feasible-visibility? '(2 1 3 ?) 2) false)
(check-expect (feasible-visibility? '(2 3 1 ?) 2) false)
(check-expect (feasible-visibility? '(2 3 1 ?) 3) true)
(check-expect (feasible-visibility? '(2 4 1 ?) 2) true)
(check-expect (feasible-visibility? '(2 4 1 ?) 3) false)
(check-expect (feasible-visibility? '(2 1 4 ?) 2) true)
(check-expect (feasible-visibility? '(2 1 4 ?) 3) false)
(check-expect (feasible-visibility? '(2 1 3 4) 3) true)
(check-expect (feasible-visibility? '(2 1 3 4) 2) false)
(check-expect (feasible-visibility? '(2 3 1 4) 3) true)
(check-expect (feasible-visibility? '(2 3 1 4) 2) false)
(check-expect (feasible-visibility? '(2 4 1 3) 2) true)
(check-expect (feasible-visibility? '(2 4 1 3) 3) false)
(check-expect (feasible-visibility? '(2 1 4 3) 2) true)
(check-expect (feasible-visibility? '(2 1 4 3) 3) false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; valid-state? : State -> Boolean
;; Determine whether the passed-in State a-state can still be legally
;; continued to produce a solution for the entire puzzle.  Rule out
;; any State in which there are duplicate entries in one row or column
;; and any State in which a row or column violates its visibility
;; constraints.

;; Examples:
(check-expect 
 (valid-state? 
  (make-state '((? ? 3) (? ? ?) (? ? ?)) 
              '(1 2 1) '(2 1 1) '(1 1 1) '(2 2 2))) false)
(check-expect
 (valid-state?
  (make-state '((? ? 3) (? ? ?) (? ? ?))
              '(3 1 1) '(1 2 2) '(1 1 1) '(2 2 2))) true)

(define (valid-state? a-state)
  (local [(define n (length (state-grid a-state)))
          (define (grid-get row col)
            (list-ref (list-ref (state-grid a-state) row) col))
          (define grid-transpose
            (map (lambda (x)
                   (map (lambda (y)
                          (grid-get y x))
                        (build-list n +))) 
                   (build-list n +)))
          ;; --- DUPLICATES PRESENT?
          (define (duplicates? matrix)
              (cond [(empty? matrix)
                     false]
                    [else
                     (local [(define cur-row (first matrix))
                             (define f-cur-row 
                               (filter (lambda (x)
                                         (not (equal? x '?)))
                                       cur-row))
                             (define fs-cur-row
                               (quicksort f-cur-row <=))
                             (define dup-pres
                               (cond [(or (empty? fs-cur-row) 
                                          (empty? (rest fs-cur-row)))
                                      false]
                                     [else
                                      (ormap (lambda(x) x)
                                             (map = 
                                                  fs-cur-row 
                                                  (append (rest fs-cur-row) 
                                                          (list (first fs-cur-row)))))]))]
                       (cond
                         [dup-pres
                          dup-pres]
                         [else
                          (duplicates? (rest matrix))]))]))
          (define no-duplicates-present ; result of left/right/bottom/top duplicate check
            (not (or (duplicates? (state-grid a-state))
                     (duplicates? grid-transpose))))
          ;; --- VISIBILITY FEASIBILITY                          
          (define left-vis-feasible ; left
            (andmap (lambda (x) x) 
                    (map feasible-visibility? 
                         (state-grid a-state) 
                         (state-left a-state))))
          (define right-vis-feasible ; right
            (andmap (lambda (x) x) 
                    (map feasible-visibility? 
                         (map reverse (state-grid a-state))
                         (state-right a-state))))
          (define top-vis-feasible ; top
            (andmap (lambda (x) x) 
                    (map feasible-visibility? 
                         grid-transpose 
                         (state-top a-state))))
          (define bottom-vis-feasible ; bottom
            (andmap (lambda (x) x) 
                    (map feasible-visibility? 
                         (map reverse grid-transpose)
                         (state-bottom a-state))))
          (define all-vis-feasible ; all
            (and left-vis-feasible
                 right-vis-feasible
                 top-vis-feasible
                 bottom-vis-feasible))
          ] 
    (and all-vis-feasible no-duplicates-present)
))

;; Tests:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complete-state? : State -> Boolean
;; Determine whether the passed-in State a-state represents a solved
;; puzzle, i.e., one in which the grid has no unknowns left it in
;; (we assume that the puzzle is legal if this is the case).

;; Examples:
(check-expect
 (complete-state?
  (make-state '((? 1 2) (3 2 1) (1 2 3))
              '(1 1 1) '(1 1 1) '(1 1 1) '(1 1 1))) false)
(check-expect
 (complete-state?
  (make-state '((1 2) (2 1))
              '(2 1) '(1 2) '(2 1) '(1 2))) true)

(define (complete-state? a-state)
  (andmap (lambda(x) (andmap (lambda(y) (not (equal? y '?))) x)) (state-grid a-state)))

;; Tests:
(define left '(1 3 2 2))
(define right'(3 2 2 1))
(define top '(1 3 2 3))
(define bottom'(2 2 3 1))
(check-expect (complete-state? (make-state
                                (make-list 4 (make-list 4 '?))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              true)
(check-expect (complete-state? (make-state
                                '((? 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 ? 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 ? 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 ?)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (? 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 ? 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 ? 1)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 ?)
                                  (1 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (? 1 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 ? 1 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 ? 1)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 ?)
                                  (1 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (? 1 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 ? 1 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 ? 1))
                                left right top bottom))
              false)
(check-expect (complete-state? (make-state
                                '((1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 1)
                                  (1 1 1 ?))
                                left right top bottom))
              false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neighbours : State -> (listof State)
;; Given a current State a-state, produce a list of all legal successor
;; states, according to the constraints laid out in the assignment (that
;; is, fill in the first non-numeric entry in the grid with each of the
;; legal numbers that can live in that spot.

;; Examples:
(check-expect
 (neighbours
  (make-state '((?)) '(1) '(1) '(1) '(1)))
 (list (make-state '((1)) '(1) '(1) '(1) '(1))))

(check-expect
 (neighbours
  (make-state
   (make-list 4 (make-list 4 '?))
   '(1 3 2 2) '(3 2 2 1) '(1 3 2 3) '(2 2 3 1)))
 (list (make-state '((4 ? ? ?) (? ? ? ?) (? ? ? ?) (? ? ? ?))
                   '(1 3 2 2) '(3 2 2 1) '(1 3 2 3) '(2 2 3 1))))

(define (neighbours a-state)
  (local [; START local
          (define n (length (state-grid a-state)))
          (define (insert-into-matrix item matrix)
            (cond [(empty? matrix)
                   empty]
                  [(member? '? (first matrix))
                   (local [(define (insert-into-list x list)
                             (cond [(empty? list)
                                    (error "Something went wrong")]
                                   [(equal? '? (first list))
                                    (cons item (rest list))]
                                   [else
                                    (cons (first list) 
                                          (insert-into-list x 
                                                            (rest list)))]))]
                     (cons (insert-into-list item (first matrix)) (rest matrix)))]
                  [else
                   (cons (first matrix) (insert-into-matrix item (rest matrix)))]))
          (define all-neighbour-states 
            (map make-state 
                 (map insert-into-matrix 
                      (build-list n add1) 
                      (make-list n (state-grid a-state)))
                 (make-list n (state-left a-state))
                 (make-list n (state-right a-state))
                 (make-list n (state-top a-state))
                 (make-list n (state-bottom a-state))))
          (define f-all-neighbour-states
            (filter valid-state? all-neighbour-states))
          ]; END local
    f-all-neighbour-states))
  
;; Tests:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search: (X -> Boolean) (X -> (listof X)) X -> (union X false)
;; Solve a puzzle, given a current State.  Use depth-first search
;; with backtracking, assuming that the search space is acyclic. 
;; The parameters are:
;;  * at-end? -- a predicate that determines whether the given
;;      state is an end state of the search (i.e., a solution).
;;  * neighbours -- a function that maps a current state to a list
;;      of neighbouring states.
;;  * a-state -- the initial state.
;;
;; DO NOT MODIFY THIS FUNCTION.

(define (search at-end? neighbours a-state)
  (local
    [;; find-route: X -> (union X false)
     ;; Search outward from this configuration to see if there's a path
     ;; to a solution.
     (define (find-route a-state)
       (cond
         [(at-end? a-state) a-state]
         [else (find-route/list (neighbours a-state))]))
     
     ;; find-route/list: (listof X) -> (union X false)
     ;; Search outward from every configuration in the passed-in list of
     ;; configurations.  If any one of them leads to a solution, stop and
     ;; produce that solution.  Produce false if you run out of options.
     (define (find-route/list lostate)
       (cond
         [(empty? lostate) false]
         [else
          (local
            [(define cur (find-route (first lostate)))]
            (cond
              [(not (boolean? cur)) cur]
              [else (find-route/list (rest lostate))]))]))]
    (find-route a-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sample puzzles for testing.  You can easily create more yourself by 
;; transcribing them from examples generated using the software mentioned
;; in the assignment.
;;

;; The puzzle that appears at the start of the assignment handout.
(define assignment-puzzle
  (make-state
   (make-list 4 (make-list 4 '?))
   '(1 3 2 2)
   '(3 2 2 1)
   '(1 3 2 3)
   '(2 2 3 1)))
(define assignment-puzzle-solution
  (make-state
   '((4 1 3 2) (2 3 4 1) (1 4 2 3) (3 2 1 4))
   (state-left assignment-puzzle)
   (state-right assignment-puzzle)
   (state-top assignment-puzzle)
   (state-bottom assignment-puzzle)))

(define puzzle-1 
  (make-state
   (make-list 4 (make-list 4 '?))
   '(1 2 2 3)
   '(3 3 1 2)
   '(1 2 3 2)
   '(3 2 1 2)))
(define puzzle-1-solution 
  (make-state
   '((4 3 1 2) (2 4 3 1) (3 1 2 4) (1 2 4 3))
   (state-left puzzle-1)
   (state-right puzzle-1)
   (state-top puzzle-1)
   (state-bottom puzzle-1)))

(define puzzle-2 
  (make-state
   (make-list 4 (make-list 4 '?))
   '(1 2 3 2)
   '(3 3 2 1)
   '(1 2 3 3)
   '(2 3 2 1)))
(define puzzle-2-solution 
  (make-state
   '((4 3 1 2) 
     (2 4 3 1) 
     (1 2 4 3) 
     (3 1 2 4))
   (state-left puzzle-2)
   (state-right puzzle-2)
   (state-top puzzle-2)
   (state-bottom puzzle-2)))

(define puzzle-3 
  (make-state
   (make-list 5 (make-list 5 '?))
   '(2 3 2 1 3)
   '(2 1 2 3 2)
   '(4 1 2 3 2) 
   '(2 5 2 1 2)))
(define puzzle-3-solution 
  (make-state
   '((1 5 2 3 4) 
     (3 4 1 2 5)
     (4 3 5 1 2)
     (5 2 3 4 1)
     (2 1 4 5 3))
   (state-left puzzle-3)
   (state-right puzzle-3)
   (state-top puzzle-3)
   (state-bottom puzzle-3)))

(define puzzle-4 
  (make-state
   (make-list 5 (make-list 5 '?))
   '(2 2 1 2 4)
   '(3 2 5 3 1)
   '(3 1 3 2 3) 
   '(3 3 2 3 1)))
(define puzzle-4-solution 
  (make-state
   '((1 5 2 4 3) 
     (3 2 1 5 4)
     (5 4 3 2 1)
     (4 1 5 3 2)
     (2 3 4 1 5))
   (state-left puzzle-4)
   (state-right puzzle-4)
   (state-top puzzle-4)
   (state-bottom puzzle-4)))

(define puzzle-5
  (make-state
   (make-list 4 (make-list 4 '?))
   '(3 2 1 2)
   '(1 2 3 2)
   '(2 2 4 1) 
   '(2 3 1 3)))
(define puzzle-5-solution 
  (make-state
   '((2 3 1 4)
     (1 4 2 3)
     (4 2 3 1)
     (3 1 4 2))
   (state-left puzzle-5)
   (state-right puzzle-5)
   (state-top puzzle-5)
   (state-bottom puzzle-5)))

(define puzzle-6
  (make-state
   (make-list 4 (make-list 4 '?))
   '(3 4 1 2)
   '(2 1 2 2)
   '(2 2 1 2) 
   '(2 1 4 3)))
(define puzzle-6-solution 
  (make-state
   '((2 3 4 1)
     (1 2 3 4)
     (4 1 2 3)
     (3 4 1 2))
   (state-left puzzle-6)
   (state-right puzzle-6)
   (state-top puzzle-6)
   (state-bottom puzzle-6)))

(define puzzle-7
  (make-state
   (make-list 4 (make-list 4 '?))
   '(1 2 3 2)
   '(3 2 1 2)
   '(1 2 4 3) 
   '(2 2 1 2)))
(define puzzle-7-solution 
  (make-state
   '((4 3 1 2)
     (1 4 2 3)
     (2 1 3 4)
     (3 2 4 1))
   (state-left puzzle-7)
   (state-right puzzle-7)
   (state-top puzzle-7)
   (state-bottom puzzle-7)))

(define puzzle-8
  (make-state
   (make-list 4 (make-list 4 '?))
   '(2 2 1 3)
   '(3 1 3 2)
   '(3 1 2 2) 
   '(2 3 1 2)))
(define puzzle-8-solution 
  (make-state
   '((2 4 3 1)
     (3 1 2 4)
     (4 3 1 2)
     (1 2 4 3))
   (state-left puzzle-8)
   (state-right puzzle-8)
   (state-top puzzle-8)
   (state-bottom puzzle-8)))

(define puzzle-9
  (make-state
   (make-list 4 (make-list 4 '?))
   '(2 4 3 1)
   '(2 1 2 2)
   '(2 1 3 2) 
   '(1 3 2 2)))
(define puzzle-9-solution 
  (make-state
   '((3 4 1 2)
     (1 2 3 4)
     (2 3 4 1)
     (4 1 2 3))
   (state-left puzzle-9)
   (state-right puzzle-9)
   (state-top puzzle-9)
   (state-bottom puzzle-9)))


(define bonus-puzzle
  (make-state
   '((? ? ? ? ? ?)
     (? ? ? ? ? ?)
     (? ? ? ? ? ?)
     (? ? ? ? ? ?)
     (? ? ? ? ? ?)
     (? ? ? 2 ? ?))
   '(2 4 ? ? ? ?)
   '(4 ? 4 ? 2 2)
   '(? 2 ? ? ? 4)
   '(? ? ? ? 4 ?)))

;; Testing example:
(check-expect (search complete-state? neighbours assignment-puzzle) assignment-puzzle-solution)
(check-expect (search complete-state? neighbours puzzle-1) puzzle-1-solution)
(check-expect (search complete-state? neighbours puzzle-2) puzzle-2-solution)
(check-expect (search complete-state? neighbours puzzle-3) puzzle-3-solution)
(check-expect (search complete-state? neighbours puzzle-4) puzzle-4-solution)
(check-expect (search complete-state? neighbours puzzle-5) puzzle-5-solution)
(check-expect (search complete-state? neighbours puzzle-6) puzzle-6-solution)
(check-expect (search complete-state? neighbours puzzle-7) puzzle-7-solution)
(check-expect (search complete-state? neighbours puzzle-8) puzzle-8-solution)
(check-expect (search complete-state? neighbours puzzle-9) puzzle-9-solution)
