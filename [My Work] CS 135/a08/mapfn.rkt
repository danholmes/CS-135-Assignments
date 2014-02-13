;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mapfn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                           Assignment 8 Q3
;;                            Daniel Holmes
;;                              20418854
;;                                BST
;; *******************************************************************
;; *** (a.) 
;; *******************************************************************
;; --
;; mapfn: (listof (Num Num -> Any)) (listof Num) -> (listof Any)
;; --
;; Purpose: Consumes a list of binary functions, fn-list, and applies
;; each function in the list to the given list of two numbers, num-pair.
;; --
;; Examples:
(check-expect (mapfn (list + - * / list remainder) '(1 1))
              (list 2 0 1 1 (list 1 1) 0))
(check-expect (mapfn (list + - * / list remainder) '(-1 -1))
              (list -2 0 1 1 (list -1 -1) 0))
(check-expect (mapfn (list + - * / list remainder)  '(-1 1))
              (list 0 -2 -1 -1 (list -1 1) 0))
;; ---------------------------------
;; Definition:
(define (mapfn fn-list num-pair)
  (cond [(empty? fn-list)
         empty]
        [(and (equal? (first fn-list) /)
              (equal? (second num-pair) 0))
         (error "Error: Cannot divide by zero.")]
        [else
         (cons ((first fn-list) (first num-pair) (second num-pair))
               (mapfn (rest fn-list) num-pair))]))
;; ---------------------------------
;; Tests:
(check-expect (mapfn (list < > expt)  '(2 3))
              (list true false 8))



;; *******************************************************************
;; *** (b.) 
;; *******************************************************************
;; --
;; is-in-order?: (ne-listof (list (Any -> Boolean) (X X -> Boolean)))
;;               (listof Any) -> (union Boolean 'error)
;; --
;; Purpose: Consumes a list of predicate and binary relational operator
;; pairs (predbinlist) and a list of operand (operandlist). A pair in
;; the first list consists of a predicate to determine the data type
;; of the list of operand and a binary relationional operator for the
;; same data type. 
;; If the operand list is empty or has one element, the function produces
;; true. The function produces true if application of at least one relational
;; operator on all consecutive elements of the operand list returns true
;; and false otherwise. If no binary relational operator can be applied
;; to the operand list with two or more elements, the predicate produces error.
;; --
;; Examples: 
(check-expect (is-in-order? (list (list integer? <))
                            '(1 2 3 4 5 6 7 8 9))
              true)
(check-expect (is-in-order? (list (list integer? >))
                            '(1 2 3 4 5 6 7 8 9))
              false)
(check-expect (is-in-order? (list (list char? char=?))
                            '(1 2 3 4 5 6 7 8 9))
              false)
(check-expect (is-in-order? (list (list integer? <)
                                  (list char? char=?))
                            '(1 2 3 4 5 6 7 8 9))
              true)
(check-expect (is-in-order? (list (list char? char=?))
                            '(1 2 3 4 5 6 7 8 9))
              (error "Error: No function to apply to list"))
;; ---------------------------------
;; Definition:
(define (is-in-order? predbinlist operandlist)
  (cond [(or (empty? operandlist)
             (empty? (rest operandlist)))
         true]
        [(empty? predbinlist)
         false]
        [(local [(define (handler predbinlist oplist)
                   (cond [(empty? predbinlist)
                                  true]
                         [else 
                          (local [(define pred? (first (first predbinlist)))]
                            (and (not (pred? (first oplist)))
                                 (handler (rest predbinlist) oplist)))]))
                 (define error? (handler predbinlist operandlist))] 
           error?)
         (error "Error: No function to apply to list")]
        
        [else
         (local [(define predbinpair (first predbinlist))
                 (define pred? (first predbinpair))
                 (define binop (second predbinpair))
                 (define (handler oplist)
                   (cond [(empty? (rest oplist))
                          true]
                         [else 
                          (and (pred? (first oplist))
                               (pred? (second oplist))
                               (binop (first oplist) (second oplist))
                               (handler (rest oplist)))]))
                 (define result-first-predbinpair (handler operandlist))
                 ]
           (or result-first-predbinpair
               (is-in-order? (rest predbinlist) operandlist)))]))
;; ---------------------------------
;; Tests:
(check-expect (is-in-order? (list (list integer? >)
                                  (list char? char=?))
                            '(1 2 3 4 5 6 7 8 9))
              false)
(check-expect (is-in-order? (list (list integer? >)
                                  (list string? string=?)
                                  (list integer? =)
                                  (list integer? <)
                                  )
                            '(1 2 3 4 5 6 7 8 9))
              true)
(check-expect (is-in-order? (list (list integer? <)) empty)
              true)
(check-expect (is-in-order? (list (list integer? <)) (list 1))
              true)
(check-expect (is-in-order? (list (list integer? <)) (list 1 2 7))
              true)
(check-expect (is-in-order? (list (list integer? >)) (list 1 2 7))
              false)
(check-expect (is-in-order? (list (list string? string<?)) (list "1" "2" "7"))
              true)
(check-expect (is-in-order? (list (list integer? >)(list string? string>?)) (list 1 2 7))
              false)

                            
            
            
            
            
                 