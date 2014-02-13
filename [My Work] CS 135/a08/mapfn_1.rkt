;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mapfn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                           Assignment 8 Q3
;;                            Daniel Holmes
;;                              20418854
;;                                BST
;; *******************************************************************
;; a.)
(check-expect (mapfn (list + - * / list remainder) '(1 1))
              (list 2 0 1 1 (list 1 1) 0))
(check-expect (mapfn (list + - * / list remainder '(-1 -1)))
              (list -2 0 1 1 (list -1 -1) 0))
(check-expect (mapfn (list + - * / list remainder  '(-1 1)))
              (list 0 -2 -1 -1 (list -1 1) 0))


(define (mapfn fn-list num-pair)
  (cond [(empty? fn-list)
         empty]
        [(and (equal? (first fn-list) /)
              (equal? (second num-pair) 0))
         (error "Error: Cannot divide by zero.")]
        [else
         (cons ((first fn-list) (first num-pair) (second num-pair))
               (mapfn (rest fn-list) num-pair))]))
                                       
(mapfn (list + - * /) '(3 1))

;; *******************************************************************
;; b.)

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

(define (is-in-order? predbinlist operandlist)
  (cond [(or (empty? operandlist)
             (empty? (rest operandlist)))
         true]
        [(empty? predbinlist)
         false]
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

            
            
            
            
            
            
                 