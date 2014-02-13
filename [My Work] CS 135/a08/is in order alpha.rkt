;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |is in order alpha|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(check-expect (handler (list integer? <) '(1 2 3 4 5 6 7 8 9)) true)
(check-expect (handler (list integer? >) '(1 2 3 4 5 6 7 8 9)) false)
(check-expect (handler (list integer? <) '(1 3 2 4 5 6 7 8 9)) false)
(check-expect (handler (list integer? <) '(1 2 "hello" 4 5 6 7 8 9)) false)
(check-expect (handler (list integer? <) '(1 2 3 4 5 6 7 8 9)) true)

(define (handler predbinpair oplist)
  (cond [(or (empty? oplist)
             (empty? (rest oplist)))
         true]
        [else 
         (local [(define pred? (first predbinpair))
                 (define binop (second predbinpair))]
         (and (pred? (first oplist))
              (pred? (second oplist))
              (binop (first oplist) (second oplist))
              (handler predbinpair (rest oplist))))]))
(handler (list integer? >) (list 1 2 7))