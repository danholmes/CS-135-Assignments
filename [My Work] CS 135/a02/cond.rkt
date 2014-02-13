;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; *****************************************
;; Assignment 2, Question 1
;; Daniel Holmes, 2041854
;; (creating an equivalent function to that given in A02, Q1
;; *****************************************

(define (q1a x)
  (cond [(and (not (p1? x)) (p2? x)) (f1 x)]
        [(p3? x) (f2 x)]
        [(not (p1? x)) (f3 x)]
        [else x]
        ))

(define (q1b x)
  (cond [(and (number? x) 
              (or (and (p3? x) (p1? x))
                  (p2? x)))
         'alfa]
        [(and (number? x)
              (p3? x))
         'bravo]
        [(number? x)
         'charlie]
        
        [(and (symbol? x)
              (symbol=? x 'delta))
         x]
        [(symbol? x)'echo]
        [else 'foxtrot]))
              