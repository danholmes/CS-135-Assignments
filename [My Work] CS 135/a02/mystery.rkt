;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mystery) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ************************************************************
;; Assignment 2, Question 2
;; Daniel Holmes, 20418854
;; (creating an equivalent function to that given in A02, Q2)
;; ************************************************************

;; bool-mystery?: Boolean Boolean Boolean -> Boolean
;; Purpose: Returns second constant if first is true, otherwise returns
;;          the third variable.
;; Examples:
(check-expect (bool-mystery? true true true) true)
(check-expect (bool-mystery? true false true) false)

(define (bool-mystery? a b c)
  (or (and a b)
      (and (not a) c)))

;; bool-mystery? tests
(check-expect (bool-mystery? true true true) true)
(check-expect (bool-mystery? true false true) false)
(check-expect (bool-mystery? true false 149) false)
(check-expect (bool-mystery? true false false) false)
(check-expect (bool-mystery? true true false) true)
(check-expect (bool-mystery? false true true) true)
(check-expect (bool-mystery? false false true) true)
(check-expect (bool-mystery? false true false) false)
(check-expect (bool-mystery? false false false) false)
(check-expect (bool-mystery? false 129 true) true)
(check-expect (bool-mystery? false 120 false) false)