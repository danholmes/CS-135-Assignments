;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |bst find missing partial prototype|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; bst-find-missing:  Bst Nat -> (listof Nat)
;; Purpose:
;; Examples:
(define (bst-find-missing tree end-key)
  (...))
(define (find-missing tree end-key list)
  (cond [(empty? tree)
         assigned-list]
        [(leaf? tree)
         (append (count-from-to (first list) (node-key tree) 'exclusive 'inclusive) 
                 (count-from-to (node-key tree) (end-key) 'inclusive 'exclusive))]
        [(empty? (node-left tree))
         ;;(append (count-from-to (
         ...]))   
;; Tests: