;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                             Assignment 8 Q2
;;                            Daniel Holmes
;;                              20418854
;;                                BST
;; *******************************************************************
(require "a7.rkt")
(require racket/trace)

;; *** Sample Constants for Examples ***:
;; First Sample Tree:
;; Fourth Gen
(define ten (make-node 10 "10" empty empty))
(define thirty-five (make-node 35 "35" empty empty))
(define one-ten (make-node 110 "110" empty empty))
(define one-thirty-five (make-node 135 "135" empty empty))
;; Third Gen
(define twenty-five (make-node 25 "25" ten thirty-five))
(define seventy-five (make-node 75 "75" empty empty))
(define one-twenty-five (make-node 125 "125" one-ten one-thirty-five))
(define one-seventy-five (make-node 175 "175" empty empty))
;; Second Gen
(define fifty (make-node 50 "50" twenty-five seventy-five))
(define one-fifty (make-node 150 "150" one-twenty-five one-seventy-five))
;; First gen / root
(define one-hundred (make-node 100 "100" fifty one-fifty))
(define six-leaf-tree one-hundred)
(define non-full-tree-1 one-hundred)
(define one one-hundred) 

;; Second Sample Tree
;; Second Gen
(define bigger thirty-five)
(define smaller ten)
;; First Gen
(define thirty (make-node 30 "30" smaller bigger))
(define two-leaf-tree thirty)
(define full-tree-1 thirty)

;; Third Sample Tree
;; First Gen
(define thirty-one (make-node 31 "31" smaller empty))
(define one-leaf-tree thirty-one)
(define non-full-tree-2 thirty-one)

;(define-struct node (key val left right))
;; A Node = (make-node 

;; *******************************************************************
;; a.) 


(define (root-at-smallest bst)
  (cond [(or (empty? bst)
             (empty? (node-left bst)))
         bst]
        [else
         (local [(define recbst (root-at-smallest (node-left bst)))]
           (make-node (node-key recbst) 
                      (node-val recbst)
                      (node-left recbst)
                      (make-node (node-key bst)
                                 (node-val bst)
                                 (node-right recbst)
                                 (node-right bst))))]))

;(bst-print (root-at-smallest one))
;(bst-print one)

;; *******************************************************************
;; b.)

(define (bst-remove key bst)
  (cond [(not (= (node-key bst) key))
         (cond [(< key (node-key bst))
                (make-node (node-key bst)
                           (node-val bst)
                           (bst-remove key (node-left bst))
                           (node-right bst))]
               [else
                (make-node (node-key bst)
                           (node-val bst)
                           (node-left bst)
                           (bst-remove key (node-right bst)))])]
        [else
         (cond [(empty? (node-right bst))
                (node-left bst)]
               [(empty? (node-left bst))
                (node-right bst)]
               [else
                (local [(define rightbst (root-at-smallest (node-right bst)))]
                  (make-node (node-key rightbst)
                             (node-val rightbst)
                             (node-left bst)
                             (node-right rightbst)))])]))

(define thirty-three (make-node 33 "33" twenty-five empty)) 

(bst-print thirty-three)



         
         
             
        

