;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                          CS 135 FALL 2012
;;                           Assignment 8 Q2
;;                            Daniel Holmes
;;                              20418854
;;                                BST
;;
;; Please note: I have performed more tests than given but this code is 
;; already expansive and I don't want it to get too lengthy. I have
;; provided what I believe to be sufficient testing while keeping the
;; length of this code mangeable and easy-to-mark.
;; *******************************************************************

(define-struct node (key val left right))
;; A binary search tree (Bst) is one of:
;; * empty
;; * (make-node Num String Bst Bst) 

;; *******************************************************************
;; *** Sample Constants for Example/Test Inputs 
;; *******************************************************************

;; ---------------------------------
;; First Sample Input Tree:
;; ---------------------------------
;; Picture:
;  ┌─175
;┌─150
;│ │ ┌─135
;│ └─125
;│   └─110
;100
;│ ┌─75
;└─50
;  │ ┌─35
;  └─25
;    └─10
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
(define input-tree-one one-hundred)

;; ---------------------------------
;; Second Sample Input Tree
;; ---------------------------------
;; Picture:
;┌─35
;30
;└─10
;; Second Gen
(define bigger thirty-five)
(define smaller ten)
;; First Gen / root
(define thirty (make-node 30 "30" smaller bigger))
(define input-tree-two thirty)

;; ---------------------------------
;; Third Sample Input Tree
;; ---------------------------------
;; Picture:
;31
;└─10
;; First Gen
(define thirty-one (make-node 31 "31" ten empty))
(define input-tree-three thirty-one)

;; ---------------------------------
;; Fourth Sample Input Tree
;; ---------------------------------
;; Picture:
;33
;│ ┌─35
;└─25
;  └─10
;; First gen / root
(define thirty-three (make-node 33 "33" twenty-five empty)) 
(define input-tree-four thirty-three)

;; ---------------------------------
;; Fifth Sample Input Tree
;; ---------------------------------
;; Picture:
;  ┌─35
;┌─25
;│ └─10
;1
;; First gen / root
(define one (make-node 1 "1" empty twenty-five)) 
(define input-tree-five one)

;; *******************************************************************
;; *** Sample Constants for Example/Test Outputs
;; *******************************************************************
;; ---------------------------------
;; *** A. One Output Tree
;; ---------------------------------
;; Picture:
;    ┌─175
;  ┌─150
;  │ │ ┌─135
;  │ └─125
;  │   └─110
;┌─100
;│ │ ┌─75
;│ └─50
;│   │ ┌─35
;│   └─25
;10
(define a-out-1 (make-node
                 10
                 "10"
                 empty
                 (make-node 100 "100"
                            (make-node 50 "50"
                                       (make-node 25 "25" 
                                                  empty 
                                                  (make-node 35 "35" empty empty))
                                       (make-node 75 "75" empty empty))
                            (make-node 150 "150"
                                       (make-node 125 "125" 
                                                  (make-node 110 "110" empty empty) 
                                                  (make-node 135 "135" empty empty))
                                       (make-node 175 "175" empty empty)))))
;; ---------------------------------
;; A. Second Output Tree
;; ---------------------------------
;; Picture:
;  ┌─35
;┌─30
;10
(define a-out-2 (make-node 10 "10" empty 
                           (make-node 30 "30" empty 
                                      (make-node 35 "35" empty empty))))
;; ---------------------------------
;; A. Third Output Tree
;; ---------------------------------
;; Picture:
;┌─31
;10
(define a-out-3
  (make-node 10 "10" empty 
             (make-node 31 "31" empty empty)))
;; ---------------------------------
;; A. Fourth Output Tree
;; ---------------------------------
;; Picture:
;┌─33
;│ │ ┌─35
;│ └─25
;10
(define a-out-4 (make-node 10 "10"
                           empty
                           (make-node 33 "33" 
                                      (make-node 25 "25" empty 
                                                 (make-node 35 "35" empty empty)) 
                                      empty)))
;; ---------------------------------
;; *** B. First Output Tree
;; ---------------------------------
;; Picture:
;    ┌─175
;┌─150
;│ │ ┌─135
;│ └─125
;110
;│ ┌─75
;└─50
;  │ ┌─35
;  └─25
;    └─10
(define b-out-1
  (make-node 110 "110"
             (make-node 50 "50"
                        (make-node 25 "25" 
                                   (make-node 10 "10" empty empty) 
                                   (make-node 35 "35" empty empty))
                        (make-node 75 "75" empty empty))
             (make-node 150 "150"
                        (make-node 125 "125" empty (make-node 135 "135" empty empty))
                        (make-node 175 "175" empty empty))))
;; ---------------------------------
;; B. Second Output Tree
;; ---------------------------------
;; Picture:
;┌─175
;│ │ ┌─135
;│ └─125
;│   └─110
;100
;│ ┌─75
;└─50
;  │ ┌─35
;  └─25
;    └─10
(define b-out-2
  (make-node 100 "100"
             (make-node 50 "50"
                        (make-node 25 "25" 
                                   (make-node 10 "10" empty empty) 
                                   (make-node 35 "35" empty empty))
                        (make-node 75 "75" empty empty))
             (make-node 175 "175"
                        (make-node 125 "125" 
                                   (make-node 110 "110" empty empty) 
                                   (make-node 135 "135" empty empty))
                        empty)))
;; ---------------------------------
;; B. Third Output Tree
;; ---------------------------------
;; Picture:
;  ┌─175
;┌─150
;│ │ ┌─135
;│ └─125
;│   └─110
;100
;│ ┌─75
;└─50
;  └─35
;    └─10
(define b-out-3
  (make-node 100 "100"
             (make-node 50 "50"
                        (make-node 35 "35" 
                                   (make-node 10 "10" empty empty) empty)
                        (make-node 75 "75" empty empty))
             (make-node 150 "150"
                        (make-node 125 "125" 
                                   (make-node 110 "110" empty empty) 
                                   (make-node 135 "135" empty empty))
                        (make-node 175 "175" empty empty))))
;; ---------------------------------
;; B. Fourth Output Tree
;; ---------------------------------
;; Picture:
;35
;└─10
(define b-out-4
  (make-node 35 "35" (make-node 10 "10" empty empty) empty))
;; ---------------------------------
;; B. Fifth Output Tree
;; ---------------------------------
;; Picture:
;┌─35
;30
(define b-out-5 
  (make-node 30 "30" empty (make-node 35 "35" empty empty)))
;; ---------------------------------
;; B. Sixth Output Tree
;; ---------------------------------
;; Picture:
;30
;└─10
(define b-out-6 
  (make-node 30 "30" (make-node 10 "10" empty empty) empty))
;; ---------------------------------
;; B. Seventh Output Tree
;; ---------------------------------
;; Picture:
;┌─35
;25
;└─10
(define b-out-7
  (make-node 25 "25" (make-node 10 "10" empty empty) (make-node 35 "35" empty empty)))




;; *******************************************************************
;; *** (a.) 
;; *******************************************************************
;; --
;; root-at-smallest: Bst -> Bst
;; --
;; Purpose: Rearranges and produces the consumed binary search tree 
;; (bst) such that the its smallest element is at the root. If the 
;; bst is emtpy, the function produces empty.
;; --
;; Examples: 
;; (please reference constant definitions for easy-to-view pictures
;; of the associated inputs and outputs for each check-expect)
(check-expect (root-at-smallest input-tree-one)
              a-out-1)
(check-expect (root-at-smallest input-tree-two)
              a-out-2)
(check-expect (root-at-smallest input-tree-three)
              a-out-3)
;; ---------------------------------
;; Definition:
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
;; ---------------------------------
;; Tests:
(check-expect (root-at-smallest input-tree-four)
              a-out-4)

;; *******************************************************************
;; *** (b.) 
;; *******************************************************************
;; bst-remove: Num Bst -> Bst
;; --
;; Purpose: Consumes a number (key) and a binary search tree (bst)
;; and produces the bst with the node containing the given key removed.
;; If the bst does not contain a node with the given key, the bst is
;; produced unaltered.
;; --
;; Examples:
;; (please reference constant definitions for easy-to-view pictures
;; of the associated inputs and outputs for each check-expect)
(check-expect (bst-remove 100 input-tree-one)
              b-out-1) ; key in root node
(check-expect (bst-remove 150 input-tree-one)
              b-out-2) ; key in right trunk node w/ left, right nodes
(check-expect (bst-remove 25 input-tree-one)
              b-out-3) ; key in left trunk node w/ left, right nodes
(check-expect (bst-remove 1000 input-tree-one)
              input-tree-one) ; key not present

;; ---------------------------------
;; Definition:
(define (bst-remove key bst)
  (cond [(empty? bst)
         empty]
        [(not (= (node-key bst) key))
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
;; ---------------------------------
;; Tests:
(check-expect (bst-remove 30 input-tree-two)
              b-out-4) ; key in root node w/ left, right nodes
(check-expect (bst-remove 10 input-tree-two)
              b-out-5) ; key in right leaf
(check-expect (bst-remove 35 input-tree-two)
              b-out-6) ; key in left leaf
(check-expect (bst-remove 33 input-tree-four)
              b-out-7) ; key in node w/ only left node
(check-expect (bst-remove 1 input-tree-five)
              b-out-7) ; key in node w/ only right node

         
         
             
        

