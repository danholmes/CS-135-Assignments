;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                             Assignment 7
;;                            Daniel Holmes
;;                              20418854
;;                                BST
;; *******************************************************************
(require "a7.rkt")

;; ******** CONSTANT/STRUCT/LIST/Etc DEDFINITIONS ********
; (define-struct node (key val left right))

;; A binary search tree (Bst) is one of:
;; * empty
;; * (make-node Num String Bst Bst)

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

;; ******** FUNCTION DEFINITIONS ********
;; **** (a) 
;; bst-left-count: Bst -> Nat
;; Purpose: consumes a binary search tree (Bst), tree, and produces the
;; total number of leaves in the tree.
;; Examples:
(check-expect (bst-leaf-count six-leaf-tree) 6)
(check-expect (bst-leaf-count two-leaf-tree) 2)
(check-expect (bst-leaf-count one-leaf-tree) 1)

(define (bst-leaf-count tree) 
   (cond
     [(empty? tree) 0]
     [(leaf? tree) 1]
     [else
      (+ 0 
         (bst-leaf-count (node-left tree))
         (bst-leaf-count (node-right tree)))]))

;; Tests:

;; leaf?: ne-Bst -> Boolean
;; Purpose: Consumes a non-empty binary search tree (Bst), tree, and 
;; produces whether or not it is a leaf.
;; Examples:

(define (leaf? tree)
  (cond [(empty? tree)
         (error "Error: given tree is empty.")]
        [(and (empty? (node-left tree))
              (empty? (node-right tree)))
         true]
        [else false]))
         
;; Tests:

;; *******************************************************************
;; (b)
;; bst-full?: Bst -> Boolean
;; Purpose: Consumes a binary search tree (Bst), tree, and produces
;; a boolean indicating whether or not it is full
(check-expect (bst-full? non-full-tree-1) true)
(check-expect (bst-full? non-full-tree-2) false)
(check-expect (bst-full? full-tree-1) true)

(define (bst-full? tree) 
  (cond [(empty? tree) true]
        [(leaf? tree)
         true]
        [(not (node-full? tree))
         false]
        [else
         (and (bst-full? (node-left tree))
              (bst-full? (node-right tree)))]))
         

;; node-full?: ne-Bst-> Boolean
;; Purpose: Consumes a non-empty binary search tree (Bst), tree,
;; and produces a boolean indicating whether or not it is full.
;; Examples:

(define (node-full? tree)
  (cond [(empty? tree) 
         (error "Given tree is empty")]
         [(or (empty? (node-right tree))
             (empty? (node-left tree)))
          false]
         [else true]))
;; Tests:
         
;; *******************************************************************
;; (c)

;; bst-add: Bst Int String -> Bst
;; Purpose: Consumes a binary search tree, tree, an integer, key, and 
;; a string, value. If the key/value pair (node) is not present in the
;; tree, the function produces a new Bst with it included in the tree
;; in the appropriate place.
;; Examples:

(define (bst-add tree key value)
  (cond [(empty? tree)
         empty]
        [; replace
         (= (node-key tree) key)
         (make-node (node-key tree) 
                    value 
                    (node-left tree) 
                    (node-right tree))]
        [; insert left
         (and (< key (node-key tree)) 
              (empty? (node-left tree)))
         (make-node (node-key tree) 
                    (node-val tree)
                    (make-node key value empty empty) 
                    (node-right tree))]
        [; insert right
         (and (< (node-key tree) key)
              (empty? (node-right tree)))
         (make-node (node-key tree) 
                    (node-val tree)
                    (node-left tree)
                    (make-node key value empty empty))]
        [else ; recurse
         (cond [(> key (node-key tree))
                (make-node (node-key tree) 
                           (node-val tree) 
                           (node-left tree) 
                           (bst-add (node-right tree) key value))]
               [(< key (node-key tree))
                (make-node (node-key tree) 
                           (node-val tree) 
                           (bst-add (node-left tree) key value) 
                           (node-right tree))])]))
;; Tests:

;; *******************************************************************
;; (d)
;; An Association List (AL) is one of:
;; * empty
;; * (cons (list Num String) AL)

;; bst->desal: Bst -> AL
;; Purpose:  
;; Examples:
(define (bst->desal tree)
  (flatten tree 'descending))
;; Tests:


;; flatten:
;; Purpose:
;; Examples:

(define (flatten tree order)
  (cond [(empty? tree)
         empty]
        [(leaf? tree)
         (list (list (node-key tree) (node-val tree)))]
        [(symbol=? order 'ascending)
         (cond 
           [(empty? (node-right tree))
            (append (flatten (node-left tree) order) (list (list (node-key tree) (node-val tree))))]
           [(empty? (node-left tree))
            (append (flatten (node-right tree) order) (list (list (node-key tree) (node-val tree))))]
           [else
            (append (flatten (node-left tree) order) 
                    (list (list (node-key tree) (node-val tree)))
                    (flatten (node-right tree) order))])]
        [(symbol=? order 'descending)
         (cond 
           [(empty? (node-right tree))
            (append (list (list (node-key tree) (node-val tree))) (flatten (node-left tree) order))]
           [(empty? (node-left tree))
            (append (list (list (node-key tree) (node-val tree))) (flatten (node-right tree) order))]
           [else
            (append  (flatten (node-right tree) order)
                    (list (list (node-key tree) (node-val tree)))
                    (flatten (node-left tree) order))])]))
;; Tests:

;; *******************************************************************
;; (e)
;; bst-find-missing:  Bst Nat -> (listof Nat)
;; Purpose:
;; Examples:
(define (bst-find-missing tree end-key)
  (cond [(empty? tree)
         (count-from-to 0 end-key 'exclusive 'exclusive)]
        [else
         (find-missing tree end-key (count-from-to 0 end-key 'exclusive 'exclusive))]))

(define (find-missing tree last-list list)
  (cond [(empty? tree)
         list]
        [else
         (append (find-missing (node-left tree) (sub1 (node-key tree)) (cut list 'keep-before (node-key tree)))
                 (find-missing (node-right tree) last-list (cut list 'keep-after (node-key tree))))]))
                                   
         
;; Tests:

;; cut: (listof Nat) (union 'keep-before 'keep-after) Nat -> (listof Nat)
;; Purpose:
;; Examples:

(define (cut list type marker)
  (cond [(symbol=? type 'keep-before)
         (cond [(< (first list) marker)
                (cons (first list) (cut (rest list) type marker))]
               [(= (first list) marker)
                empty]
               [else
                (error "something went wrong")])]
        [(symbol=? type 'keep-after)
         (cond [(empty? list)
                empty]
                [(> (first list) marker)
                 (cons (first list) (cut (rest list) type marker))]
                [(<= (first list) marker)
                 (cut (rest list) type marker)])]))
;; Tests:

;; count-from-to: Nat Nat (union 'inclusive 'exclusive) -> (listof Nat)
;; Purpose: Produces a list of natural numbers between the first number
;; consumed, start, and the second number consumed, end. Inclusive. 
;; If n is any natural number in the list,  start < n < end 
;; Examples:
(define (count-from-to start end start-type end-type)
  (cond [(and (symbol=? start-type 'inclusive)
              (symbol=? end-type 'inclusive))
         (cond [(= (add1 start) end)
                empty]
               [else 
                (append (list (add1 start)) (count-from-to (add1 start) end start-type end-type))])]
        [(and (symbol=? start-type 'exclusive)
              (symbol=? end-type 'exclusive))
         (cond [(= start (add1 end))
                empty]
               [else
                (append (list start) (count-from-to (add1 start) end start-type end-type))])]
        [(and (symbol=? start-type 'inclusive)
              (symbol=? end-type 'exclusive))
         (cond [(= start end)
                empty]
               [else 
                (append (list (add1 start)) (count-from-to (add1 start) end start-type end-type))])]
        [(and (symbol=? start-type 'exclusive)
              (symbol=? end-type 'inclusive))
         (cond [(= start  end)
                empty]
               [else 
                (append (list start) (count-from-to (add1 start) end start-type end-type))])]))

;; *******************************************************************
;; (f)



         
                
         



   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   