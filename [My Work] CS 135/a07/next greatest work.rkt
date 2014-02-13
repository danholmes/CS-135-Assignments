;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |next greatest work|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define-struct greatest (key value rank))
;; 

(define (find-x-greatest tree rank)
  (cond [(empty? tree)
         false]
        [; if its a leaf
         (leaf? tree)
         (make-greatest (node-key tree) (node-val tree) 1)]
        [; if a full stem
         (and (node? (node-left tree))
              (node? (node-right tree)))
         (cond [; the left is the correct rank
                (>= (greatest-rank (find-x-greatest (node-left tree) rank) rank))
                (greatest-rank (find-x-greatest (node-left tree) rank))]
               [; the current node is the correct rank
                (>= (add1 (greatest-rank (find-x-greatest (node-left tree) rank))) rank)
                (make-greatest (node-key tree) 
                               (node-val tree) 
                               (add1 (greatest-rank (find-x-greatest (node-left tree) rank))))]
               [; 
                
                
                
        ;[; if the desired greatest is on the left side
         ;(< rank (greatest-rank (find-x-greatest (node-left tree) rank)))
         ;...]






(define (flatten2 tree num-greatest)
  (cond [(empty? tree)
         empty]
        [; nothing on the right
         (empty? (node-right tree))
         (append (list (list (node-key tree) (node-val tree))) (flatten2 (node-left tree) num-greatest))]
        [ ; nothing on the left
         (empty? (node-left tree))
         (append (list (list (node-key tree) (node-val tree))) (flatten2 (node-right tree) num-greatest))]
        [; stuff on right and left
         else
         (cond [(>= (length(flatten2 (node-left tree) num-greatest)) num-greatest)
                (flatten2 (node-left tree) num-greatest)]
               [else
                (append  (flatten2 (node-left tree) num-greatest)
                         (list (list (node-key tree) (node-val tree)))
                         (flatten2 (node-right tree) num-greatest))])]))






(define (find-equal-or-greater tree key)
  (cond [; key matches, or is greater than, current node-key
         (= (node-key tree) key)
         (node-key tree)]
        [; current node-key is greater than key but there are no smaller
         (and (< key (node-key tree))
              (empty? (node-left tree)))
         (node-key tree)]
        [; current node-key is greater than key and next smaller node-key
         ; is smaller than key and there are none in between 
         (and (< key (node-key tree))
              (< (node-key (node-left tree)) key)
              (empty? (node-right (node-left tree))))
         (node-key tree)]
        [; there are no node-keys equal or greater than key
         (and (< (node-key tree) key)
              (empty? (node-right tree)))
         false]
        [; recurse
         else
         (cond 
           [(< (node-key tree) key)
            (find-equal-or-greater (node-right tree) key)]
           [(< key (node-key tree))
            (find-equal-or-greater (node-left tree) key)])]))