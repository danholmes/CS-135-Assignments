;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |flatten key string|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (flatten tree)
  (cond [(empty? tree)
         empty]
        [(leaf? tree)
         (list (list (node-key tree) (node-val tree)))]
        [(empty? (node-right tree))
         (append (flatten (node-left tree)) (list (list (node-key tree) (node-val tree))))]
        [(empty? (node-left tree))
         (append (flatten (node-right tree)) (list (list (node-key tree) (node-val tree))))]
        [else
         (append (flatten (node-left tree)) 
                 (list (list (node-key tree) (node-val tree)))
                 (flatten (node-right tree)))]))