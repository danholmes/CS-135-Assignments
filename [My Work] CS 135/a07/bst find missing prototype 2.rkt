;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |bst find missing prototype 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (bst-find-missing tree end-key)
  (...))
(define (find-missing tree end-key list)
  (cond [(empty? tree)
         assigned-list]
        [(leaf? tree)
         (append (cut 'upto (node-key tree) list) (cut 'after (node-key tree) list))]
        [(empty? (node-right tree))
         (append (find-missing (node-left tree) (node-key tree) (cut 'upto (node-key tree) list))
                 (cut 'after (node-key tree) list)]
        [(empty? (node-left tree))
         (find-missing (node-right tree) (