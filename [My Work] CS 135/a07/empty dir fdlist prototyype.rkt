;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |empty dir fdlist prototyype|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (empty-dir-in-fdlist? fd)
  (cond [(empty? fd)
         false]
        [(dir? (first fd))
         (cond [(empty? (first fd))
                true]
               [else
                (or 
                 (empty-dir-in-fdlist? (rest fd)) 
                 (empty-dir-in-fdlist? (dir-contents (first fd))))])]
        [(file? (first fd))
          (or false (empty-dir-in-fdlist? (rest fd)))]))