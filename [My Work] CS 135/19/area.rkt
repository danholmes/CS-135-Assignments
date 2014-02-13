;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (area-of-polygon list-of-posns) ...)


(define (copy-first-to-end list)

(define (copy-first-to-end-repeater list)
  (cond [(empty? (rest list))
         (first list)]
        [else (cons (copy-first-to-end (rest list))
                    (first list))]))

(define (posn-x-list-make posn-list)
  (cons (posn-x (first posn-list))
        (posn-x-list-make (rest posn-list))))

(define (posn-y-list-make posn-list)
  (cons (posn-y (first posn-list))
        (posn-y-list-make (rest posn-list))))


