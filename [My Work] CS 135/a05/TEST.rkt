;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname TEST) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define bieber-list (list #\B #\i #\e #\b #\e #\r))



(define (find-bieber string-to-search)
  (locate-bieber (string->list string-to-search) bieber-list 1))





(define (locate-bieber list-of-char list-of-bieber location-of-bieber)
  (cond [(empty? list-of-bieber)
         location-of-bieber]
        [(empty? list-of-char)
         "No Biebers here!"]
         [(char=? (first list-of-char)
                    (first list-of-bieber))
         (locate-bieber (rest list-of-char)
                          (rest list-of-bieber)
                          location-of-bieber)]
         [(not (equal? list-of-bieber bieber-list))
          (locate-bieber (rest list-of-char) bieber-list (+ 1 location-of-bieber))]
         [else (locate-bieber (rest list-of-char) list-of-bieber (+ 1 location-of-bieber))]))

(check-expect (find-bieber "Biebe Biebe Biebe r ffwefwefwefwef Bieber")
              36)