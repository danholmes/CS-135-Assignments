;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area-proto3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(check-expect (copy-first-to-end (cons 1 (cons 2 (cons 3 empty))))
              (cons 2 (cons 3 (cons 1 empty))))

(define (copy-first-to-end list)
  (add-to-end (rest list) 
              (first list)))
        
(define (add-to-end list to-add)
  (cond [(empty? list)
         (cons to-add empty)]
        [else (cons (first list)
                    (add-to-end (rest list) to-add))]))

(define (area-of-polygon posn-list) 
  (* 0.5 
     (aop-repeater posn-list
                   (copy-first-to-end posn-list))))

(define (aop-repeater posn-list fte-posn-list)
  (cond [(empty? posn-list)
         0]
        [else
         (+ (- (* (posn-x (first posn-list)) 
                  (posn-y (first fte-posn-list)))
               (* (posn-x (first fte-posn-list))
                  (posn-y (first posn-list))))
            (aop-repeater (rest posn-list) (rest fte-posn-list)))]))

(check-expect (area-of-polygon (cons (make-posn 15 15)
                                     (cons (make-posn 23 30)
                                           (cons (make-posn 50 25) empty))))
              -222.5)
           
  