;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname garbage) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; * 3
;    (list (state-grid a-state)
;          grid-transpose
;          (duplicates? (state-grid a-state))
;          (duplicates? grid-transpose)
;          
;          left-vis-feasible
;          right-vis-feasible
;          top-vis-feasible
;          bottom-vis-feasible
;          no-duplicates-present)
; **** 2
(define teststate (make-state
                   (list
                    (list 4 '? '? '?)
                    (list '? '? '? '?)
                    (list '? '? '? '?)
                    (list '? '? '? '?))
                   (list 1 3 2 2)
                   (list 3 2 2 1)
                   (list 1 3 2 3)
                   (list 2 2 3 1)))

(define cur-row (first (state-grid teststate)))

(define f-cur-row 
  (filter (lambda (x)
            (not (equal? x '?)))
          cur-row))

(define fs-cur-row
  (quicksort f-cur-row <=))

(define dup-pres
  (cond [(empty? (rest fs-cur-row))
         false]
        [else
         (ormap (lambda(x) x)
                (map = 
                     fs-cur-row 
                     (append (rest fs-cur-row) 
                             (list (first fs-cur-row)))))]))
cur-row
f-cur-row
fs-cur-row
dup-pres

(define (get-listof-visible loc lov)
  (local [(define floc (filter (lambda (x) 
                                (not (equal? '? x)))
                              loc))]
   (cond [(empty? floc)
         lov]
        [(empty? lov)
         (get-listof-visible (rest floc) (list (first floc)))]
        [(> (first floc) (first lov))
         (get-listof-visible (rest floc) (cons (first floc) lov))]
        [else
         (get-listof-visible (rest floc) lov)])))

(define (fv? lov vis)
  (local [(define flov (filter (lambda (x)
                                 (not (equal? x '?)))
                               lov))
          (define lo-1-to-n 
            (build-list (length lov) add1))
          (define lo-nonpresent
            (filter (lambda (x)
                      (not (member? x lov))) lo-1-to-n))
          (define rev-lo-nonpresent
            (reverse lo-nonpresent))
          (define (insert-into-lov ins-list)
            (cond [(equal? '? (first lov))
                   (append ins-list flov)]
                  [else
                   (append flov ins-list)]))
          (define vis-min-list (insert-into-lov rev-lo-nonpresent))
          (define vis-max-list (insert-into-lov lo-nonpresent))
          (define vis-min (visibility-number vis-min-list))
          (define vis-max (visibility-number vis-max-list))
          (define vis-within-bounds (or (and (<= vis-min vis)
                                             (<= vis vis-max))
                                        (empty? flov)))]
    (list lo-1-to-n 
          lo-nonpresent 
          rev-lo-nonpresent 
          vis-min-list 
          vis-max-list
          vis-min
          vis-max
          vis-within-bounds)))
;(fv? '(2 1 ? ?) 2)