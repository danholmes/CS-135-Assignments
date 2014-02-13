;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname garbage) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define zip-const-list (lambda (const l1) 
                                   (zip (build-list (length l1) (lambda (x) const))
                                        l1)))


(define-struct clusterdata (data index dist))
;; A ClusterData = (make-clusterdata X Nat Num)

;; *******************************************************************
;; *** b.)
;; *******************************************************************
; --
;; zip: (listof X) (listof Y) -> (listof (list X Y))
;; --
;; Purpose:
;; --
;; Examples:
;; ---------------------------------
;; Definition:
(define (zip l1 l2) (map list l1 l2))
;; ---------------------------------
;; Tests:

(define (lambda (const l1)
  (cond [(empty? l1)
         empty]
        [else
         (cons (list const (first l1))
               (zip-const-list const (rest l1)))])))
(define (distfunc-updated distfunc dp-cluster)
  (local [(define dp (first dp-cluster))
          (define cluster (second dp-cluster))]
    (distfunc dp cluster)))


;; ---------------------------------
(define (average-distance distfunc listof-cc listof-dp)
  (local [(define (avg listof-num)
            (/ (foldr (lambda (x y) (+ x y)) 0 listof-num)
               (length listof-num)))
          ;; WORKS:
          (define (my-classify-point dp) 
            (classify-point distfunc dp listof-cc))
          (define new-listof-clusterdata
            (map my-classify-point listof-dp))
          ;; / WORKS
          (define (listof-dist cc list-of-dp)
            (cond [(empty? list-of-dp)
                   empty]
                  [else
                   (cons (distfunc cc (first list-of-dp)) 
                         (listof-dist cc (rest list-of-dp)))]))
          (define (listof-listof-dist list-of-cc)
            (cond [(empty? list-of-cc)
                   empty]
                  [else
                   (cons (listof-dist (first list-of-cc) listof-dp) 
                         (listof-listof-dist (rest list-of-cc)))]))
          (define (find-avg lolod)
            (cond [(empty? lolod)
                   0]
                  [else
                   (+ (avg (first lolod)) (find-avg (rest lolod)))]))
          (define lolo-dist (listof-listof-dist listof-cc))
          (define num-avg (find-avg lolo-dist))]
    avg))