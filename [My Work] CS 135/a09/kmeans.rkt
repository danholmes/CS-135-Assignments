;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname kmeans) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                          CS 135 FALL 2012
;;                           Assignment 9 Q1
;;                            Daniel Holmes
;;                              20418854
;;                               k-means
;; *******************************************************************

(require "simpledata.rkt")
(require "irisdata.rkt")

;; *******************************************************************
;; *** Sample Constants for Example/Test Inputs 
;; *******************************************************************

(define (num-dist num1 num2) (abs (- num1 num2)))

(define (num-avg lon) (cond [(empty? lon) 0] [else (/ (foldr + 0 lon) 
                                                      (length lon))]))

;; *******************************************************************
;; *** Functions
;; *******************************************************************
;; *** a.)
;; *******************************************************************

(define-struct clusterdata (data index dist))
;; A ClusterData = (make-clusterdata X Nat Num)

;; *******************************************************************
;; *** b.)
;; *******************************************************************
;; --
;; classify-point: (X X -> Num[>=0]) X (ne-listof X) -> ClusterData
;; --
;; Purpose: Takes a distance function (distfunc), a datapoint (dp), 
;; and a non-empty list of cluster-centres (clust-list) and produces
;; a ClusterData with fields: the input data point, the index (position
;; in the list) of the closest cluster-center in the input list of
;; cluster-centers (determined by distfunc), and the distance to the
;; smaller index of the cluster-center in the input list of two 
;; cluster-centers give the same distance.
;; --
;; Examples: 
(check-expect (classify-point num-dist 5 (list 1 2 3 6 9))
              (make-clusterdata 5 3 1))
(check-expect (classify-point num-dist 5 (list 4 1 2 3 6 9))
              (make-clusterdata 5 0 1))
(check-expect (classify-point num-dist 5 (list 4.1 1 2 3 6 9))
               (make-clusterdata 5 0 0.9))
;; ---------------------------------
;; Definition:
(define (classify-point distfunc dp clust-list)
  (;; START local 
   local [(define (distfunc-clust-index cluster-index-pair)
            (local [(define cluster (first cluster-index-pair))]
              (distfunc dp cluster)))
          (define (zip l1 l2) (map list l1 l2))
          (define cluster-dp-index
            (argmin distfunc-clust-index (zip clust-list 
                                              (build-list (length clust-list)
                                                          (lambda (x) x)))))]
    ;; END local
  (make-clusterdata dp 
                    (second cluster-dp-index) 
                    (distfunc dp (first cluster-dp-index)))))
;; ---------------------------------
;; Tests:
(check-expect (classify-point num-dist 5 (list 4 6))
               (make-clusterdata 5 0 1))

;; *******************************************************************
;; *** c.)
;; *******************************************************************
;; --
;; get-new-center: ((listof X) -> X) Nat (ne-listof ClusterData) -> X
;; --
;; Purpose: Consumes an averaging function (avg-func), a natural number
;; representing the cluster index (clust-index) and a list of ClusterData,
;; and produces the average of all the data-points in the list whose 
;; clusterdata-index matches the input index. At least one must match.
;; --
;; Examples:
(check-expect (get-new-center num-avg 1 (list (make-clusterdata 4 1 0) 
                                              (make-clusterdata 100 2 0)
                                (make-clusterdata 6 1 0)
                                (make-clusterdata 150 2 0))) 
              5)
(check-expect (get-new-center num-avg 
                              2 
                              (list (make-clusterdata 4 1 0)
                                    (make-clusterdata 100 2 0) 
                                    (make-clusterdata 6 1 0) 
                                    (make-clusterdata 150 2 0)))
              125)
;; ---------------------------------
;; Definition:
(define (get-new-center avg-func clust-index listof-clusterdata)
  (local [(define filtered-cd-list 
            (filter (lambda (x) 
                      (cond [(= clust-index (clusterdata-index x)) 
                             true]
                            [else false]))
                    listof-clusterdata))
          (define listof-dp-from-listof-fcd (map clusterdata-data 
                                                 filtered-cd-list))]
   (avg-func listof-dp-from-listof-fcd)))

;; ---------------------------------
;; Tests:
(check-expect (get-new-center num-avg 
                              3 
                              (list (make-clusterdata 2 3 0)
                                    (make-clusterdata 19 3 0)
                                    (make-clusterdata 1928 3 0)
                                    (make-clusterdata -129 3 0)
                                    (make-clusterdata 1000000 1 0)
                                    (make-clusterdata 304 3 0)))
              424.8)
(check-expect (get-new-center num-avg 
                              3 
                              (list (make-clusterdata 2 3 0)
                                    (make-clusterdata 19 3 0)))
              10.5)
(check-expect (get-new-center num-avg 
                              3 
                              (list (make-clusterdata -2 3 0)
                                    (make-clusterdata -50 3 0)
                                    (make-clusterdata -140 3 0)))
              -64)
(check-expect (get-new-center num-avg 
                              3 
                              (list (make-clusterdata 2 3 0)
                                    (make-clusterdata 19 3 0)
                                    (make-clusterdata 1928 3 0)
                                    (make-clusterdata -130 3 0)
                                    (make-clusterdata 1000000 1 0)))
              454.75)
(check-expect (get-new-center num-avg 
                              3 
                              (list (make-clusterdata 4 3 0)))
              4)

;; *******************************************************************
;; *** d.)
;; *******************************************************************
;; --
;; get-new-centers: (X X -> Num[>=0]) ((listof X) -> X) (listof X) 
;;                  (listof X)
;;                  -> (listof X)
;; --
;; Purpose: Consumes a distance function (distfunc), an averaging 
;; function (avgfunc), a list of cluster-centers (listof-cc), 
;; and a list of data-points (listof-dp). Produces a new list
;; of cluster-centers using a single K-means step of distance 
;; computation and averaging (where K is the number of cluster-centers).
;; Output order is the same as in the input list of clusters.
;; --
;; Examples:
(check-expect (get-new-centers num-dist num-avg (list 0 10)
                 (list 1 2 3 11 12 13))
              (list 2 12))
(check-expect (get-new-centers num-dist num-avg (list 0 -10)
                 (list -1 -2 -3 -11 -12 -13))
              (list -2 -12))
;; ---------------------------------
;; Definition:
(define (get-new-centers distfunc avgfunc listof-cc listof-dp)
  (local [(define (my-classify-point dp) 
            (classify-point distfunc dp listof-cc))
          (define new-listof-clusterdata
            (map my-classify-point listof-dp))
          (define (my-get-new-center clust-index)
            (get-new-center avgfunc clust-index new-listof-clusterdata))
          (define my-new-centers (map my-get-new-center 
                                      (build-list (length listof-cc) 
                                                  (lambda (x) x))))
          ]
    my-new-centers))
;; ---------------------------------
;; Tests:
(check-expect (get-new-centers num-dist num-avg (list 0 10)
                 (list 0 0 1 1 2 2 3 3 4 4))
              (list 2 0))
(check-expect (get-new-centers num-dist num-avg (list 15 50 70)
                 (list 0 14 22 36 48 54 68 75 82 98))
              (list 12 46 80.75))
(check-expect (get-new-centers num-dist num-avg (list 2 4)
                 (list 1 3))
              (list 2 0))

;; *******************************************************************
;; *** e.)
;; *******************************************************************
;; --
;; average-distance: (X X -> Num[>=0]) (listof X) (listof X) 
;;                   -> Num
;; --
;; Purpose: Consumes a distance function (distfunc), a non-empty list
;; of cluster-centers (listof-cc), and a non-empty list of data points 
;; (listof-dp). Produces a Num representing the average 
;; distance/distortionfrom all data-points in the listof-dp to their 
;; closest cluster-centers in listof-cc. 
;; --
;; Examples:
(check-expect (average-distance num-dist 
                                (list 1 2 3) 
                                (list 0.9 1.0 1.1 1.9 
                                      2.0 2.1 2.9 3.0 3.1))
              1/15)

(define (average-distance distfunc listof-cc listof-dp)
  (local [(define (avg listof-num)
            (/ (foldr (lambda (x y) (+ x y)) 0 listof-num)
               (length listof-num)))
          (define (my-classify-point dp) 
            (classify-point distfunc dp listof-cc))
          (define new-listof-clusterdata
            (map my-classify-point listof-dp))
          (define list-of-dist (map clusterdata-dist 
                                    new-listof-clusterdata))
          (define avg-dist (avg list-of-dist))
          ]
    avg-dist))
;; Tests:
(check-expect (average-distance num-dist 
                                (list -1 -2 -3) 
                                (list -0.9 -1.0 -1.1 -1.9 
                                      -2.0 -2.1 -2.9 -3.0 -3.1))
              1/15)
(check-expect (average-distance num-dist 
                                (list 0 20 40) 
                                (list 1 4 9 7 5 11 21 30 36 49 23 54 29 26))
              6.5)
(check-expect (average-distance num-dist 
                                (list 1) 
                                (list 1 1 1 1 1 1 1 1 1))
              0)
(check-expect (average-distance num-dist 
                                (list -1) 
                                (list 1))
              2)

;; *******************************************************************
;; *** f.)
;; *******************************************************************
;; --
;; k-means: (X X -> Num[>=0]) ((listof X) -> X) (ne-listof X) 
;;          (ne-listof X) Num[>0] -> (X -> Num)
;; --
;; Purpose: Consumes a distance function (distfunc), an averaging
;; function (avgfunc), a non-empty list of cluster-centers (listof-cc),
;; a non-empty list of data-points (listof-dp), and a number greater
;; than zero (threshold). Produces a function which takes a datapoint
;; and produces an index corresponding to the closest cluster-center
;; after the K-means algorithm has been run on the input listof-dp
;; and listof-cc.
;; ---------------------------------
;; Definition:
(define (k-means distfunc avgfunc listof-cc listof-dp threshold)
  (local [(define new-listof-cc (get-new-centers distfunc avgfunc 
                                                 listof-cc listof-dp))]
   (cond [(> threshold (abs (- (average-distance distfunc 
                                                 listof-cc 
                                                 listof-dp)
                               (average-distance distfunc 
                                                 new-listof-cc 
                                                 listof-dp))))
         (lambda (x)
           (clusterdata-index (classify-point distfunc x listof-cc)))]
        [else
         (k-means distfunc avgfunc new-listof-cc listof-dp threshold)])))
;; ---------------------------------
;; Tests:   
(define classifier (k-means simple-dist 
                            simple-avg 
                            (list (first simple-data)
                                  (second simple-data)
                                  (third simple-data)
                                  (fourth simple-data))
                            simple-data
                            0.01))

(check-expect (map classifier simple-data) simple-classes)

(define classifier-2 (k-means iris-dist
                              iris-avg
                              (list (first iris-data)
                                    (second iris-data)
                                    (third iris-data))
                              iris-data
                              0.01))

(define (check-errors stu-list correct-list)
  (cond [(empty? correct-list)
         0]
        [(equal? (first stu-list) (first correct-list))
         (+ 0 (check-errors (rest stu-list) (rest correct-list)))]
        [else
         (+ 1 (check-errors (rest stu-list) (rest correct-list)))]))

(check-expect (check-errors (map classifier-2 iris-data) 
                            iris-classes) 
              12)