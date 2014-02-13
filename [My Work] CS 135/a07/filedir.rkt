;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname filedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;;                             Assignment 7
;;                            Daniel Holmes
;;                              20418854
;; *******************************************************************
(require "a7.rkt")

;; ******** CONSTANT/STRUCT/LIST/Etc DEDFINITIONS ********
;; A FileSystem is a:
;; * Dir

;; A FileDir is one of:
;; * File
;; * Dir

;; A FDList is one of:
;; * empty
;; * (cons FileDir FDList)

;; (define-struct file (name size timestamp))
;; A File = (make-file String Nat Nat)

;; (define-struct dir (name contents))
;; A Dir = (make-dir String FDList)

;; *******************************************************************
;; (a.)
;; my-file-fn: File -> Any
(define (my-file-fn file)
  (...(file-name)...
   ...(file-size)...
   ...(file-timestamp)...))

;; my-dir-fn: Dir -> Any
(define (my-dir-fn dir)
  (...(dir-name dir)...
   ...(my-fdlist-fn (dir-contents dir))...))

;; my-filedir-fn: FileDir -> Any
(define (my-filedir-fn fildir)
  (cond [(file? fildir)
         (...(my-file-fn fildir)...)
         ]
        [(dir? fildir)
         (...(my-dir-fn fildir)...)
         ]))

;; my-fdlist-fn: FDList -> Any
(define (my-fdlist-fn list)
  (cond [(empty? list)
         (...)
         ]
        [else 
         (...(my-filedir-fn (first list))...
          ... (my-fdlist-fn (rest list))...)
         ]))

;; *******************************************************************
;; (b.)
;; count-dir: FileSystem -> Nat
;; Purpose: Consumes a filesystem, fs, and produces the number of 
;; directories in the entire filesystem tree.
;; Examples:

(define (count-dir fs)
  (+ 1 (count-fdlist (dir-contents fs))))


(define (count-fdlist fd)
  (cond [(empty? fd)
         0]
        [(dir? (first fd))
         (+ 1 
            (count-fdlist (rest fd)) 
            (count-fdlist (dir-contents (first fd))))]
        [(file? (first fd))
          (+ 0 (count-fdlist (rest fd)))]))

;; *******************************************************************
;; (c.)

(check-expect (empty-dir-exist? fs1) true)
(check-expect (empty-dir-exist? dir0) true)
(check-expect (empty-dir-exist? dir00) true)
(check-expect (empty-dir-exist? dir1) false)
(check-expect (empty-dir-exist? dir2) false)

(define (empty-dir-exist? fs)
  (ede-filedir-handler fs))


(define (ede-fdlist-handler fd)
  (cond [(empty? fd)
         false]
        [else
         (or (ede-filedir-handler (first fd))
             (ede-fdlist-handler (rest fd)))]))

(define (ede-filedir-handler fildir)
  (cond [(file? fildir)
         false]
        [(dir? fildir)
         (cond [(empty? (dir-contents fildir))
                true]
               [else (or false (ede-fdlist-handler (dir-contents fildir)))])]))

;; *******************************************************************
;; (d.)
;; A Timestamp is a:
;; * Nat

;; newer-files: FileSystem Timestamp -> (listof String)
;; Purpose: Produces a list of file names that appear in the consumed
;; FileSystem - fs - and any of its subdirectories whose Timestamp
;; is greater than the consumed Timestamp - ts. Ordering follows the 
;; order of the FileSystem and any subdirectories.
;; Examples:
(define (newer-files fs ts)
  (nf-filedir-handler fs ts))
;; Tests:

;; nf-fdlist-handler:
;; Purpose:
;; Examples:
(define (nf-fdlist-handler fs ts)
  (cond [(empty? fs)
         empty]
        [else
         (cond [(cons? (nf-filedir-handler (first fs) ts))
                (append (nf-filedir-handler (first fs) ts) (nf-fdlist-handler (rest fs) ts))]
               [else
                (nf-fdlist-handler (rest fs) ts)])]))
;; Tests:

;; nf-filedir-handler:
;; Purpose:
;; Examples:
(define (nf-filedir-handler fd ts)
  (cond [(empty? fd)
         empty]
        [(file? fd)
         (cond [(> (file-timestamp fd) ts)
                (list (file-name fd))]
               [else
                empty])]
        [(dir? fd)
         (nf-fdlist-handler (dir-contents fd) ts)]))
;; Tests:

;; *******************************************************************
;; (e.)

(define tempfile1 (make-file "t1" 1000 8))
(define tempfile10 (make-file "t10" 1000 8))
(define dir22 (make-dir "twofiles" (list tempfile1 tempfile10)))
(define dir0000 (make-dir "empty3f1" (list dir00 file1 dir00)))
;(check-expect (find-longest dir0000) "empty3f1/empties/emptydir")
;(check-expect (find-longest dir22) "twofiles/t10")

;; find-longest: FileSystem -> String
;; Purpose:
;; Examples:
(define (find-longest fd)
  (...))

(define (make-fs-name-list fs)
  (...))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  




                
        
        