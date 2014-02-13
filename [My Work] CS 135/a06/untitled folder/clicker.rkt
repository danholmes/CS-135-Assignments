;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname clickerv2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;; Assignment 6, Question 1
;; (clicker)
;; Daniel Holmes, 20418854
;; *******************************************************************
;; *** DEFINED CONSTANTS ***
(define %-top-ans 0.75) 
(define question-value 2)
(define value-corr-ans 2)
(define value-incorr-ans 1)
(define value-unans-ques 0)

;; *** FUNCTIONS *** 

;; clicker-grade: (listof (union 'A 'B 'C 'D 'E 'none))
;;                (listof (union 'A 'B 'C 'D 'E)) 
;;                -> Num
;; Purpose: To calculate a student's grade from the given list of their
;; clicker responses (stu-responses) and the given list of the correct
;; clicker responses. The student may have responded with a letter (A to E)
;; (Symbol) response or they may not have responded at all - 'none. 
;; The correct clicker answers are all one of the letters A to E (Symbol).
;; This function uses the pre-defined values for the different possibilities
;; in terms of awarding marks (ques-weight, corr-ans, incorr-ans, no-ans).
;; Examples:

(check-expect (clicker-grade (list 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A)
                             (list 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A))
              100)
(check-expect (clicker-grade (list 'A 'A 'A 'A 'A 'A 'A 'none 
                                   'none 'none 'none 'none)
                             (list 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A))
              100)

(define (clicker-grade stu-responses corr-responses)
  (participation-mark (count stu-responses corr-responses 'all)
                      (count stu-responses corr-responses 'right)
                      (count stu-responses corr-responses 'wrong)))


;; Tests:




;; count: (listof (union 'A 'B 'C 'D 'E 'none))
;;        (listof (union 'A 'B 'C 'D 'E)) 
;;        (union 'right 'wrong 'all)
;;         -> Num
;; Purpose: Depending on the value given for count-type
;; ('right, 'wrong, 'all), this function counts the number of correct
;; responses, incorrect responses, or simply the total number of responses,
;; and returns that number. It takes in a list of student responses
;; (stu-responses) and a list of the correst responses (corr-responses) to
;; use when determining the number of correct/incorrect/all responses.
;; Examples:


(define (count stu-responses corr-responses count-type)
  (cond [(empty? stu-responses)
         0]
        [(and 
          (or (equal? count-type 'right)
              (equal? count-type 'all))
          (equal? (first stu-responses)
                  (first corr-responses)))
         (+ 1 (count (rest stu-responses)
                     (rest corr-responses)
                     count-type))]
        
        [(and 
          (or (equal? count-type 'wrong)
              (equal? count-type 'all))
          (not (equal? (first stu-responses) 'none))
          (not (equal? (first stu-responses)
                  (first corr-responses))))
         (+ 1 (count (rest stu-responses)
                     (rest corr-responses)
                     count-type))]
        
        [(equal? count-type 'all)
         (+ 1 (count (rest stu-responses)
                     (rest corr-responses)
                     count-type))]
        
        [else 
         (+ 0 (count (rest stu-responses)
                     (rest corr-responses)
                     count-type))]))

;; Tests
(check-expect (count (list 'A 'A 'A) (list 'A 'A 'A) 'right)
              3)
(check-expect (count (list 'A 'A 'B) (list 'A 'A 'A) 'right)
              2)
(check-expect (count (list 'A 'B 'B) (list 'A 'A 'A) 'right)
              1)
(check-expect (count (list 'B 'B 'B) (list 'A 'A 'A) 'right)
              0)
(check-expect (count (list 'A 'A 'A) (list 'A 'A 'A) 'wrong)
              0)
(check-expect (count (list 'A 'A 'B) (list 'A 'A 'A) 'wrong)
              1)
(check-expect (count (list 'A 'B 'B) (list 'A 'A 'A) 'wrong)
              2)
(check-expect (count (list 'B 'B 'B) (list 'A 'A 'A) 'wrong)
              3)
(check-expect (count (list 'A 'A 'A) (list 'A 'A 'A) 'all)
              3)
(check-expect (count (list 'A 'A 'B) (list 'A 'A 'A) 'all)
              3)
(check-expect (count (list 'A 'B 'B) (list 'A 'A 'A) 'all)
              3)
(check-expect (count (list 'B 'B 'B) (list 'A 'A 'A) 'all)
              3)
(check-expect (count (list 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A)
                     (list 'A 'A 'A 'A 'A 'A 'A 'A 'A 'A)
                     'right)
              10)


;; participation-mark: Int Int Int -> Int
;; Purpose: To calculate the student's participation mark given the total clicker
;;          questions asked, the number they answered correctly, and the number they
;;          answered incorrectly.
;; Examples:
(check-expect (participation-mark 100 50 50) 
              [* 100 (/ 125 150)]) 
(check-expect (participation-mark 100 50 25) 
              [* 100 (/ 125 150)]) 
(check-expect (participation-mark 100 50 0) 
              [* 100 (/ 100 150)]) 

(define (participation-mark TQA NCA NIA)
  (cond [; (NCA >= MCQ)
         (>= NCA (* TQA %-top-ans))  
         (* (/ value-corr-ans question-value)
             100)]
        
        [{;(NCA < MCQ), (NCA+NIA >= MCQ)
          and (< NCA (* TQA %-top-ans)) 
              (>= (+ NCA NIA) (* TQA %-top-ans))}
         {* (/ (+ (* NCA value-corr-ans) 
               (*(- (* TQA %-top-ans) NCA ) value-incorr-ans))
            (* TQA %-top-ans question-value))
            100}]
        
        [;(NCA < MCQ), (NCA+NIA < MCQ)and (< NCA (* TQA %-top-ans))
              (< (+ NCA NIA) (* TQA %-top-ans))
         {* (/ (+ (* NCA value-corr-ans) 
                  (* NIA  value-incorr-ans)
                  (* (- (* TQA %-top-ans) (+ NCA NIA) ) value-unans-ques))
               (* TQA %-top-ans question-value))
            100}]))
;; Tests
(check-expect (participation-mark 400 0 0) 0)
(check-expect (participation-mark 400 0 400) 50)
(check-expect (participation-mark 400 0 300) 50)
(check-expect (participation-mark 400 0 200) 200/6)
(check-expect (participation-mark 400 0 100) 100/6)
(check-expect (participation-mark 400 400 0) 100)
(check-expect (participation-mark 400 300 0) 100)
(check-expect (participation-mark 400 200 0) 400/6)
(check-expect (participation-mark 400 100 0) 200/6)
(check-expect (participation-mark 400 300 100) 100)
(check-expect (participation-mark 400 200 200) 500/6)
(check-expect (participation-mark 400 100 300) 400/6)
(check-expect (participation-mark 400 100 50) 250/6)
(check-expect (participation-mark 400 50 100) 200/6)
    
    
    
    