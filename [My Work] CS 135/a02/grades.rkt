;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ******************************************
;; CS 135 Assignment 2, Question 3
;; Daniel Holmes, 20418854
;; ******************************************

;; Abbreviation Dictionary
;; TQA = Total Questions Asked
;; NCQ = Number Correct Questions
;; NIQ = Number Incorrect Questions

;; Defined Constants
;; Explained above definition where not self-obvious enough.
; the percent of the student's top answers that are marked
(define %-top-ans 0.75) 
(define question-value 2)
(define value-corr-ans 2)
(define value-incorr-ans 1)
(define value-unans-ques 0)
(define final-exam-weight 0.45)
(define first-midterm-weight 0.10)
(define second-midterm-weight 0.20)
(define overall-assignments-weight 0.20)
(define participation-weight 0.05)
; minimum-assignment-grade = minimum overall assignment grade needed 
; to not fail the course
(define minimum-assignment-grade 50)
; minimum-exam-grade = minimum overall exams grade needed to 
; not fail the course
(define minimum-exam-grade 50)
; maximum grade the student will achieve if they have failed the exam 
; and/or assignment grade minimum
(define maximum-failing-grade 46)
; for final-cs135-exam-grade-needed function, the minimum grade the student 
; wishes to acheieve in the course 
(define desired-grade 60)
(define min-weighted-exam-mark 50)
(define min-possible-mark 0)
(define max-possible-mark 100)

;; Function Definitions

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

;; participation-mark tests
;; Seperated by title describing type of student answers given

;; some amount of correct answers. 
(check-expect (participation-mark 100 100 0) 
              100) ;; NCA > MCQ
(check-expect (participation-mark 100 75 0) 
              100) ;; NCA = MCQ
(check-expect (participation-mark 100 50 0) 
              [* 100 (/ 100 150)]) ;; NCA < MCQ
;; some amount of correct answers and incorrect answers
(check-expect (participation-mark 100 50 50) 
              [* 100 (/ 125 150)]) ;; (NCA < MCQ), (NCA+NIA > MCQ)
(check-expect (participation-mark 100 50 25) 
              [* 100 (/ 125 150)]) ;; (NCA < MCQ), (NCA+NIA = MCQ)
(check-expect (participation-mark 100 25 25) 
              [* 100 (/ 75 150)]) ;; (NCA < MCQ), (NCA+NIA < MCQ)
;; some amount of incorrect answers
(check-expect (participation-mark 100 0 100) 
              [* 100 (/ 75 150)]) ;; (NCA = 0), (NIA > MCQ)
(check-expect (participation-mark 100 0 75) 
              [* 100 (/ 75 150)]) ;; (NCA = 0), (NIA = MCQ)
(check-expect (participation-mark 100 0 50) 
              [* 100 (/ 50 150)]) ;; (NCA = 0), (NIA < MCQ)
;; all unanswered
(check-expect (participation-mark 100 0 0 ) 
              0) 

;; final-cs135-grade: Int Int Int Int Int -> Int
;; Purpose: To determine the student's final grade in the course.
;; Examples:
(check-expect (final-cs135-grade 100 100 100 100 100) 100)
(check-expect (final-cs135-grade 0 0 0 0 0) 0)

(define (final-cs135-grade final-exam-grade 
                           first-midterm-grade
                           second-midterm-grade
                           overall-assignments-grade
                           participation-grade)
  (cond [ ; if assignments or exams mark do not meet minimum, theyve failed, calculate failing mark
         (or (< overall-assignments-grade minimum-assignment-grade)
            (< (calculate-weighted-exam-grade final-exam-grade 
                                              first-midterm-grade 
                                              second-midterm-grade) 
               min-weighted-exam-mark))
        (cond [(> (calculate-grade final-exam-grade 
                                   first-midterm-grade
                                   second-midterm-grade
                                   overall-assignments-grade
                                   participation-grade)
                  maximum-failing-grade)
               maximum-failing-grade]
              
              [else (calculate-grade final-exam-grade 
                                     first-midterm-grade
                                     second-midterm-grade
                                     overall-assignments-grade
                                     participation-grade)])]
        [ ; otherwise, their grades DO meet minimums, so calculate passing mark
         else (calculate-grade final-exam-grade 
                                     first-midterm-grade
                                     second-midterm-grade
                                     overall-assignments-grade
                                     participation-grade)]))
;; final-cs135-grade Tests
(check-expect (final-cs135-grade 49 49 49 100 100) 46)
(check-expect (final-cs135-grade 100 100 100 46 100) 46)

;; calculate-weighted-exam-grade: Int Int Int -> Int
;; Purpose: Calculates weighted exam grade from three exams
;; Examples:
(check-expect (calculate-weighted-exam-grade 0 0 0) 0)
(check-expect (calculate-weighted-exam-grade 100 100 100) 100)

(define (calculate-weighted-exam-grade final-exam-grade 
                                       first-midterm-grade
                                       second-midterm-grade)
  (/ (+ (* final-exam-grade final-exam-weight)
        (* first-midterm-grade first-midterm-weight)
        (* second-midterm-grade second-midterm-weight))
     (+ final-exam-weight
        first-midterm-weight
        second-midterm-weight)))


;; calculate-grade: Int Int Int Int Int -> Int
;; Purpose: Calculates their final grade
;; Examples:
(check-expect (calculate-grade 100 100 100 100 100) 100)
(check-expect (calculate-grade 0 0 0 0 0 ) 0)

(define (calculate-grade final-exam-grade 
                           first-midterm-grade
                           second-midterm-grade
                           overall-assignments-grade
                           participation-grade)
  (+ (* final-exam-grade final-exam-weight)
     (* first-midterm-grade first-midterm-weight)
     (* second-midterm-grade second-midterm-weight)
     (* overall-assignments-grade overall-assignments-weight)
     (* participation-grade participation-weight)))

;; final-cs135-exam-grade-needed: Int Int Int Int -> Any
;; Calculates needed mark on final exam to exceed the minimum average exam mark
;; and the desired final mark (defined above, originally 60), or returns 'impossible
;; if its not possible to meet the minimum assignments or exams requirement, or their
;; desired mark in the course.
;; Examples:
(check-expect (final-cs135-exam-grade-needed 0 0 0 0) 'impossible)
(check-expect (final-cs135-exam-grade-needed 100 100 0 100) 'impossible)
              
(define (final-cs135-exam-grade-needed first-midterm-grade
                                       second-midterm-grade
                                       overall-assignments-grade
                                       participation-grade)
  (cond [ ; check if their assignments grade meets minimum required, if it doesnt return "impossible"
         (< overall-assignments-grade minimum-assignment-grade)
         'impossible]
        [ ; check if getting a perfect mark on the exam will allow them to meet minimum exam requirement
          ; or their desired-grade, if it doesn't then return "impossible"
         (or (< (calculate-grade max-possible-mark 
                                  first-midterm-grade
                                  second-midterm-grade
                                  overall-assignments-grade
                                  participation-grade) 
                 desired-grade)
             (< (calculate-weighted-exam-grade max-possible-mark
                                                first-midterm-grade
                                                second-midterm-grade)
                 min-weighted-exam-mark))
         'impossible]
         ; SINCE their assignments mark meets minimum required, AND SINCE the mark required for them to
          ; achieve both the minimum-weighted-exam requirement and their desired-final-mark, THEN
          ; calculate the exam mark they must at least achieve to receive at least the desired mark
         [else
           (* (/ (- desired-grade
                  (* first-midterm-grade first-midterm-weight)
                  (* second-midterm-grade second-midterm-weight)
                  (* overall-assignments-grade overall-assignments-weight)
                  (* participation-grade participation-weight))
                (* final-exam-weight 100)) 
            100)]))
        
            
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    