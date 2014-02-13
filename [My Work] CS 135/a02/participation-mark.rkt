;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname participation-mark) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; TQA = Total Questions Asked
;; NCQ = Number Correct Questions
;; NIQ = Number Incorrect Questions
(define %TOPANS 0.75)
(define Question-Value 2)
(define Value-Corr-Ans 2)
(define Value-Incorr-Ans 1)
(define Value-Unans-Ques 0)

;; MAKE HELPER FUNCTIONS TO REDUCE COMPLEXITY~~~!!!!!!

(define (participation-mark TQA NCA NIA)
  (cond [(>= NCA (* TQA %TOPANS))  ; (NCA >= MCQ)
         (* (/ Value-Corr-Ans Question-Value)
             100)]
        
        [{and (< NCA (* TQA %TOPANS)) ;(NCA < MCQ), (NCA+NIA >= MCQ)
              (>= (+ NCA NIA) (* TQA %TOPANS))}
         {* (/ (+ (* NCA Value-Corr-Ans) 
               (*(- (* TQA %TOPANS) NCA ) Value-Incorr-Ans))
            (* TQA %TOPANS Question-Value))
            100}]
        
        [{and (< NCA (* TQA %TOPANS)) ;(NCA < MCQ), (NCA+NIA < MCQ)
              (< (+ NCA NIA) (* TQA %TOPANS))}
         {* (/ (+ (* NCA Value-Corr-Ans) 
                  (* NIA  Value-Incorr-Ans)
                  (* (- (* TQA %TOPANS) (+ NCA NIA) ) Value-Unans-Ques))
               (* TQA %TOPANS Question-Value))
            100}]))

;; CorrectAns
(check-expect (participation-mark 100 100 0) 100) ;; NCA > MCQ
(check-expect (participation-mark 100 75 0) 100) ;; NCA = MCQ
(check-expect (participation-mark 100 50 0) [* 100 (/ 100 150)]) ;; NCA < MCQ

;; CorrectAns + IncorrectAns
(check-expect (participation-mark 100 50 50) [* 100 (/ 125 150)]) ;; (NCA < MCQ), (NCA+NIA > MCQ)
(check-expect (participation-mark 100 50 25) [* 100 (/ 125 150)]) ;; (NCA < MCQ), (NCA+NIA = MCQ)
(check-expect (participation-mark 100 25 25) [* 100 (/ 75 150)]) ;; (NCA < MCQ), (NCA+NIA < MCQ)

;; IncorrectAns
(check-expect (participation-mark 100 0 100) [* 100 (/ 75 150)]) ;; (NCA = 0), (NIA > MCQ)
(check-expect (participation-mark 100 0 75) [* 100 (/ 75 150)]) ;; (NCA = 0), (NIA = MCQ)
(check-expect (participation-mark 100 0 50) [* 100 (/ 50 150)]) ;; (NCA = 0), (NIA < MCQ)

;; Unans
(check-expect (participation-mark 100 0 0 ) 0) ;; All unans
        
         