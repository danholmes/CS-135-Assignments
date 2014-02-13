;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rpsls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *********************************************************************
;; Assignment 2, Question 4
;; Daniel Holmes, 20418854
;; (determining the winner in a game of rock-paper-scissors-lizard-spock)
;; *********************************************************************

;; who-wins?: Symbol Symbol -> Symbol
;; Purpose: Determines the winner in a game of rpsls, given the options
;;          chosen by the two players. Assumes there is no tie. Helper function.
;; Examples:
(check-expect (who-wins?'scissors 'rock) 'player2)
(check-expect (who-wins? 'rock 'lizard) 'player1)

(define (who-wins? p1-action p2-action)
  (cond [(equal? p1-action 'rock)
         (cond [(equal? p2-action 'scissors) 'player1]
               [(equal? p2-action 'lizard) 'player1]
               [else 'player2])]
         [(equal? p1-action 'paper)
         (cond [(equal? p2-action 'rock) 'player1]
               [(equal? p2-action 'spock) 'player1]
               [else 'player2])]
         [(equal? p1-action 'scissors)
         (cond [(equal? p2-action 'paper) 'player1]
               [(equal? p2-action 'lizard) 'player1]
               [else 'player2])]
         [(equal? p1-action 'lizard)
         (cond [(equal? p2-action 'spock) 'player1]
               [(equal? p2-action 'paper) 'player1]
               [else 'player2])]
         [(equal? p1-action 'spock)
         (cond [(equal? p2-action 'rock) 'player1]
               [(equal? p2-action 'scissors) 'player1]
               [else 'player2])]))

;; rpsls: Symbol Symbol -> Symbol
;; Purpose: Determines whether the game is a tie or there is indeed a 
;;          winner, determines the winner if there is one, and outputs
;;          the result of the game (tie or the winning player).
;; Examples:
(check-expect (rpsls 'scissors 'lizard) 'player1)
(check-expect (rpsls 'scissors 'spock) 'player2)
   
(define (rpsls p1-action p2-action)
  (cond [(equal? p1-action p2-action) 'tie]
        [else (who-wins? p1-action p2-action)]))

;; rpsls tests
;; Including those in the examples, tests every possible combination.
;; Blank lines seperate the tests by type of option the first player chooses.
(check-expect (rpsls 'scissors 'scissors) 'tie)
(check-expect (rpsls 'scissors 'paper) 'player1)
(check-expect (rpsls 'scissors 'rock) 'player2)

(check-expect (rpsls 'rock 'rock) 'tie)
(check-expect (rpsls 'rock 'paper) 'player2)
(check-expect (rpsls 'rock 'scissors) 'player1)
(check-expect (rpsls 'rock 'lizard) 'player1)
(check-expect (rpsls 'rock 'spock) 'player2)

(check-expect (rpsls 'paper 'rock) 'player1)
(check-expect (rpsls 'paper 'paper) 'tie)
(check-expect (rpsls 'paper 'scissors) 'player2)
(check-expect (rpsls 'paper 'lizard) 'player2)
(check-expect (rpsls 'paper 'spock) 'player1)

(check-expect (rpsls 'lizard 'rock) 'player2)
(check-expect (rpsls 'lizard 'paper) 'player1)
(check-expect (rpsls 'lizard 'scissors) 'player2)
(check-expect (rpsls 'lizard 'lizard) 'tie)
(check-expect (rpsls 'lizard 'spock) 'player1)

(check-expect (rpsls 'spock 'spock) 'tie)
(check-expect (rpsls 'spock 'rock) 'player1)
(check-expect (rpsls 'spock 'paper) 'player2)
(check-expect (rpsls 'spock 'scissors) 'player1)
(check-expect (rpsls 'spock 'lizard) 'player2)

