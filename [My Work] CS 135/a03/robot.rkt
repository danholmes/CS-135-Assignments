;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; *******************************************************************
;; Assignment 3, Question 2
;; (creating a robot navigator)
;; Daniel Holmes, 20418854
;; *******************************************************************
;; a.)
(define-struct robotcmd (dist dir))
;; A robotcmd = (make-robotcmd Nat Char)

;; *******************************************************************
;; b.)

;; robot-ctrl: Posn RobotCmd -> Posn
;; Purpose: To calculate the resulting position of a robot after moving
;; the robot the distance in the given direction (given in nav-cmd) from
;; its current position (cur-pos).
;; Examples:
(check-expect (robot-ctrl (make-posn 0 0) 
                          (make-robotcmd 5 'north)) 
              (make-posn 0 5))
(check-expect (robot-ctrl (make-posn 0 0) 
                          (make-robotcmd 5 'south)) 
              (make-posn 0 -5))
(check-expect (robot-ctrl (make-posn 0 0) 
                          (make-robotcmd 5 'west)) 
              (make-posn -5 0))
(check-expect (robot-ctrl (make-posn 5 0) 
                          (make-robotcmd 5 'east)) 
              (make-posn 10 0))

(define (robot-ctrl cur-pos nav-cmd) 
  (add-posns cur-pos 
             (conv-robotcmd-to-posn nav-cmd)))

;; add-posns: Posn Posn -> Posn
;; Purpose: To add two Posns, posn1 and posn2, like one would add vectors.
;; Examples:
(check-expect (add-posns (make-posn 1 1)
                         (make-posn 2 2))
              (make-posn 3 3))
(check-expect (add-posns (make-posn 1 1)
                         (make-posn -2 8))
              (make-posn -1 9))
(check-expect (add-posns (make-posn -7 -1)
                         (make-posn 2 0))
              (make-posn -5 -1))

(define (add-posns posn1 posn2)
  (make-posn (+ (posn-x posn1) (posn-x posn2))
             (+ (posn-y posn1) (posn-y posn2))))

;; conv-robotcmd-to-posn: RobotCmd -> Posn
;; Purpose: To convert a RobotCmd, nav-cmd, to a vector-like Posn. 
;; This can be added to a position posn, satisfying the original nav-cmd.
;; Examples:
(check-expect (conv-robotcmd-to-posn (make-robotcmd 5 'north)) 
              (make-posn 0 5))
(check-expect (conv-robotcmd-to-posn (make-robotcmd 5 'south)) 
              (make-posn 0 -5))
(check-expect (conv-robotcmd-to-posn (make-robotcmd 5 'west)) 
              (make-posn -5 0))
(check-expect (conv-robotcmd-to-posn (make-robotcmd 5 'east)) 
              (make-posn 5 0))

(define (conv-robotcmd-to-posn nav-cmd)
  (cond [(equal? (robotcmd-dir nav-cmd)'north)
         (make-posn 0 (robotcmd-dist nav-cmd))]
        [(equal? (robotcmd-dir nav-cmd) 'south)
         (make-posn 0 (- 0 (robotcmd-dist nav-cmd)))]
        [(equal? (robotcmd-dir nav-cmd) 'west)
         (make-posn (- 0 (robotcmd-dist nav-cmd)) 0)]
        [(equal? (robotcmd-dir nav-cmd) 'east)
         (make-posn (robotcmd-dist nav-cmd) 0)]))

;; *******************************************************************
;; c.)

;; robot-obstacle-ctrl: Posn RobotCmd Posn -> Posn
;; Purpose: To produce a new robot location, given the current location
;; (cur-pos), a navigation command (nav-cmd), and the position of an
;; obstacle the robot may encounter along its path. If the robot meets
;; the obstacle, the location returned will be the obstacle. If not,
;; it will be the result of moving according to the nav-cmd.
;; Examples:
(check-expect (robot-obstacle-ctrl (make-posn 0 0)
                                   (make-robotcmd 5 'north)
                                   (make-posn 0 3))
              (make-posn 0 3))
(check-expect (robot-obstacle-ctrl (make-posn 0 0)
                                   (make-robotcmd 5 'north)
                                   (make-posn 0 7))
              (make-posn 0 5))
(check-expect (robot-obstacle-ctrl (make-posn 0 0)
                                   (make-robotcmd 5 'north)
                                   (make-posn 0 0))
              (make-posn 0 0))

(define (robot-obstacle-ctrl cur-pos nav-cmd obs-pos)
  (cond [(equal? cur-pos obs-pos)
        cur-pos]
        [(collision? cur-pos nav-cmd obs-pos)
         obs-pos]
        [else (robot-ctrl cur-pos nav-cmd)]))
         
;; collision?: Posn RobotCmd Posn -> Boolean
;; Purpose: To determine if the robot has collided with the obstacle
;; (positions given in cur-pos and obs-pos, respectively) while 
;; traveling along the path given in the navigation command (nav-cmd).
;; Examples:
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'north)
                          (make-posn 0 5)) true)
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'south)
                          (make-posn 0 -5)) true)
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'east)
                          (make-posn 5 0)) true)
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'west)
                          (make-posn -5 0)) true)

(define (collision? cur-pos nav-cmd obs-pos)
  (cond [(and (symbol=? (robotcmd-dir nav-cmd) 'south)
              (= (posn-x cur-pos) (posn-x obs-pos))
              (< (- (posn-y obs-pos) (posn-y cur-pos)) 
                 0)
              (<= (abs (- (posn-y obs-pos) (posn-y cur-pos)))
                  (robotcmd-dist nav-cmd)))
         true]
        [(and (symbol=? (robotcmd-dir nav-cmd) 'north)
              (= (posn-x cur-pos) (posn-x obs-pos))
              (> (- (posn-y obs-pos) (posn-y cur-pos)) 
                 0)
              (<= (abs (- (posn-y cur-pos) (posn-y obs-pos)))
                  (robotcmd-dist nav-cmd)))
         true]
         [(and (symbol=? (robotcmd-dir nav-cmd) 'east)
              (= (posn-y cur-pos) (posn-y obs-pos))
              (> (- (posn-x obs-pos) (posn-x cur-pos)) 
                 0)
              (<= (abs (- (posn-x obs-pos) (posn-x cur-pos)))
                  (robotcmd-dist nav-cmd)))
         true]
        [(and (symbol=? (robotcmd-dir nav-cmd) 'west)
              (= (posn-y cur-pos) (posn-y obs-pos))
              (< (- (posn-x obs-pos) (posn-x cur-pos)) 
                 0)
              (<= (abs (- (posn-x cur-pos) (posn-x obs-pos)))
                  (robotcmd-dist nav-cmd)))
         true]
         [else false]))

;; Collision? tests
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'north)
                          (make-posn 0 -5)) false)
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'south)
                          (make-posn 0 5)) false)
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'east)
                          (make-posn -5 0)) false)
(check-expect (collision? (make-posn 0 0)
                          (make-robotcmd 5 'west)
                          (make-posn 5 0)) false)         
;; *******************************************************************
; d.)

(define-struct robotctrlcmd (position command))
;; A robotctrlcmd = (make-robotctrlcmd Posn RobotCmd)

;; robot-ctrl2: Posn RobotCtrlCmd RobotCtrlCmd -> Posn
;; Purpose: Takes two commands from two controllers. Determines
;; distance to the two controllers and follows the command of the 
;; closer one. Doesn't follow any if they are equally distant. Respective 
;; commands and distances are given in robotctrlcmd-1 and robotctrlcmd-2. 
;; The current position of the robot is cur-pos. Returns the final 
;; robot position.
;; Examples:
(check-expect (robot-ctrl2 (make-posn 0 0)
                           (make-robotctrlcmd (make-posn 0 1) 
                                              (make-robotcmd 10 'north))
                           (make-robotctrlcmd (make-posn 1 0) 
                                              (make-robotcmd 5 'north)))
              (make-posn 0 0))
(check-expect (robot-ctrl2 (make-posn 0 0)
                           (make-robotctrlcmd (make-posn 0 2) 
                                              (make-robotcmd 10 'north))
                           (make-robotctrlcmd (make-posn 1 0) 
                                              (make-robotcmd 5 'north)))
              (make-posn 0 5))
(check-expect (robot-ctrl2 (make-posn 0 0)
                           (make-robotctrlcmd (make-posn 1 0) 
                                              (make-robotcmd 10 'east))
                           (make-robotctrlcmd (make-posn 2 0) 
                                              (make-robotcmd 5 'north)))
              (make-posn 10 0))
                           
(define (robot-ctrl2 cur-pos robotctrlcmd-1 robotctrlcmd-2)
  (cond [(> (sqr-dist cur-pos
                      (robotctrlcmd-position robotctrlcmd-1))
            (sqr-dist cur-pos
                      (robotctrlcmd-position robotctrlcmd-2)))
         (robot-ctrl cur-pos (robotctrlcmd-command robotctrlcmd-2))]
        [(< (sqr-dist cur-pos
                      (robotctrlcmd-position robotctrlcmd-1))
            (sqr-dist cur-pos
                      (robotctrlcmd-position robotctrlcmd-2)))
         (robot-ctrl cur-pos (robotctrlcmd-command robotctrlcmd-1))]
        [else cur-pos]))

;; sqr-dist: Posn Posn -> Num
;; Purpose: Computes the distance, squared, between the two points
;; given by origin and point (both Posns). Returns this distance as
;; a Num. Designed based on the formula for the Euclidean distance
;; between two points.
;; Examples:
(check-expect (sqr-dist (make-posn 0 0)
                        (make-posn 5 5))
              50)
(check-expect (sqr-dist (make-posn 2 7)
                        (make-posn 5 5))
              13)
(check-expect (sqr-dist (make-posn -10 -10)
                        (make-posn 5 5))
              450)
(define (sqr-dist origin point)
  (+ (sqr (- (posn-x point)
             (posn-x origin)))
     (sqr (- (posn-y point)
             (posn-y origin)))))
