(module a7 (lib "plt-pretty-big-text.ss" "lang")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data and type definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (Don't worry about the #f at the end of the define-struct lines.
  ;; That's a technical detail that doesn't matter in the teaching languages.)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-struct node (key val left right) #f)
  ;; A binary search tree (Bst) is one of:
  ;; * empty
  ;; * (make-node Num String Bst Bst)
  
  ;; A FileSystem is a:
  ;; * Dir
  
  ;; A FileDir is one of:
  ;; * File
  ;; * Dir
  
  ;; A FDList is one of:
  ;; * empty
  ;; * (cons FileDir FDList)
  
  (define-struct file (name size timestamp) #f)
  ;; A File = (make-file String Nat Nat)
  
  (define-struct dir (name contents) #f)
  ;; A Dir = (make-dir String FDList)
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Code for Pretty Printing a BST with bst-print
  ;; (minimal design recipe)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; bst-print-indent (listof Boolean) -> String
  ;; Purpose: Print the indentation / lines of the bst
  ;;          each Boolean in lob corresponds to a "depth level" of 
  ;;          the tree and is true if we are on the right side
  (define (bst-print-indent lob)
    (cond [(empty? lob) ""]
          [else (string-append 
                 (cond
                   ;; lowest level and last entry, so put in 'elbows'
                   [(and (empty? (rest lob)) (first lob)) 
                    (string #\u250c #\u2500)]
                   [(empty? (rest lob)) (string #\u2514 #\u2500)]
                   ;; for higher levels, print vertical bar if
                   ;; two sequential left/rights are mismatched
                   [(not (equal? (first lob) (second lob)))
                    (string #\u2502 #\space)]
                   ;; otherwise, just white space
                   [else "  "])
                 (bst-print-indent (rest lob)))]))
  
  ;; bst-print-aux: Bst (listof boolean) -> String
  ;; Purpose: Recurse through a tree printing each node in order.
  ;;          lob 'accumulates' information about whether we're on
  ;;          the right side or the left side of each split
  (define (bst-print-aux tree lob)
    (cond
      [(empty? tree) ""]
      [else (string-append
             (bst-print-aux (node-right tree) (append lob (list true)))
             (bst-print-indent lob) (number->string (node-key tree)) "\n"
             (bst-print-aux (node-left tree) (append lob (list false))))]))
  
  ;; fs-print Bst -> void
  ;; Purpose: Print a pretty Bst, a wrapper for bst-print-aux
  (define (bst-print tree)
    (display (bst-print-aux tree empty)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Code for Pretty Printing a FileSystem with fs-print
  ;; (minimal design recipe)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; lofd-print: (listof (union File Dir)) (listof boolean) -> String
  ;; Purpose: Recurse through a directory (lofd), dispatching each entry
  ;;          to fd-print.  lob 'accumulates' information about whether
  ;;          or not it's the last entry in the dir
  (define (lofd-print lofd lob)
    (cond
      [(empty? lofd) ""]
      [else (string-append 
             (fd-print (first lofd)
                       (append lob (list (empty? (rest lofd)))))
             (lofd-print (rest lofd) lob))]))
  
  ;; fd-print (union File Dir) (listof Boolean) -> String
  ;; Purpose: Print a File or a Dir (and then recurse if it's a Dir)
  ;;          The lob ride-along is used for indentation
  (define (fd-print fd lob)
    (cond 
      [(file? fd) (string-append (fd-print-indent lob)
                                 (string #\u2500 #\space )
                                 (file-name fd) "\n")]
      [else (string-append (fd-print-indent lob)
                           (string #\u25bc #\space ) 
                           (dir-name fd) "\n"
                           (lofd-print (dir-contents fd) lob))]))
  
  ;; fd-print-indent (listof Boolean) -> String
  ;; Purpose: Print the indentation / lines of the filesystem view
  ;;          each Boolean corresponds to a "level of indentation"
  ;;          and is true if we are on the last entry of that level
  (define (fd-print-indent lob)
    (cond [(empty? lob) ""]
          [else 
           (string-append 
            (cond
              ;; lowest level and last entry, so use "L"
              [(and (first lob) (empty? (rest lob))) (string #\u2514 #\u2500)]
              ;; lowest level so use "T"
              [(empty? (rest lob)) (string #\u251c #\u2500)]
              ;; higher level, but last entry, so just blank
              [(first lob) (string #\space #\space)]
              ;; higher level, but not last entry so use "|"
              [else (string #\u2502 #\space)])
            ;; recurse to next (lower) level
            (fd-print-indent (rest lob)))]))
  
  ;; fs-print FileSystem -> void
  ;; Purpose: Print a pretty FileSystem, a wrapper for fd-print
  (define (fs-print fs)
    (display (fd-print fs empty)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define sample-fs
    (make-dir "root" (list 
      (make-file "readme.txt" 187 5)
      (make-dir "photos" (list 
        (make-file "dave.jpg" 3669000 6)
        (make-file "troy.jpg" 2866000 7)
        (make-file "craig.jpg" 2709000 8)
        (make-dir "vacation" (list                    
          (make-file "beach1.jpg" 3297000 9)
          (make-file "beach2.jpg" 2173000 10)
          (make-file "beach3.jpg" 2747000 11)))
        (make-file "ian.jpg" 3287000 12)
        (make-file "byron.jpg" 2294000 13)))
      (make-dir "music" (list 
        (make-dir "rock" (list 
          (make-file "rush-limelight.mp3" 10184000 14)
          (make-file "u2-beautiful-day.mp3" 9693000 15)))
        (make-dir "dance" (list 
          (make-file "lady-gaga-bad-romance.mp3" 9376000 16)
          (make-file "eric-prydz-call-on-me-extended.mp3" 17669000 16)))))
      (make-dir "schoolwork" empty)
      (make-dir "notes" (list 
        (make-file "shopping.txt" 573 17)
        (make-file "todo.txt" 301 18))))))
  
  
  (define sample-bst-smallnum
    (make-node 8 "8" 
    (make-node 4 "4" 
    (make-node 2 "2" 
    (make-node 1 "1" empty empty)
    (make-node 3 "3" empty empty))
    (make-node 6 "6"
    (make-node 5 "5" empty empty)
    (make-node 7 "7" empty empty)))
    (make-node 12 "12"
    (make-node 10 "10"
    (make-node 9 "9" empty empty)
    (make-node 11 "11" empty empty))
    (make-node 14 "14"
    (make-node 13 "13" empty empty) empty))))
  
  (define sample-bst-bignum
    (make-node 154 "150" 
    (make-node 110 "110" empty
    (make-node 142 "142" 
    (make-node 111 "111" empty empty)
    (make-node 144 "144" empty empty)))
    (make-node 212 "212"
    (make-node 177 "177" empty empty)
    (make-node 242 "242" empty
    (make-node 266 "266" empty
    (make-node 391 "391" 
    (make-node 305 "305" empty empty) empty))))))
  
  (define sample-bst (make-node 75 "75" sample-bst-smallnum sample-bst-bignum))
  
  ;; this "provides" the following so they are visible when you "require" them 
  
  (provide    
   
   make-node
   node?
   node-key
   node-val
   node-left
   node-right
   
   make-file
   file?
   file-name
   file-size 
   file-timestamp
   
   make-dir
   dir?
   dir-name
   dir-contents
   
   fs-print
   bst-print
   
   sample-fs
   sample-bst-smallnum
   sample-bst-bignum
   sample-bst
   
   )
  )