(module plotdata (lib "plt-pretty-big-text.ss" "lang")

  (provide   plotdata   plotimage)

  (require "imagedata.rkt")
  (require (lib "image.ss" "2htdp")) 
  ;; a list of colors we can use 
  ;; mycolors: (listof String)
  (define mycolors (list "red" "green" "blue" "black" "cyan" "purple" "pink" "yellow" "grey" "orange"))

  ;; main plotdata function
  ;; plotdata: (listof Num) (listof Nat) Nat -> image  
  (define (plotdata thedata indices scale)
    (plot-data thedata indices mycolors scale scale))
  
  ;;plot-data:  (listof Num) (listof Nat) (listof String) Nat Nat -> image
  ;;plots the data using the list colors, one color per index indices.  
  ;;The indices must range between 0 and (length colors)
  ;;xscale and yscale define the size of the output image, 
  ;; these should be set according to your device
  ;; using xscale=yscale=500 works for me, but try
  ;;something larger/smaller if you want a bigger/smaller picture
  (define (plot-data thedata indices colors xscale yscale)
    (local [(define datax (map first thedata))
            (define datay (map second thedata))
            (define xmax (foldr max (first datax) datax))
            (define ymax (foldr max (first datay) datay))
            (define xmin (foldr min (first datax) datax))
            (define ymin (foldr min (first datay) datay))
            (define xrange (- xmax xmin))
            (define yrange (- ymax ymin))]
      (plot-data-help thedata indices colors xscale yscale xrange yrange xmin ymin)))
      
  ;;plot-data-help: (listof Num) (listof Nat) (listof String) Nat Nat Num Num Num Num -> image
  (define (plot-data-help data indices colors xscale yscale xrange yrange xmin ymin)
    (cond [(empty? data) (empty-scene xscale yscale)]
          [else (place-image (circle 3 "solid" (list-ref colors (remainder (first indices) (length mycolors))))
                             (* (/ (- (first (first data)) xmin) xrange) xscale)
                             (* (/ (- (second (first data)) ymin) yrange) xscale)
                             (plot-data-help (rest data)(rest indices) colors xscale yscale xrange yrange xmin ymin))]))

;;a method for converting imprecise numbers to integers in 0...255
;;chooses the element of the lst that is close to val
;;lookup: Num[0..255] (listof Nat) -> Nat
(define (lookup val lst)
  (foldr (lambda (x y) (cond [(< (abs (- val x)) 0.1) x] [else y])) 0 lst))

;;convert float to byte
;;Num -> Nat
(define (convert-float-to-byte x)
  (lookup (round (* x 255)) (build-list 256 (lambda (x) x))))

;;convert pixels to colors
;;convert-pixel-tocolor: Pixel->Color
(define (convert-pixel-to-color p)
  (make-color (convert-float-to-byte (pixel-red p))
              (convert-float-to-byte (pixel-green p))
              (convert-float-to-byte (pixel-blue p))))

;;builds a list of lists giving every pixel location in a nx by ny image
;; starting at row i and column j
;; buildxy Nat Nat Nat Nat -> (listof (list Nat Nat))
  (define (buildxy i j nx ny)
    (cond [(= i nx) empty]
          [(= j ny) (buildxy (+ i 1) 0 nx ny)]
          [else (cons (list i j) (buildxy i (+ j 1) nx ny))]))

;; plotimage: (listof Nat) (listof Pixel) Num Num -> image
;; consumes a list of indices and a list of Pixels (thecolors), and the image size nx,ny
;; and plots the image in indices using the elements of this list as indices into thecolors
  (define (plotimage indices thecolors nx ny)
    (plotimage-help indices (map convert-pixel-to-color thecolors) nx ny (buildxy 0 0 nx ny)))
  
;; plotimage-help: (listof Nat) (listof Pixel) Num Num (listof (list Nat Nat)) -> image
;; plots the image at locations (i,j) in xylocs
  (define (plotimage-help indices thecolors nx ny xylocs)
    (cond [(empty? indices) (empty-scene nx ny)]
          [else (place-image (square 1 "solid" (list-ref thecolors (first indices)))
                             (first (first xylocs))
                             (second (first xylocs))
                             (plotimage-help (rest indices) thecolors nx ny (rest xylocs)))
			     ]))
  

)	


