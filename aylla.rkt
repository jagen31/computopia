#lang racket

(require 2htdp/image 2htdp/universe (for-syntax syntax/parse racket/syntax))

(define (eye color) (overlay (star 100 'outline 'yellow) (circle 100 'solid color)))

(define (penny color) (pen color 10 "solid" "round" "bevel"))
(define (penrose color) (make-pen color 50 "solid" "round" "round"))

(define (face face-shape eye-color1 eye-color2)
 (overlay (above
            (triangle 100 'solid 'purple)
            (rectangle 0 50 'solid 'transparent)
            (ellipse 100 25 'solid 'orange))
           (overlay/offset (beside (eye eye-color1) (eye eye-color2)) 0 89 face-shape)))

(define hat (wedge 200 180 'solid 'black))
(define dehat (overlay (wedge 190 180 'outline (penny 'black))
                       (wedge 200 180 'solid 'white)))

(define-syntax (define-alien stx)
  (syntax-parse stx
    [(_ name:id expr)
     #:with alien-says (format-id stx "~a-says" #'name)
     #'(begin
         (define name expr)
         (define (alien-says t) (above name (text t 32 'red))))]))

(define santa-hat (above (circle 40 'outline 'black) (triangle 300 'solid 'red)))

(define-alien aylla (above hat (face (rotate 180 (triangle 400 'solid 'yellow)) 'red 'green)))
(define-alien deedy (above hat (face (rectangle 400 400 'solid 'darkgreen) 'blue 'brown)))
(define-alien leeo (above hat (face (circle 200 'solid 'maroon) 'orange 'cyan)))
(define-alien derayy (above dehat (face (regular-polygon 150 10 'solid 'cyan) 'darkblue 'darkpink)))
(define-alien rayy (above hat (face (regular-polygon 150 10 'solid 'blue) 'cyan 'pink)))
(define-alien link (above hat (face (rectangle 200 400 'solid 'darkorange) 'pink 'pink)))
(define-alien nik (above hat (face (rectangle 500 300 'solid 'darkpurple) 'lightpurple 'pink)))
(define-alien sunyy (above hat (face (circle 200 'solid 'brown) 'darkblue 'cyan)))
(define-alien daydo (face (star 400 'solid 'cyan) 'cyan 'cyan))
(define-alien fleebledrop
  (above hat (face (overlay (rhombus 300 50 'solid 'pink) (rotate 90 (rhombus 300 50 'solid 'darkpink))) 'magenta 'black)))
(define-alien zeezee (face (overlay/align 'middle 'bottom (scale 3 (pulled-regular-polygon 180 3 1.8 30 "solid" "purple")) (rotate 60 (scale 3 (pulled-regular-polygon 200 3 1.8 30 "solid" "purple")))) 'brown 'cyan))

(define (overlay/grid picture x y picture2)
  (overlay/xy picture (* x -50) (* y -50) picture2))


(define-alien santa
  (above santa-hat
         (face (overlay/offset (overlay (rotate 40 (wedge 220 100 'outline 'black))
                                        (rotate 40 (wedge 220 100 'solid 'white)))
                               0 -175
                               (regular-polygon 140 10 'solid 'lightpink)) 'white 'white)))

(define bunny-ear (overlay (ellipse 60 180 'solid 'red) (ellipse 75 200 'solid 'pink)))
(define bunny-ears (beside bunny-ear (line 50 1 'transparent) bunny-ear))

(define-alien bunny (above bunny-ears (face (ellipse 300 450 'solid 'pink) 'cyan 'cyan)))

(define-alien daddy
  (overlay/offset
   (rotate 180 (triangle 100 'solid 'darkred))
   0 -130
   (overlay/offset (rectangle 100 30 'solid 'darkred) 0 -35 (face (ellipse 350 570 'solid 'red) 'blue 'blue))))

(define-alien connor
  (underlay/offset
   (face (above (rectangle 300 150 'solid 'red)
                (flip-vertical (triangle 300 'solid 'red)))
         'lightturquoise
         'lightturquoise)
   0 -200 (rectangle 290 80 'solid 'lightgoldenrod)))

(define-alien mara
  (overlay/offset
   (face (above (rectangle 300 150 'solid 'red)
                (flip-vertical (triangle 300 'solid 'red)))
         'turquoise
         'turquoise)
   0 -150 (rotate -45 (wedge 200 270 'solid 'lightgoldenrod))))

(define-alien mommy
  (overlay (face (ellipse 450 300 'solid 'red) 'lightblue 'lightblue) (rotate -45 (wedge 300 270 'solid 'brown))))

(define a aylla)
(define b bunny)
(define l link)
(define s santa)
(define d daddy)
(define e deedy)
(define de deedy)
(define dee deedy)
(define m mommy)
(define r rayy)
(define fleeb fleebledrop)
(define fleeb-mom (face (rotate 90 (rhombus 400 50 'solid 'darkpink)) 'magenta 'black))
(define fleeb-dad (face (rhombus 400 50 'solid 'pink) 'magenta 'black))

(define (house color)
  (overlay/align 'middle 'bottom
                 (overlay (rectangle 120 200 'outline (penny 'black)) (rectangle 120 200 'solid 'brown))
                 (above (overlay (triangle 400 'outline (penny 'black)) (triangle 400 'solid color))
                        (overlay (rectangle 400 400 'outline (penny 'black)) (rectangle 400 400 'solid color)))))

(define (named-house name color)
  (above (text name 140 'black) (house color)))

(define (in-house alien color) (overlay/align 'middle 'bottom alien (house color)))

(define mappy
  (overlay (rectangle 200 1000 'solid 'blue) (rectangle 1000 1000 'solid  'green)))

(define grid
  (for*/fold ([acc empty-image])
             ([x (in-range 20)] [y (in-range 20)])
    (overlay/grid (overlay (text (format "~s,~s" x y) 12 'brown) (rectangle 50 50 'outline 'brown)) x y acc)))

(define mappy-grid (overlay/align 'left 'top grid mappy))

(define-syntax (put-on-mappy stx)
  (syntax-parse stx
    [(_ [x* y* expr*] ... {~optional {~and no-grid {~datum no-grid}}})
     (for/fold ([acc (if (attribute no-grid) #'mappy #'mappy-grid)])
               ([x (syntax->list #'(x* ...))] [y (syntax->list #'(y* ...))] [expr (syntax->list #'(expr* ...))])
         #`(overlay/grid #,expr #,x #,y #,acc))]))

(define mattress (above (line 25 25 (penny 'brown)) (line -25 25 (penny 'brown)) (line 25 25 (penny 'brown)) (line -25 25 (penny 'brown))))
(define mattress-canyon (let () (define m (rotate 90 mattress)) (beside m m m)))

(define north-pole (overlay (rectangle 150 150 'outline (penny 'red)) (circle 13 'solid 'green) (rectangle 150 150 'solid 'white) ))
(define easterville (overlay (ellipse 35 50 'solid 'lightpurple) (rotate 45 (overlay  (rectangle 150 150 'outline (penny 'cyan)) (rectangle 150 150 'solid 'lightyellow)))))

(define (building stories)
    (overlay
     (rectangle 350 (+ 50 (* 100 stories)) 'outline (penny 'black))
     (for*/fold ([acc (rectangle 350 (+ 50 (* 100 stories)) 'solid 'grey)])
                ([i (in-range 3)] [j (in-range stories)])
       (overlay/grid (rectangle 50 50 'solid 'yellow) (add1 (* i 2)) (add1 (* j 2)) acc))))
(define flop-city
    (overlay/align 'right 'bottom (rotate 80 (building 9))
                 (overlay/align 'right 'bottom (rotate -30 (building 7))
                                (beside/align 'bottom (building 8) (building 3) (building 5)))))

(define pond (rectangle 800 800 'solid 'blue))

(define computopia
  (put-on-mappy 
   [3 13 (scale 1/10 rayy)]
   [5 13 (rotate 180 (scale 1/10 derayy))]
   [4 13 (scale 1/10 (named-house "rayy &\nderayy" 'red))]
   [11 19 (scale 1/10 aylla)]
   [12 18(scale 1/10 (named-house "aylla" 'red))]
   [13 1 mattress-canyon]
   [15 1 (text "Mattress Canyon" 24 'white)]
   [13 0 (scale 1/10 daydo)]
   [14 0 (scale 1/10 (named-house "daydo" 'cyan))]
   [16 8 (scale 1/10 nik)]
   [15 8 (scale 1/10 (named-house "nik" 'red))]
   [2 1 north-pole]
   [2 3 (text "North Pole" 24 'green)]
   [2 1 (scale 1/10 santa)]
   [4 1 (scale 1/10 (named-house "santa" 'green))]
   [18 14 (scale 1/10 deedy)]
   [19 13 (scale 1/10 (named-house "deedy" 'red))]
   [0 7 easterville]
   [0 10 (text "Easterville" 24 'black)]
   [1 8 (scale 1/10 bunny)]
   [2 7 (scale 1/10 (named-house "bunny" 'cyan))]
   [7 6 (scale 1/10 (named-house "link" 'red))]
   [6 7 (scale 1/10 link)]
   [14 15 (scale 1/5 flop-city)]
   [14 18 (text "Flop CITY" 40 'purple)]
   [17 15 (rotate -30 (scale 1/10 connor))]
   [14 17 (rotate 80 (scale 1/10 mara))]))