#lang class1
;; Author: Brandon Gier and Mark Mayers
;;Date: 1/31/11
;;Creates the lightcycles game in Racket
(require class1/universe)
(require 2htdp/image)

;;Cell System set-up:
(define CELL-WIDTH 30)
(define CELL-HEIGHT 30)
(define CELL-SIZE 10)
(define HEIGHT (* CELL-WIDTH CELL-SIZE))
(define WIDTH (* CELL-HEIGHT CELL-SIZE))

;;A Direction is one of:
;; - "up"
;; - "down"
;; - "left"
;; - "right"


;;A world is a (new world% [ListOf Bikes])
(define-class world%
  (fields bike1 bike2)
  
    ;;Sets the tick rate to be a reasonable speed for gameplay
  (define/public (tick-rate) 1/3)
  
  ;;Connects to a specified server. LOCALHOST by default,
  ;;However this won't work unless you also run the server
  (define/public (register) LOCALHOST)
  
  
  ;;to-draw: -> Scene
  ;;Renders the game on a scene
  (define/public (to-draw)
    (local [(define image (send (field bike2) draw 
                                (send (field bike1) draw
                                      (empty-scene HEIGHT WIDTH))))]
      (cond[(send (field bike1) collide? (field bike2))
            (overlay (text "YOU LOSE!" 20 "black") image)]
           [(send (field bike2) collide? (field bike1))
            (overlay (text "YOU WIN!" 20 "black") image)]
           [else image])))
  
  ;;on-recieve: Package -> World%
  ;;Converts a message to a world
  (define/public (on-receive m)
    (new world% 
         (new bike% 
              (first (first m))
              (first (rest (first m)))
              (first (rest (rest (first m))))
              (first (rest (rest (rest (first m))))))
         (new bike%
              (first (rest m))
              (first (rest (rest m)))
              (first (rest (rest (rest m))))
              (first (rest (rest (rest (rest m))))))))
  
  
  ;;onkey: String -> world%
  ;;Handles key inputs in a world
  (define/public (on-key key)
    (cond [(string=? "up" key) (new world% 
                                    (send (field bike1) change-dir key) 
                                    (field bike2))]
          [(string=? "down" key) (new world%  
                                      (send (field bike1) change-dir key) 
                                      (field bike2))]
          [(string=? "left" key) (new world% 
                                      (send (field bike1) change-dir key) 
                                      (field bike2))]
          [(string=? "right" key) (new world% 
                                       (send (field bike1) change-dir key) 
                                       (field bike2))]
          [else this]))
  
  
  ;;on-tick:  -> Package
  ;;Sends a bundle containing data for movement processing when it ticks
  (define/public (on-tick)
    (make-package this (send (field bike1) dir)))
  
  ;;Stop-when: -> Boolean
  ;;Stops when either bike has lost
  (define/public (stop-when)
    (or (send (field bike1) collide? (field bike2))
        (send (field bike2) collide? (field bike1)))))

;;A Bike is a (new bike% ComplexNumber Color [Listof Complex-Numbers] Direction)
(define-class bike%
  (fields loc col trail dir)
  
  ;;Draw: Scene -> Scene
  ;;Draws the current bike on a given scene
  (define/public (draw scn)
    (foldr (lambda (obj scn) 
             (place-image 
              (rectangle CELL-SIZE CELL-SIZE "solid" (field col))
              (+ (/ CELL-SIZE 2) 
                 (* CELL-SIZE (real-part obj))) 
              (+ (/ CELL-SIZE 2) 
                 (* CELL-SIZE (imag-part obj)))
              scn))
           (place-image 
            (rectangle CELL-SIZE CELL-SIZE "solid" (field col)) 
            (+ (/ CELL-SIZE 2) 
               (* CELL-SIZE (real-part (field loc)))) 
            (+ (/ CELL-SIZE 2) 
               (* CELL-SIZE (imag-part (field loc))))
            scn)
           (field trail)))
  
  ;;Move: -> Bike
  ;;Moves the bike one space
  (define/public (move)
    (cond [(string=? (field dir) "left") (new bike% (+ (field loc) -1+0i) 
                                            (field col)
                                            (cons (field loc) (field trail))
                                            (field dir))]
          [(string=? (field dir) "right") (new bike% (+ (field loc) 1+0i)
                                              (field col)
                                              (cons (field loc) (field trail))
                                              (field dir))]
          [(string=? (field dir) "down") (new bike% (+ (field loc) 0+1i)
                                               (field col)
                                               (cons (field loc) (field trail))
                                               (field dir))]
          [else (new bike% (+ (field loc) 0-1i) 
                     (field col)
                     (cons (field loc) (field trail))
                     (field dir))]))
  
  
  ;;change-dir Direction -> Bike
  ;;Changes the direction of a bike
  (define/public (change-dir dir)
    (new bike% (field loc) (field col) (field trail) dir))
  
  ;;collide?: Bike -> Boolean
  ;;Determines if this bike has collided with itself or another
  (define/public (collide? bike)
    (cond[(member? (field loc) (send bike trail)) true]
         [(member? (field loc) (field trail)) true]
         [(= (field loc) (send bike loc)) true]
         [(or (>= (real-part (field loc)) CELL-WIDTH)
              (< (real-part (field loc)) 0)) true]
         [(or (>= (imag-part (field loc)) CELL-HEIGHT)
              (< (imag-part (field loc)) 0)) true]
         [else false])))


;;Universe code for running server!
(define-class universe%
  (fields p1 p2 b1 b2)
  
  ;;on-new: World -> Bundle
  ;;Handles incoming connections on a server
  (define/public (on-new con)
    (make-bundle (cond
                   [(empty? (field p1))
                    (new universe% con (field p2) 
                         BIKE1DEFAULT
                         BIKE2DEFAULT)]
                   [(empty? (field p2))
                    (new universe% (field p1) con 
                         BIKE1DEFAULT
                         BIKE2DEFAULT)]
                   [else this])
                 empty empty))
  
  ;;on-msg World, Sexpr -> Bundle
  ;;Procceses and handles incoming data
  (define/public (on-msg iw m)
    (if (or (empty? (field p1)) (empty? (field p2)))
        (make-bundle this empty empty)
        (make-bundle
         (new universe% (field p1) (field p2)
              (if (iworld=? iw (field p1))
                  (send (send (field b1) change-dir m) move)
                  (field b1))
              (if (iworld=? iw (field p2))
                  (send (send (field b2) change-dir m) move)
                  (field b2)))
         (list (make-mail 
                iw
                (if (iworld=? iw (field p1))
                    (cons (send this extract (field b1) m) 
                          (send this extract (field b2) (send (field b2) dir)))
                    (cons (send this extract (field b2) m)
                          (send this extract (field b1) (send (field b1) dir))))))
         empty)))
  
  ;;extract: Bike Direction -> [ListOf SExpr]
  ;;Returns a list of all data in the bikes in the game
  (define/public (extract bike dir)
    (list (send bike loc)
          (send bike col)
          (send bike trail)
          dir)))


;;Tests for the World% class
(define TESTWORLD (new world%
                       (new bike% 1+1i "blue" empty "up")
                       (new bike% 2+2i "orange" empty "up")))

(check-expect (send TESTWORLD to-draw)
              (place-image (rectangle CELL-SIZE CELL-SIZE "solid" "blue")
                           (* CELL-SIZE (/ 3 2)) (* CELL-SIZE (/ 3 2))
                           (place-image (rectangle CELL-SIZE CELL-SIZE "solid" "orange")
                                        (* CELL-SIZE (+ 2 (/ 1 2))) (* CELL-SIZE (+ 2 (/ 1 2)))
                                        (empty-scene WIDTH HEIGHT))))

(check-expect (send TESTWORLD on-receive (cons (list 0 "green" empty "up")
                                               (list 3 "blue" empty "down")))
              (new world% 
                   (new bike% 0 "green" empty "up")
                   (new bike% 3 "blue" empty '"down")))
(check-expect (send TESTWORLD on-key "left")
              (new world% (new bike% 1+1i "blue" empty "left")
                   (new bike% 2+2i "orange" empty "up")))
(check-expect (send TESTWORLD on-key "up")
              (new world% (new bike% 1+1i "blue" empty "up")
                   (new bike% 2+2i "orange" empty "up")))
(check-expect (send TESTWORLD on-key "a")
              (new world% (new bike% 1+1i "blue" empty "up")
                   (new bike% 2+2i "orange" empty "up")))


;;Default starting positions for the bikes
;;Also used in code for tests of bikes
(define BIKE1DEFAULT (new bike% 5+5i "blue" empty "right"))
(define BIKE2DEFAULT (new bike% 15+15i "orange" empty "left"))

(check-expect (send BIKE1DEFAULT draw (empty-scene WIDTH HEIGHT))
              (place-image (rectangle CELL-SIZE CELL-SIZE "solid" "blue")
                           (+ (* CELL-SIZE 5) (/ CELL-SIZE 2)) 
                           (+ (* CELL-SIZE 5) (/ CELL-SIZE 2))
                           (empty-scene WIDTH HEIGHT)))
(check-expect (send BIKE1DEFAULT move)
              (new bike% 6+5i "blue" (list 5+5i) "right"))
(check-expect (send BIKE1DEFAULT change-dir "up")
              (new bike% 5+5i "blue" empty "up"))
(check-expect (send BIKE1DEFAULT collide? BIKE2DEFAULT) false)
(check-expect (send BIKE1DEFAULT collide? (new bike% 5+5i "orange" empty "up")) true)
(check-expect (send BIKE1DEFAULT collide? (new bike% 1+1i "green" (list 5+5i) "up")) true)


;;Universe Tests
(define con1 (new world% BIKE1DEFAULT BIKE2DEFAULT))
(define con2 (new world% BIKE2DEFAULT BIKE1DEFAULT))

(define univ1 (new universe% empty empty empty empty))
(define univ2 (new universe% con1 empty BIKE1DEFAULT BIKE2DEFAULT))
(define univ3 (new universe% con1 con2 BIKE1DEFAULT BIKE2DEFAULT))

(check-expect (send univ1 on-new con1)
              (make-bundle univ2 empty empty))
(check-expect (send univ2 on-new con2)
              (make-bundle univ3 empty empty))
(check-expect (send univ3 on-new con1)
              (make-bundle univ3 empty empty))
(check-expect (send univ1 on-msg con1 "hi")
              (make-bundle univ1 empty empty))
(check-expect (send univ3 extract BIKE1DEFAULT "up")
              (list 5+5i "blue" empty "up"))


;;Launches a test server and 2 clients for running the game.
(launch-many-worlds 
 (universe (new universe% empty empty BIKE1DEFAULT BIKE2DEFAULT))
 (big-bang (new world% BIKE1DEFAULT BIKE2DEFAULT))
 (big-bang (new world% BIKE2DEFAULT BIKE1DEFAULT)))