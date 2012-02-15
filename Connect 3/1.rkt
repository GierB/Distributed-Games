#lang class1
;;By: Brandon Gier and Mark Mayers
;;Contains code to play Connect 3 (connect 4 on a 4x4 board where you need 3 in a row to win)
;;Because we don't tolerate trickery or people trying to move in columns not possible,
;;If you try to place a piece in a full row you lose your turn.
(require 2htdp/image)
(require class1/universe)


;;Constants for the game
(define CELLSWIDE 4)
(define CELLSHIGH 4)
(define CELLWIDTH 50)
(define RADIUS (/ CELLWIDTH 2))

(define BASESCENE (overlay (rectangle (+ 5 (* CELLSWIDE CELLWIDTH))
                                      (+ 5 (* CELLSHIGH CELLWIDTH))
                                      "solid" "yellow")
                           (empty-scene (* CELLSWIDE CELLWIDTH)
                                        (* CELLSHIGH CELLWIDTH))))


;;A Player is a: (new player% board)
;;Represents a player playing a game of connect 3
(define-class player%
  (fields board)
  
  ;;Prevents constant refresh as a guard against seizures
  (define/public (tick-rate) 60)
  
  ;;Connects to a specific server
  (define/public (register) LOCALHOST)
  
  (check-expect (send (new player% EMPTY-BOARD) to-draw)
                (foldr (λ(x scn)
                         (foldr (λ(y x2) 
                                  (place-image 
                                   (circle RADIUS "solid" "white")
                                   (- (* CELLWIDTH x) (- (/ CELLWIDTH 2) 2))
                                   (+ (* CELLWIDTH (- CELLSHIGH y)) (- (/ CELLWIDTH 2) 2))
                                   x2))
                                scn
                                (build-list 4 add1)))
                       BASESCENE
                       (build-list 4 add1)))
  ;;to-draw: -> Scene
  ;;Draws the current game on a scene
  (define/public (to-draw)
    (local [(define listRow (build-list 4 add1))]
    (foldr (λ(x scn)
             (foldr (λ(y x2) 
                      (place-image 
                       (circle RADIUS "solid" (send (field board) piece-at x y))
                       (- (* CELLWIDTH x) (- (/ CELLWIDTH 2) 2))
                       (+ (* CELLWIDTH (- CELLSHIGH y)) (- (/ CELLWIDTH 2) 2))
                       x2))
                    scn
                    listRow))
           BASESCENE
           (build-list 4 add1))))
  
  ;;stop-when: -> Player%
  ;;Stops when the game has been won
  (define/public (stop-when)
    (send (field board) win?))
  
  (check-expect (send (new player% EMPTY-BOARD) on-mouse 4 4 "left")
                (new player% EMPTY-BOARD))
  (check-expect (send (new player% EMPTY-BOARD) on-mouse 4 4 "button-down")
                (make-package (new player% EMPTY-BOARD) 1))
  ;;on-mouse: MouseEvent Number Number -> Message
  ;;Sends a message when the player clicks the mouse
  (define/public (on-mouse x y event)
    (cond [(string=? event "button-down")
           (make-package this
                         (ceiling (/ x CELLWIDTH)))]
          [else this]))
  
  ;;on-receive: Bundle -> Player
  ;;Creates a new player when a message is received
  (define/public (on-receive msg)
    (new player% (new board% msg)))
  )


;;A space is one of:
;; -"red"
;; -"black"
;; -"white" (represents empty space)

;;A Board is a (new board% [ListOf [ListOf Spaces]])
;;Represents a board and current player spots occupied
;;The first object of the first list is interpreted as the bottom left space in a board
;;Coordinates are counted as 1 1 being the first possible space
(define-class board%
  (fields spaces)
  
  
  (check-expect (send EMPTY-BOARD col-full? 1) false)
  (check-expect (send (new board% (list (list "red" "green")
                                        (list "blue" "yellow")))
                       col-full? 1) true)
  ;;col-full?: Number -> Boolean
  ;;Returns true if a column is full (the last space isn't empty)
  (define/public (col-full? col)
    (not (string=? "white" (send this piece-at col (length (first (field spaces)))))))
  
  
  (check-expect (send EMPTY-BOARD place 1 "red")
                (new board% (list (list "red" "white" "white" "white")
                                  (list "white" "white" "white" "white")
                                  (list "white" "white" "white" "white")
                                  (list "white" "white" "white" "white"))))
  (check-expect (send EMPTY-BOARD place 2 "black")
                (new board% (list (list "white" "white" "white" "white")
                                  (list "black" "white" "white" "white")
                                  (list "white" "white" "white" "white")
                                  (list "white" "white" "white" "white"))))
  (check-expect (send (new board% (list (list "red" "white" "white")
                                        (list "white" "white" "white")
                                        (list "white" "White" "white"))) place 1 "black")
                (new board% (list (list "red" "black" "white")
                                  (list "white" "white" "white")
                                  (list "white" "White" "white"))))
  ;;place: Number Space -> Board
  ;;Places a given piece at the next empty spot in a column, if possible
  ;;If this is not possible returns the game in the same state.
  (define/public (place col space)
    (local [(define (parse-build list x)
              (cond [(not (cons? list)) empty]
                    [(<= x 1) (cons (replace (car list)) (cdr list))]
                    [else (cons (car list) (parse-build (rest list) (sub1 x)))]))
            (define (replace lis)
              (cond [(empty? lis) empty]
                    [(string=? (car lis) "white")
                     (cons space (cdr lis))]
                    [else (cons (car lis) (replace (cdr lis)))]))]
      (cond [(col-full? col) this]
            [else (new board% (parse-build (field spaces) col))])))
  
  (check-expect (send EMPTY-BOARD piece-at 1 1) "white")
  (check-expect (send EMPTY-BOARD piece-at 50 50) "false")
  ;;piece-at: Number Number -> [Space or Boolean]
  ;;Returns the given space (color of piece) on a given board
  (define/public (piece-at x y)
    (local [(define (parse list x)
              (cond [(not (cons? list)) "false"]
                    [(<= x 1) (car list)]
                    [else (parse (rest list) (sub1 x))]))]
      (parse (parse (field spaces) x) y)))
  
  (check-expect (send EMPTY-BOARD win?) false)
  (check-expect (send (new board% (list (list "red" "white" "white" "white")
                                        (list "black" "red" "white" "white")
                                        (list "black" "blac" "red" "white")
                                        (list "white" "white" "white" "white"))) win?)
                true)
  (check-expect (send (new board% (list (list "black" "blac" "red" "white")   
                                        (list "black" "red" "white" "white")
                                        (list "red" "white" "white" "white")
                                        (list "white" "white" "white" "white"))) win?)
                true)
  (check-expect (send (new board% (list (list "black" "black" "black" "white")
                                        (list "white" "white" "white" "red")
                                        (list "white" "white" "white" "white")
                                        (list "white" "white" "white" "red"))) win?)
                true)
  (check-expect (send (new board% (list (list "black" "white" "white" "white")
                                        (list "black" "white" "white" "white")
                                        (list "black" "white" "white" "white")
                                        (list "white" "white" "white" "white"))) win?)
                true)
  ;;win?: -> Boolean
  ;;Returns true if any space has won
  ;;This tests for victory by: Vertical, horizontal or diagonal
  (define/public (win?)
    (local [(define (listmake-Diag x y sofar xmod ymod)
              (cond[(and (and (> x 0) (> y 0))
                         (and (< x 5) (< y 5)))
                     (listmake-Diag (xmod x) (ymod y) (cons (send this piece-at x y) sofar) xmod ymod)]
                    [else sofar]))
            (define (listRow number list)
              (cond [(empty? list) empty]
                    [else (cons (send (new board% list) piece-at 1 number)
                                (listRow number (cdr list)))]))]
      (ormap (λ(x) (or (rowwin x "red" 3)
                       (rowwin x "black" 3)))
             (append 
              (field spaces)
              (append 
               (build-list 4 (λ(x) (listRow (add1 x) (field spaces))))
               (append (append 
                        (build-list 4 (λ(x) (listmake-Diag x 1 empty add1 add1))) ;;Ascending Diagonals
                        (build-list 4 (λ(y) (listmake-Diag 1 y empty add1 add1)))) ;;Ascending that start above (1, 1)
                       (append 
                        (build-list 4 (λ(x) (listmake-Diag x 4 empty add1 sub1))) ;;Descending Diagonals
                        (build-list 4 (λ(y) (listmake-Diag 1 y empty add1 sub1)))))))))))


;;A Universe is a: (new universe% IWorld IWorld Board IWorld)
;;Represents a given universe in a boardgame
;;Interpretation: The 2 players are stored, along with the current board.
;;The turn should be equal to the value of one of the 2 players
(define-class universe%
  (fields p1 p2 board turn)
  
  ;;on-new: IWorld -> Bundle
  ;;Handles new connections
  (define/public (on-new con)
    (cond [(empty? (field p1)) 
           (make-bundle (new universe% con (field p2) (field board) con)
                        (list (make-mail con (send (field board) spaces)))
                        empty)]
          [(empty? (field p2))
           (make-bundle (new universe% (field p1) con (field board) (field turn)) 
                        (list (make-mail con (send (field board) spaces)))
                        empty)]
          [else
           (make-bundle this empty (list con))]))
  
  ;;on-msg: IWorld Package -> Bundle
  (define/public (on-msg iw msg)
    (cond [(iworld=? iw (field turn))
           (if (iworld=? iw (field p1))
               (make-bundle 
                (new universe% (field p1) (field p2)
                     (send (field board) place msg "red") (field p2))
                (list (make-mail iw (send (send (field board) place msg "red") spaces))
                      (make-mail (field p2) (send (send (field board) place msg "red") spaces)))
                empty)
               (make-bundle 
                (new universe% (field p1) (field p2)
                     (send (field board) place msg "black") (field p1))
                (list (make-mail iw (send (send (field board) place msg "black") spaces))
                      (make-mail (field p1) (send (send (field board) place msg "black") spaces)))
                empty))]
          [else (make-bundle this
                             (list (make-mail iw (send (field board) spaces)))
                                   empty)])))

;;rowwin?: List Space Number -> Boolean
;;Returns true if the list contains a specified number of a kind in a row
(define (rowwin list space num)
  (cond [(= 0 num) true]
        [(empty? list) false]
        [(string=? (car list) space)
         (rowwin (cdr list) space (sub1 num))]
        [else (rowwin (cdr list) space 3)]))
(check-expect (rowwin (list "white" "white" "white") "red" 3)
              false)
(check-expect (rowwin (list "red" "red" "red") "red" 3) true)
(check-expect (rowwin (list "red" "white" "red") "red" 2) false)


(define EMPTY-BOARD (new board% '(("white" "white" "white" "white")
                                  ("white" "white" "white" "white")
                                  ("white" "white" "white" "white")
                                  ("white" "white" "white" "white"))))



(launch-many-worlds
 (universe (new universe% empty empty EMPTY-BOARD empty))
 (big-bang (new player% EMPTY-BOARD))
 (big-bang (new player% EMPTY-BOARD)))