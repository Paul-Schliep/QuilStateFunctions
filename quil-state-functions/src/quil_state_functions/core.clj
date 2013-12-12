(ns quil-state-functions.core
	(:use [quil.core]
              [clojure.test]
	      [quil-state-functions.stateFunctions])
)

(defn drawBoard []
	(rect 0 0 300 300)
	(rect 300 0 300 300)
	(rect 600 0 300 300)
	(rect 0 300 300 300)
	(rect 300 300 300 300)
	(rect 600 300 300 300)
	(rect 0 600 300 300)
	(rect 300 600 300 300)
	(rect 600 600 300 300)
)

(defn setup []
	(smooth)
	(stroke-weight 5)
	(stroke 0 153 0)
	(fill 255 255 255)
	(drawBoard)
	(setup-state [:clicks 0 :squareOne -1 :squareTwo -1 :squareThree -1 :squareFour -1 :squareFive -1 :squareSix -1 :squareSeven -1 :squareEight -1 :squareNine -1 :xTurn true :testKeyOne 2 :testKeyTwo 5])
)

(defn findSquare[x y]
	(cond
	  (and (< y 300) (< x 300)) [0 0]
	  (and (< y 600) (< x 300)) [0 300]
	  (and (< y 900) (< x 300)) [0 600]
	  (and (< y 300) (< x 600)) [300 0]
	  (and (< y 600) (< x 600)) [300 300]
	  (and (< y 900) (< x 600)) [300 600]
	  (and (< y 300) (< x 900)) [600 0]
	  (and (< y 600) (< x 900)) [600 300]
	  (and (< y 900) (< x 900)) [600 600]
	)
)

(defn checkEndState []	
	(cond
	  (and (and (= (get-value :squareOne) 0) (= (get-value :squareTwo) 0)) (= (get-value :squareThree) 0)) (frame-rate 0)  
	  (and (and (= (get-value :squareOne) 1) (= (get-value :squareTwo) 1)) (= (get-value :squareThree) 1)) (frame-rate 0)
	  (and (and (= (get-value :squareFour) 0) (= (get-value :squareFive) 0)) (= (get-value :squareSix) 0)) (frame-rate 0)
	  (and (and (= (get-value :squareFour) 1) (= (get-value :squareFive) 1)) (= (get-value :squareSix) 1)) (frame-rate 0)
	  (and (and (= (get-value :squareSeven) 0) (= (get-value :squareEight) 0)) (= (get-value :squareNine) 0)) (frame-rate 0)
	  (and (and (= (get-value :squareSeven) 1) (= (get-value :squareEight) 1)) (= (get-value :squareNine) 1)) (frame-rate 0)
	  (and (and (= (get-value :squareOne) 0) (= (get-value :squareFour) 0)) (= (get-value :squareSeven) 0)) (frame-rate 0) 
	  (and (and (= (get-value :squareOne) 1) (= (get-value :squareFour) 1)) (= (get-value :squareSeven) 1)) (frame-rate 0)
	  (and (and (= (get-value :squareTwo) 0) (= (get-value :squareFive) 0)) (= (get-value :squareEight) 0)) (frame-rate 0)
	  (and (and (= (get-value :squareTwo) 1) (= (get-value :squareFive) 1)) (= (get-value :squareEight) 1)) (frame-rate 0)
	  (and (and (= (get-value :squareThree) 0) (= (get-value :squareSix) 0)) (= (get-value :squareNine) 0)) (frame-rate 0)
	  (and (and (= (get-value :squareThree) 1) (= (get-value :squareSix) 1)) (= (get-value :squareNine) 1)) (frame-rate 0)
	  (and (and (= (get-value :squareOne) 0) (= (get-value :squareFive) 0)) (= (get-value :squareNine) 0)) (frame-rate 0)
	  (and (and (= (get-value :squareOne) 1) (= (get-value :squareFive) 1)) (= (get-value :squareNine) 1)) (frame-rate 0)
	  (and (and (= (get-value :squareThree) 0) (= (get-value :squareFive) 0)) (= (get-value :squareSeven) 0)) (frame-rate 0)
	  (and (and (= (get-value :squareThree) 1) (= (get-value :squareFive) 1)) (= (get-value :squareSeven) 1)) (frame-rate 0)
	  ;;cats game is taken care of by the clicks system variable in player move
	)
)

(defn drawXHelper [x y]
	(cond 
	  (and (= x 0) (= y 0)) :squareOne
	  (and (= x 300) (= y 0)) :squareTwo
	  (and (= x 600) (= y 0)) :squareThree
	  (and (= x 0) (= y 300)) :squareFour
	  (and (= x 300) (= y 300)) :squareFive
	  (and (= x 600) (= y 300))  :squareSix
	  (and (= x 0) (= y 600)) :squareSeven
	  (and (= x 300) (= y 600)) :squareEight
  	  (and (= x 600) (= y 600)) :squareNine
	)
)

(defn drawOHelper[x y]
	(cond 
	  (and (= x 0) (= y 0)) :squareOne
	  (and (= x 300) (= y 0)) :squareTwo
	  (and (= x 600) (= y 0)) :squareThree
	  (and (= x 0) (= y 300)) :squareFour
	  (and (= x 300) (= y 300)) :squareFive
	  (and (= x 600) (= y 300)) :squareSix
	  (and (= x 0) (= y 600)) :squareSeven
	  (and (= x 300) (= y 600)) :squareEight
  	  (and (= x 600) (= y 600)) :squareNine
	)
)
	

(defn drawX []
	(let [x (first (findSquare (mouse-x) (mouse-y)))
	      y (second (findSquare (mouse-x) (mouse-y)))]
	(stroke-weight 5)
	(stroke 176 23 31)
	(if (= true (get-value :xTurn)) (do
		(if (= (get-value (drawXHelper x y)) -1) (do
			(line (+ x 75) (+ y 75) (+ x 225) (+ y 225))
			(line (+ x 225) (+ y 75) (+ x 75) (+ y 225))
			(update-state :clicks inc)
			(compare-and-update (drawXHelper x y) -1 1)
			(update-state :xTurn false)
			)
		)	
		)
	))
)

(defn drawO []
	(let [x (first (findSquare (mouse-x) (mouse-y)))
	      y (second (findSquare (mouse-x) (mouse-y)))]
	(stroke-weight 5)
	(stroke 0 0 238)
	(if (= false (get-value :xTurn)) (do
		(if (= (get-value (drawOHelper x y)) -1) (do 
			(ellipse (+ x 150) (+ y 150) 150 150)
			(update-state :clicks inc)
			(compare-and-update (drawOHelper x y) -1 0)
			(update-state :xTurn true)
			)
		)
		)
	))	
)


(defn player-move []
	(println (get-value :testKeyOne))
	(println (get-value :testKeyTwo))
	(swap-values :testKeyOne :testKeyTwo)
	(println (get-value :testKeyOne))
	(println (get-value :testKeyTwo))
	(if (= (mouse-button) :left) (drawX) (drawO) )
	(checkEndState)	
	(if (= (get-value :clicks) 9) (frame-rate 0))
)

(defsketch ticTacToe	
	:title "Tic-Tac-Toe"
	:setup setup
	:mouse-pressed player-move
	:size [900 900]
)

 
