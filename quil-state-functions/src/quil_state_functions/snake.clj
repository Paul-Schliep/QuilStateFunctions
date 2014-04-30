(ns quil-state-functions.snake
	(:use [quil.core]
        [clojure.test]
	      [quil-state-functions.stateFunctions])
)

(def states
	{:snake [450 750 450 770 450 790 450 810] :snakeHeadX 450 :snakeHeadY 750 :food [150 150] :snake-direction "north" :score 0}
)

(defn reset-game-state []
  (loop [coll states]
    (if (empty? coll) (get-value :snake) (do (update-state (first (first coll)) (second (first coll))) (recur (rest coll)))))
)

(defn sqr [x]
      (* x x)
)

(defn distance [x1 y1 x2 y2]
       (let [bigX (max x1 x2)
      	    bigY (max y1 y2)
	    smallX (min x1 x2)
	    smallY (min y1 y2)]
	    (sqrt (+ (sqr (- bigX smallX)) (sqr (- bigY smallY)))))
)

(defn overlap? [x1 y1 x2 y2 amount]
	    (> amount (distance x1 y1 x2 y2))
)

(defn inSnake? [coll x y]
      (loop [s coll]
      	    (if (empty? s) false
      	    	(if (overlap? (+ (first s) 10) (+ (second s) 10) x y (+ 10 (sqrt (+ (sqr 10) (sqr 10))))) true (recur (drop 2 s)))))
)

(defn inSnake2? [coll x y]
      (loop [s coll]
      	    (if (empty? s) false
      	    	(if (overlap? (+ (first s) 10) (+ (second s) 10) x y 19) true (recur (drop 2 s)))))
)

(defn outOfBounds? [x y]
      (not (and (and (< 0 x) (> 900 x)) (and (< 0 y) (> 900 y))))
)

(defn gameover? []
      (if (inSnake2? (drop 2 (get-value :snake)) (+ 10 (first (get-value :snake))) (+ 10 (second (get-value :snake))))
        true
        (outOfBounds? (first (get-value :snake)) (second (get-value :snake))))
)

(defn setup []
	(background-color "white")
	(frame-rate 10)
)

(defn update-snake-position []
	(cond
		(= (get-value :snake-direction) "north") (update-state :snakeHeadY (- (get-value :snakeHeadY) 20))
		(= (get-value :snake-direction) "south") (update-state :snakeHeadY (+ 20 (get-value :snakeHeadY)))
		(= (get-value :snake-direction) "east") (update-state :snakeHeadX (+ 20 (get-value :snakeHeadX)))
		(= (get-value :snake-direction) "west") (update-state :snakeHeadX (- (get-value :snakeHeadX) 20))
	)
)

(defn random-food-helper []
      [(+ 20 (rand-int 860)) (+ 20 (rand-int 860))]
)

(defn random-food [coll]
      (if (inSnake? (get-value :snake) (first coll) (second coll)) (random-food (random-food-helper)) coll)
)


(defn update-food [coll]
    (if (overlap? (+ 10 (get-value :snakeHeadX)) (+ 10 (get-value :snakeHeadY)) (first (get-value :food)) (second (get-value :food)) (+ 10 (sqrt (+ (sqr 10) (sqr 10)))))
      (random-food (random-food-helper))
	    coll)
)

(defn update-score [val]
  (if (overlap? (+ 10 (get-value :snakeHeadX)) (+ 10 (get-value :snakeHeadY)) (first (get-value :food)) (second (get-value :food)) (+ 10 (sqrt (+ (sqr 10) (sqr 10)))))
    (inc val)
    val)
)

(defn draw-food [food]
	(draw-circle (first food) (second food) 20 "yellow")
)

(defn draw-canvas []
	(background-color "white")
  (write-text (str "Score: " (get-value :score)) 15 400 100)
)


;; Split up into update-snake and draw-snake
(defn draw-snake [snake]
	(loop [s snake x 0]
		(if (= (/ (count snake) 2) x) ()
		(do (draw-rect (first s) (second s) 20 20 "green") (recur (drop 2 s) (inc x))))
	)
)

(defn update-snake [coll]
	(update-snake-position)
  (if (gameover?) (reset-game-state)
	(do (let  [x1 (get-value :snakeHeadX)
	       y1 (get-value :snakeHeadY)
	       x2 (first (get-value :food))
	       y2 (second (get-value :food))]
	(vec (cons (get-value :snakeHeadX) (cons (get-value :snakeHeadY) (if (overlap? (+ x1 10) (+ y1 10) x2 y2 (+ 10 (sqrt (+ (sqr 10) (sqr 10)))))
			coll
			(drop-last 2 coll))))))))
)

(defn controls []
	(cond
		(and (= (key-input) "UP-ARROW") (not (= "south" (get-value :snake-direction)))) (update-state :snake-direction "north")
		(and (= (key-input) "DOWN-ARROW") (not (= "north" (get-value :snake-direction)))) (update-state :snake-direction "south")
		(and (= (key-input) "RIGHT-ARROW") (not (= "west" (get-value :snake-direction)))) (update-state :snake-direction "east")
		(and (= (key-input) "LEFT-ARROW") (not (= "east" (get-value :snake-direction)))) (update-state :snake-direction "west")
	)
)

(def updates
	{:setup-drawing setup :snake update-snake :score update-score :food update-food}
)

(def display-order
	{:canvas draw-canvas :food draw-food :snake draw-snake}
)

;; Support code
(defn setup-sketch []
	(smooth)
	(setup-state (merge (merge (hash-map :display-order display-order) (merge (hash-map :updates updates) states)) (hash-map :states states)))
	((:setup-drawing (get-value :updates)))
)

(defn display []
	(loop [s (get-value :display-order)]
		(if (empty? s) ()
		(do  (let [key (first (first s))
	      	   fun (second (first s))] (if (contains? (get-value :states) key) (fun (get-value key)) (fun)))
      		   (recur (rest s)))))
)

(defn update []
      (loop [s (rest (get-value :updates))]
      	    (if (empty? s) ()
	    	(do (let [key (first (first s))
		     fun (second (first s))]
		     (update-state key (fun (get-value key))) (recur (rest s))))))
)

;;Should call update function then display function
(defn draw-sketch []
	(update)
	(display)
)

;; To be generated by macro
(defsketch snake
	:setup setup-sketch
	:draw draw-sketch
	:key-pressed controls
	:title "Hungry, Hungry, Snake"
	:size [900 900]
)

(defn -main [& args])
