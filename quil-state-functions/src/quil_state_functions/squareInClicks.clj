(ns quil-state-functions.squareInClicks
	(:use [quil.core]
              [clojure.test]
	      [quil-state-functions.stateFunctions])
)


(defn setup []
	(setup-state [:insideSquare 0 :outsideSquare 0])
	(smooth)
	(background 0 0 0)
)

(defn mouse []
	(let [x (mouse-x)
	      y (mouse-y)]
		(if (and (and (>= x 450) (<= x 650)) (and (>= y 450) (<= y 650))) 
			(update-state :insideSquare inc) 
			(update-state :outsideSquare inc)
		)  
	)
)

(defn draw []
	(background 0 0 0)
	(fill 255 0 0)
	(rect 450 450 200 200)
	(text-font (create-font "Utopia Italic" 20 true))
	(if (> (get-value :insideSquare) (get-value :outsideSquare))
		(text (str "Clicks Inside the Square: " (get-value :insideSquare)) 450 200)
		(text (str "Clicks Outside the Square: " (get-value :outsideSquare)) 450 200)
	)
)

(defsketch squareInClicks
	:title "Square in clicks"
	:setup setup
	:mouse-pressed mouse
	:size [900 900]
	:draw draw
)

(defn -main [& args]
	
)  
