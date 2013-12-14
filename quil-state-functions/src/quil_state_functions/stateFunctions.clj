(ns quil-state-functions.stateFunctions
	(:use [clojure.test]
              [quil.core])
	;(:require [quil-state-functions.snake.updates :as updates]
	;	  [quil-state-functions.snake.states :as states])
)

(defn wrap-var [kv]
	(let [key (first kv) 
	      val (second kv)]
	(ref val))	
)

(defn setup-state [coll]
	;(if (map? coll) (apply set-state! (vec (interleave (map (fn [key var] key) coll) (map (fn [key var] (if (fn? var) var (ref var))) coll)))))
	(apply set-state! (interleave (keys coll) (map wrap-var coll)))
)

(defn update-state [key set]
	(dosync (if (function? set) (alter (state key) set) (ref-set (state key) set))) 
)

(defn get-value [key]
	@(state key)
)

(defn compare-and-update [key check set]
	(dosync 
		(when (= check (get-value key))
			(ref-set (state key) set)
		)
	)
)

(defn swap-values [key1 key2]
	(dosync
		(let [val1 (get-value key1)
		     val2 (get-value key2)]
		     (ref-set (state key1) val2)
		     (ref-set (state key2) val1)
		)
	)
)

;; Easier Color Usage
;; red, blue, yellow, green, purple, orange, pink, black, white, gray, cyan, magenta, maroon, navy, lime, silver, gold
;; transparent colors too

(defn fill-color [str & [transparency]]
	(if (= transparency nil) (do (let [transparency 1000] 
		(cond
	 	 ;; Base colors
	  	 (= str "red") (fill 255 0 0 transparency)
	  	 (= str "blue") (fill 0 0 255 transparency)
	  	 (= str "yellow") (fill 255 255 0 transparency)
	  	 (= str "green") (fill 0 128 0 transparency)
	  	 (= str "purple") (fill 128 0 128 transparency)
	  	 (= str "orange") (fill 255 165 0 transparency)
	  	 (= str "pink") (fill 255 192 203 transparency)
	  	 (= str "black") (fill 0 0 0 transparency)
		 (= str "brown") (fill 165 42 42 transparency)
	  	 (= str "white") (fill 255 255 255 transparency)
	  	 (= str "gray") (fill 128 128 128 transparency)
	  	 (= str "grey") (fill 128 128 128 transparency)
	  	 (= str "silver") (fill 192 192 192 transparency)
	  	 (= str "gold") (fill 255 215 0 transparency)
	  	 (= str "cyan") (fill 0 255 255 transparency)
	  	 (= str "magenta") (fill 255 0 255 transparency)
	  	 (= str "maroon") (fill 128 0 0 transparency)
	 	 (= str "navy") (fill 0 0 128 transparency)
	  	 (= str "lime") (fill 0 255 0 transparency)
	  	 (= str "teal") (fill 0 128 128 transparency)
		 (= str "random-color") (fill (random 255) (random 255) (random 255) transparency)
		)))
		(cond
	 	 (= str "red") (fill 255 0 0 transparency)
	 	 (= str "blue") (fill 0 0 255 transparency)
	 	 (= str "yellow") (fill 255 255 0 transparency)
	 	 (= str "green") (fill 0 128 0 transparency)
	 	 (= str "purple") (fill 128 0 128 transparency)
	 	 (= str "orange") (fill 255 165 0 transparency)
	 	 (= str "pink") (fill 255 192 203 transparency)
	 	 (= str "black") (fill 0 0 0 transparency)
		 (= str "brown") (fill 165 42 42 transparency)
	 	 (= str "white") (fill 255 255 255 transparency)
	 	 (= str "gray") (fill 128 128 128 transparency)
	  	 (= str "grey") (fill 128 128 128 transparency)
	 	 (= str "silver") (fill 192 192 192 transparency)
	 	 (= str "gold") (fill 255 215 0 transparency)
	  	 (= str "cyan") (fill 0 255 255 transparency)
	  	 (= str "magenta") (fill 255 0 255 transparency)
	 	 (= str "maroon") (fill 128 0 0 transparency)
	 	 (= str "navy") (fill 0 0 128 transparency)
	  	 (= str "lime") (fill 0 255 0 transparency)
	  	 (= str "teal") (fill 0 128 128 transparency)
		 (= str "random-color") (fill (random 255) (random 255) (random 255) transparency)
		)
	)
)

(defn background-color [str & [transparency]]
	(if (= transparency nil) (do (let [transparency 1000] 
		(cond
	 	 ;; Base colors
	  	 (= str "red") (background 255 0 0 transparency)
	  	 (= str "blue") (background 0 0 255 transparency)
	  	 (= str "yellow") (background 255 255 0 transparency)
	  	 (= str "green") (background 0 128 0 transparency)
	  	 (= str "purple") (background 128 0 128 transparency)
	  	 (= str "orange") (background 255 165 0 transparency)
	  	 (= str "pink") (background 255 192 203 transparency)
	  	 (= str "black") (background 0 0 0 transparency)
		 (= str "brown") (background 165 42 42 transparency)
	  	 (= str "white") (background 255 255 255 transparency)
	  	 (= str "gray") (background 128 128 128 transparency)
	  	 (= str "grey") (background 128 128 128 transparency)
	  	 (= str "silver") (background 192 192 192 transparency)
	  	 (= str "gold") (background 255 215 0 transparency)
	  	 (= str "cyan") (background 0 255 255 transparency)
	  	 (= str "magenta") (background 255 0 255 transparency)
	  	 (= str "maroon") (background 128 0 0 transparency)
	 	 (= str "navy") (background 0 0 128 transparency)
	  	 (= str "lime") (background 0 255 0 transparency)
	  	 (= str "teal") (background 0 128 128 transparency)
		 (= str "random-color") (background (random 255) (random 255) (random 255) transparency)
		)))
		(cond
	 	 (= str "red") (background 255 0 0 transparency)
	 	 (= str "blue") (background 0 0 255 transparency)
	 	 (= str "yellow") (background 255 255 0 transparency)
	 	 (= str "green") (background 0 128 0 transparency)
	 	 (= str "purple") (background 128 0 128 transparency)
	 	 (= str "orange") (background 255 165 0 transparency)
	 	 (= str "pink") (background 255 192 203 transparency)
	 	 (= str "black") (background 0 0 0 transparency)
		 (= str "brown") (background 165 42 42 transparency)
	 	 (= str "white") (background 255 255 255 transparency)
	 	 (= str "gray") (background 128 128 128 transparency)
	  	 (= str "grey") (background 128 128 128 transparency)
	 	 (= str "silver") (background 192 192 192 transparency)
	 	 (= str "gold") (background 255 215 0 transparency)
	  	 (= str "cyan") (background 0 255 255 transparency)
	  	 (= str "magenta") (background 255 0 255 transparency)
	 	 (= str "maroon") (background 128 0 0 transparency)
	 	 (= str "navy") (background 0 0 128 transparency)
	  	 (= str "lime") (background 0 255 0 transparency)
	  	 (= str "teal") (background 0 128 128 transparency)
		 (= str "random-color") (background (random 255) (random 255) (random 255) transparency)
		)
	)
)

(defn stroke-color [str & [transparency]]
	(if (= transparency nil) (do (let [transparency 1000] 
		(cond
	 	 ;; Base colors
	  	 (= str "red") (stroke 255 0 0 transparency)
	  	 (= str "blue") (stroke 0 0 255 transparency)
	  	 (= str "yellow") (stroke 255 255 0 transparency)
	  	 (= str "green") (stroke 0 128 0 transparency)
	  	 (= str "purple") (stroke 128 0 128 transparency)
	  	 (= str "orange") (stroke 255 165 0 transparency)
	  	 (= str "pink") (stroke 255 192 203 transparency)
	  	 (= str "black") (stroke 0 0 0 transparency)
	  	 (= str "white") (stroke 255 255 255 transparency)
		 (= str "brown") (stroke 165 42 42 transparency)
	  	 (= str "gray") (stroke 128 128 128 transparency)
	  	 (= str "grey") (stroke 128 128 128 transparency)
	  	 (= str "silver") (stroke 192 192 192 transparency)
	  	 (= str "gold") (stroke 255 215 0 transparency)
	  	 (= str "cyan") (stroke 0 255 255 transparency)
	  	 (= str "magenta") (stroke 255 0 255 transparency)
	  	 (= str "maroon") (stroke 128 0 0 transparency)
	 	 (= str "navy") (stroke 0 0 128 transparency)
	  	 (= str "lime") (stroke 0 255 0 transparency)
	  	 (= str "teal") (stroke 0 128 128 transparency)
		 (= str "random-color") (stroke (random 255) (random 255) (random 255) transparency)
		)))
		(cond
	 	 (= str "red") (stroke 255 0 0 transparency)
	 	 (= str "blue") (stroke 0 0 255 transparency)
	 	 (= str "yellow") (stroke 255 255 0 transparency)
	 	 (= str "green") (stroke 0 128 0 transparency)
	 	 (= str "purple") (stroke 128 0 128 transparency)
	 	 (= str "orange") (stroke 255 165 0 transparency)
	 	 (= str "pink") (stroke 255 192 203 transparency)
	 	 (= str "black") (stroke 0 0 0 transparency)
		 (= str "brown") (stroke 165 42 42 transparency)
	 	 (= str "white") (stroke 255 255 255 transparency)
	 	 (= str "gray") (stroke 128 128 128 transparency)
	  	 (= str "grey") (stroke 128 128 128 transparency)
	 	 (= str "silver") (stroke 192 192 192 transparency)
	 	 (= str "gold") (stroke 255 215 0 transparency)
	  	 (= str "cyan") (stroke 0 255 255 transparency)
	  	 (= str "magenta") (stroke 255 0 255 transparency)
	 	 (= str "maroon") (stroke 128 0 0 transparency)
	 	 (= str "navy") (stroke 0 0 128 transparency)
	  	 (= str "lime") (stroke 0 255 0 transparency)
	  	 (= str "teal") (stroke 0 128 128 transparency)
		 (= str "random-color") (stroke (random 255) (random 255) (random 255) transparency)
		)
	)
)


;;Text
;;List of available fonts in text file "Fonts.txt"
(defn write-text [str size x y & [color-str font]]
	(cond
	(and (= color-str nil) (= font nil))
 		(do (fill-color "black")
		    (text-font (create-font "Liberation Sans" size true))
  		    (text str x y))
	(= color-str nil) 
		(do (fill-color "black")
		    (text-font (create-font font size true))
  		    (text str x y))
	(= font nil)
		(do (fill-color color-str)
		    (text-font (create-font "Liberation Sans" size true))
  		    (text str x y))
	(not (or (= color-str nil) (= font nil)))
		(do (fill-color color-str)
	    		(text-font (create-font font size true))
  	    		(text str x y))
	)	
)

;; Shapes
(defn draw-rect [x y width height str & [transparency]]
	(if (= transparency nil) (do (let [transparency 255]
		  (do (fill-color str transparency)
		  (rect x y width height))
		)
		(do (fill-color str transparency)
		(rect x y width height))
	)
))

(defn draw-circle [x y radius str & [transparency]]
	(if (= transparency nil) (do (let [transparency 255]
		  (do (fill-color str transparency)
		  (ellipse x y radius radius))
		)
		(do (fill-color str transparency)
		(ellipse x y radius radius))
	)
))

(defn draw-ellipse [x y width height str & [transparency]]
	(if (= transparency nil) (do (let [transparency 255]
		  (do (fill-color str transparency)
		  (ellipse x y width height))
		)
		(do (fill-color str transparency)
		(ellipse x y width height))
	)
))


(defn draw-triangle [x1 y1 x2 y2 x3 y3 str & [transparency]]
	(if (= transparency nil) 
		(do (let [transparency 255]
		  (do (fill-color str transparency)
		  (triangle x1 y1 x2 y2 x3 y3))
		)
		(do (fill-color str transparency)
		(triangle x1 y1 x2 y2 x3 y3))
	)
))

;;key-codes
(defn key-input []
	(cond 
		(= (key-code) 38) "UP-ARROW"
		(= (key-code) 40) "DOWN-ARROW"
		(= (key-code) 37) "LEFT-ARROW"
		(= (key-code) 39) "RIGHT-ARROW"
		(= (key-code) 32) "SPACE"
		(= (key-code) 16) "SHIFT"
		(= (key-code) 9) "TAB"
		(= (key-code) 8) "BACKSPACE"
		(= (key-code) 10) "ENTER"
		(= (key-code) 17) "CTRL"
		(= (key-code) 18) "ALT"
		(= (key-code) 192) "`"
		(= (key-code) 222) "'"
		:else (char (key-code))
	)
)

;(defn setup []
;	(smooth)
;	(setup-state (cons (cons :updates updates) (cons (:states states) (flatten (vec states)))))
;	(:setup (state :updates))
;)

;(defn draw []
;	(map (fn[key val] (if (and (not (= key :setup)) (fn? val)) (get-value key))) (state :updates)) ;;(set-state key (val (get-value key)))))
;)
