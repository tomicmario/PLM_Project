(ns simple-jframe.inputManager
  (:gen-class))

(def inputs (atom #{}))
(def mouse (atom {:x 0, :y 0}))

(defn add-input[x] 
  (swap! inputs conj x))

(defn remove-input [x]
  (swap! inputs disj x))

(defn contains [x]
  (contains? @inputs x))

(defn update-mouse [x, y] 
  (swap! mouse assoc-in [:x] x)
  (swap! mouse assoc-in [:y] y))