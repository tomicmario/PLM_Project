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

(defn input-to-vector []
  (let [inputs @inputs
        y (- (if (contains? inputs :down) 1 0) (if (contains? inputs :up) 1 0))
        x (- (if (contains? inputs :right) 1 0) (if (contains? inputs :left) 1 0))]
    {:vec-x x :vec-y y}))
