(ns simple-jframe.player
  (:gen-class)
  (:require [simple-jframe.inputManager :as im]))

(def player (atom {:x 0, :y 0}))
(def player-vector (atom {:x 0, :y 0}))

(defn update-vector []
  (let [y (- (if (im/contains :down) 1 0) (if (im/contains :up) 1 0))
        x (- (if (im/contains :right) 1 0) (if (im/contains :left) 1 0))]
    (swap! player-vector assoc-in [:y] y)
    (swap! player-vector assoc-in [:x] x)))

(defn move-player []
  (update-vector)
  (swap! player assoc-in [:y] (+ (:y @player-vector) (:y @player)))
  (swap! player assoc-in [:x] (+ (:x @player-vector) (:x @player))))