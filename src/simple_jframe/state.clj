(ns simple-jframe.state
  (:gen-class)
  (:require [simple-jframe.entities :as e]))

(def inputs (atom #{}))
(def mouse (atom {:x 0, :y 0}))

(def player_state (atom (e/default-player)))
(def enemies (atom []))
(def player-projectiles (atom []))
(def enemy-projectiles (atom []))

(defn get-state [& [timestamp]]
  (let [player @player_state
        enemies @enemies
        enemy-projectiles @enemy-projectiles
        player-proj @player-projectiles
        inputs @inputs
        mouse @mouse]
    {:player player :enemies enemies :e-proj enemy-projectiles :p-proj player-proj
     :inputs inputs :mouse mouse :timestamp timestamp}))

(defn add-input[x] 
  (swap! inputs conj x))

(defn remove-input [x]
  (swap! inputs disj x))

(defn update-mouse [x, y] 
  (swap! mouse assoc-in [:x] x)
  (swap! mouse assoc-in [:y] y))

(defn reset-all []
  (reset! player_state (e/default-player))
  (reset! enemies  [])
  (reset! player-projectiles [])
  (reset! enemy-projectiles []))

(defn save-state [state]
  (reset! player-projectiles (into [] (:p-proj state)))
  (reset! enemy-projectiles (into [] (:e-proj state)))
  (reset! enemies (into [] (:enemies state)))
  (reset! player_state (:player state)))

(defn update-state [state]
  (if (contains? (:inputs state) :reset)
    (reset-all)
    (save-state state)))