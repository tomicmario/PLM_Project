(ns game.state
  (:gen-class)
  (:require [game.entities :as e]
            [game.state :as state]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(def inputs (atom #{}))
(def mouse (atom {:x 0, :y 0}))

(defn default-state []
  {:player (e/default-player) :p-proj [] :e-proj []
   :enemies [] :timestamp 0 :bounds bounds :score 0})

(def entity-state (atom (default-state)))

(defn default-entity-state []
  (reset! entity-state (default-state)))

(defn get-state []
  (let [inputs @inputs
        mouse @mouse
        entities @entity-state]
    (merge entities {:inputs inputs :mouse mouse})))

(defn add-input [x]
  (swap! inputs conj x))

(defn remove-input [x]
  (swap! inputs disj x))

(defn update-mouse [x y max-x max-y]
  (let [new-x (* x (/ (:max-x bounds) max-x))
        new-y (* y (/ (:max-y bounds) max-y))]
  (swap! mouse assoc :x new-x)
  (swap! mouse assoc :y new-y)))

(defn reset []
  (default-entity-state))

(defn save-state [state]
  (let [new-state {:e-proj (:e-proj state) :p-proj (:p-proj state)
                   :player (:player state) :enemies (:enemies state)
                   :timestamp (inc (:timestamp state))
                   :bounds (:bounds state) :score (:score state)}]
    (reset! entity-state new-state)))

(defn update-state [state]
  (if (contains? (:inputs state) :reset)
    (reset)
    (save-state state)))