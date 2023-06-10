(ns simple-jframe.state
  (:gen-class)
  (:require [simple-jframe.entities :as e]
            [simple-jframe.state :as state]))

(def inputs (atom #{}))
(def mouse (atom {:x 0, :y 0}))

(def entity-state (atom {:player (e/default-player) :p-proj [] 
                         :e-proj [] :enemies [] :timestamp 0}))

(defn default-entity-state []
  (reset! entity-state {:player (e/default-player) :p-proj [] :e-proj 
                        [] :enemies [] :timestamp 0}))

(defn get-state []
  (let [inputs @inputs
        mouse @mouse
        entities @entity-state]
     (merge entities {:inputs inputs :mouse mouse})))

(defn add-input[x] 
  (swap! inputs conj x))

(defn remove-input [x]
  (swap! inputs disj x))

(defn update-mouse [x y] 
  (swap! mouse assoc :x x)
  (swap! mouse assoc :y y))

(defn reset []
  (default-entity-state))

(defn save-state [state]
  (let [new-state {:e-proj (:e-proj state)
                   :p-proj (:p-proj state)
                   :player (:player state)
                   :enemies (:enemies state)
                   :timestamp (inc (:timestamp state))}]
    (reset! entity-state new-state)))

(defn update-state [state] 
  (if (contains? (:inputs state) :reset)
    (reset)
    (save-state state)))