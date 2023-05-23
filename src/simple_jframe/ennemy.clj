(ns simple-jframe.ennemy
  (:gen-class)
  (:require [simple-jframe.player :as p]))

(def ennemies (atom []))

(defn calculate-angle [target-x target-y x y]
  (Math/atan2 (- target-x x) (- target-y y)))

(defn create-axeman []
  (let [x (rand-int 500)
        y (rand-int 500)
        angle (- 315 (calculate-angle (:x @p/player) (:y @p/player) x y))]
    {:Ennemy :AxeMan
     :x x :y y
     :vec-x (- (Math/cos angle) (Math/sin angle))
     :vec-y (+ (Math/sin angle) (Math/cos angle))}))

(defn create-shooter []
  (let [x (rand-int 500)
        y (rand-int 500)
        angle (- 315 (calculate-angle (:x @p/player) (:y @p/player) x y))]
    {:x x :y y
     :vec-x (- (Math/cos angle) (Math/sin angle))
     :vec-y (+ (Math/sin angle) (Math/cos angle)) 
     :Ennemy :Shooter}))

(defmulti move-ennemy (fn [x] [(:Ennemy x)]))

(defmethod move-ennemy [:AxeMan] [x] 
  (let [angle (- 315 (calculate-angle (:x @p/player) (:y @p/player) (:x x) (:y x)))]
  {:vec-x (- (Math/cos angle) (Math/sin angle))
   :vec-y (+ (Math/sin angle) (Math/cos angle)) 
   :x (+ (:x x) (:vec-x x))
   :y (+ (:y x) (:vec-y x))
   :Ennemy :AxeMan}))

(defn move-ennemies []
  (let [updated-ennemies (mapv move-ennemy @ennemies)]
    (reset! ennemies updated-ennemies)))