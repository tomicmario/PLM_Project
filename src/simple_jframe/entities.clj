(ns simple-jframe.entities
  (:gen-class) 
  (:require [simple-jframe.inputManager :as im]))

(def player_state (atom {:x 0, :y 0 :type :player :health 100}))
(def enemies (atom []))
(def projectiles (atom []))

(defn calculate-angle [target-x target-y x y]
  (Math/atan2 (- target-x x) (- target-y y)))

(defn entity [x y health type & [more]] 
  {:x x :y y :health health :type type :more more })

(defn player [x y health]
  (entity x y health :player))

(defn projectile [x y vector]
  (entity x y 0 :projectile vector))

(defn axe-man [x y health]
  (entity x y health :axe-man))

(defn shooter [x y health]
  (entity x y health :shooter))

(defn gen-vector [entity target]
  (let [x (:x entity)
        y (:y entity)
        target-x (:x target)
        target-y (:y target)
        angle (- 315 (calculate-angle target-x target-y x y))
        vec-x (- (Math/cos angle) (Math/sin angle))
        vec-y (+ (Math/sin angle) (Math/cos angle))]
    {:vec-x vec-x :vec-y vec-y}))

(defn create-projectile [player mousePosition]
  (let [x (:x player)
        y (:y player)
        vector (gen-vector player mousePosition)]
    (projectile x y vector)))

(defn create-axeman []
  (let [x (rand-int 500)
        y (rand-int 500)]
    (axe-man x y 100)))

(defn create-shooter []
  (let [x (rand-int 500)
        y (rand-int 500)]
    (shooter x y 100)))

(defn new-position [entity vector] 
  {:x (+ (:x entity) (:vec-x vector))
   :y (+ (:y entity) (:vec-y vector))})

(defmulti move (fn [entity vector] [(:type entity)]))

(defmethod move [:axe-man] [entity vector] 
  (let [pos (new-position entity vector)]
  (axe-man (:x pos) (:y pos) (:health entity))))

(defmethod move [:projectile] [projectile vec]
  (projectile (+ (:x projectile) (:vec-x vec)) 
              (+ (:y projectile) (:vec-y vec))
              vec))

(defmethod move [:player] [entity vector]
  (let [pos (new-position entity vector)]
  (player (:x pos) (:y pos) (:health entity))))

