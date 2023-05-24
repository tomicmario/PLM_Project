(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.entities :as e])
  (:require [simple-jframe.inputManager :as im]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(defn close-enough? [entity]
  (> 1000 (max (Math/abs (:x entity)) (Math/abs (:y entity)))))

(defn can-shoot? [entity timestamp]
  (> timestamp (+ (:last-shot (:more entity)) 5)))

(defn get-state [timestamp]
  (let [player @e/player_state
       enemies @e/enemies
       projectiles @e/projectiles
       inputs @im/inputs
       mouse @im/mouse]
    {:player player :enemies enemies :projectiles projectiles
     :inputs inputs :mouse mouse :timestamp timestamp :collided nil}))

(defn save-state [state] 
  (reset! e/projectiles (:projectiles state))
  (reset! e/enemies (:enemies state))
  (reset! e/player_state (:player state)))

(defn move-proj [state]
  (let [proj (map (fn [p] (e/move p)) (:projectiles state))]
    (assoc-in state [:projectiles] proj)))

(defn move-enemies [state]
  (let [enemies (map (fn [e] (e/move e (e/gen-vector e (:player state)))) (:enemies state))]
    (assoc-in state [:enemies] enemies)))

(defn move-player [state] 
  (let [vector (im/input-to-vector (:inputs state))
        player (e/move (:player state) vector)]
    (assoc-in state [:player] player)))

(defn shoot [state projectile]
  (let [new-proj (conj (:projectiles state) projectile)]
    (-> state
        (assoc-in [:projectiles] new-proj)
        (assoc-in [:player] (e/update-timestamp (:player state) (:timestamp state))))))

(defn player-shoot [state]
  (let [player (:player state)
        timestamp (:timestamp state)]
  (if (and (im/contains (:inputs state) :click) (can-shoot? player timestamp))
    (shoot state (e/create-projectile (:player state) (:mouse state)))
    state)))

(defn add-enemy [state]
  (if (< (count (:enemies state)) 10) 
    (let [enemies (conj (:enemies state) (e/create-axeman))]
     (assoc-in state [:enemies] enemies)) 
    state))

(defn correct-positions [state]
  (let [player (:player state)
        enemies (:enemies state)]
    (-> state
        (assoc-in [:player] (e/correct-position player bounds))
        (assoc-in [:enemies] (map (fn [e] (e/correct-position e bounds)) enemies)))))

(defn clean-projectiles [state]
  (assoc-in state [:projectiles] (filter (fn [e] (close-enough? e)) (:projectiles state))))

(defn clean-projectiles2 [state]
  (let [test (into #{} (:collided state))]
     (assoc-in state [:projectiles] (filter (fn [e] (not(contains? test e))) (:projectiles state)))))

(defn clean-enemies [state]
  (assoc-in state [:enemies] (filter (fn [e] (< 0 (:health e))) (:enemies state))))

(defn collision-temp [a b]
  (let [x1 (+ (:x a))
        y1 (+ (:y a))
        x2 (+ (:x b))
        y2 (+ (:y b))
        distance (Math/sqrt (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1))))]
    (< distance (+ (/ (:width a) 2) (/ (:width b) 2)))))

(defn colliding? [a b]
  ;todo
  (if (= :player (:type a)) false 
      (collision-temp a b)))

(defn get-collide-damage [collisions] 
  (reduce + (map (fn[x] (:health x)) collisions)))

(defn collide [entity projectiles]
  (let [colliding (filter (fn [e] (colliding? entity e)) projectiles)]
    {:entity entity :projectiles (if (empty? colliding) nil colliding)}))


(defn treat-collision-player [state]
  (let [player (:player state)
        projectiles (:projectiles state)
        collided (collide player projectiles)
        new-player (e/damage-entity (get-collide-damage (:projectiles collided)) player)]
    (-> (assoc-in state [:player] new-player)
        (assoc-in [:collided] (flatten (conj (:collided state) (:projectiles collided)))))))

(defn treat-collision-enemies [state]
  (let [enemies (:enemies state)
        projectiles (:projectiles state)
        collision-data (map (fn [e] (collide e projectiles)) enemies)
        collided  (filter identity (map :projectiles collision-data))
        new-enemy (map (fn [e] (e/damage-entity (get-collide-damage (:projectiles e)) (:entity e))) collision-data)]
    (-> (assoc-in state [:enemies] new-enemy)
        (assoc-in [:collided] (flatten (conj (:collided state) collided))))))


(defn move [timestamp]
  (-> (get-state timestamp) 
      (move-proj)
      (move-player)
      (move-enemies)
      (treat-collision-player)
      (treat-collision-enemies)
      (correct-positions)
      (add-enemy)
      (player-shoot)
      (clean-projectiles)
      (clean-projectiles2)
      (clean-enemies)
      (save-state)))
