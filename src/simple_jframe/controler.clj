(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.entities :as e])
  (:require [simple-jframe.inputManager :as im]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(defn close-enough? [entity]
  (> 1000 (max (Math/abs (:x entity)) (Math/abs (:y entity)))))

(defn extract-from-data [type data]
  (filter identity (map type data)))

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

(defn update-state [state] 
  (if (im/contains (:inputs state) :reset)
    (e/reset-all)
    (save-state state)))

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


(defmulti get-target (fn [entity & []] [(:type entity)]))

(defmethod get-target [:player] [e state]
  (:mouse state))

(defmethod get-target [:axe-man] [e state]
  (:player state))

(defmulti can-shoot? (fn [entity & []] [(:type entity)]))

(defmethod can-shoot? [:player] [entity state]
  (and (> (:timestamp state) (+ (:last-shot entity) 10))
       (im/contains (:inputs state) :click)))

(defmethod can-shoot? [:axe-man] [entity state]
  (let [b (:player state)
        x1 (+ (:x entity)) y1 (+ (:y entity))
        x2 (+ (:x b)) y2 (+ (:y b))
        distance (Math/sqrt (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1))))]
    (< distance 30)))

(defn get-shoot-data [entity state]
  (let [timestamp (:timestamp state)
        target (get-target entity state)]
    (if (can-shoot? entity state)
      (let [updated-entity (e/update-timestamp entity timestamp)]
        {:entity updated-entity :projectiles (e/create-projectile updated-entity target)})
      {:entity entity :projectiles nil})))

(defn enemies-shoot [state]
  (let [proj-data (map (fn [e] (get-shoot-data e state)) (:enemies state))
        shot-projectiles (extract-from-data :projectiles proj-data)
        updated-enemies (extract-from-data :entity proj-data)]
    (-> state
        (assoc-in [:enemies] updated-enemies)
        (assoc-in [:projectiles] (flatten (conj (:projectiles state) shot-projectiles))))))

(defn player-shoot [state]
  (let [proj-data (get-shoot-data (:player state) state)]
    (-> state
        (assoc-in [:player] (:entity proj-data))
        (assoc-in [:projectiles] (conj (:projectiles state) (:projectiles proj-data))))))

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
  (let  [colliding-proj (into #{} (:collided state))]
    (-> state 
        (assoc-in [:projectiles] (filter close-enough? (:projectiles state)))
        (assoc-in [:projectiles] (filter (fn [e] (not(contains? colliding-proj e))) (:projectiles state))))))

(defn clean-enemies [state]
  (assoc-in state [:enemies] (filter (fn [e] (< 0 (:health e))) (:enemies state))))

(defn collision-temp [a b]
  (let [x1 (+ (:x a)) y1 (+ (:y a))
        x2 (+ (:x b)) y2 (+ (:y b))
        distance (Math/sqrt (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1))))]
    (< distance (+ (/ (:width a) 2) (/ (:width b) 2)))))

(defn colliding? [a b]
  (collision-temp a b))

(defn get-collide-damage [collisions] 
  (reduce + (map (fn[x] (:health x)) collisions)))

(defn get-collision-data [entity projectiles]
  (let [collide-cond (fn [e] (and (colliding? entity e) (not= :player (:type entity))))
        colliding (filter collide-cond projectiles)]
    {:entity entity :projectiles (if (empty? colliding) nil colliding)}))

(defn apply-damage[d]
  (e/damage-entity (get-collide-damage (:projectiles d)) (:entity d)))

(defn colliding-proj-from-data [d]
  (filter identity (map :projectiles d)))

(defn treat-collision-player [state]
  (let [collided-proj (get-collision-data (:player state) (:projectiles state))
        updated-player (apply-damage collided-proj)]
    (-> state
        (assoc-in [:player] updated-player)
        (assoc-in [:collided] (flatten (conj (:collided state) (:projectiles collided-proj)))))))

(defn treat-collision-enemies [state]
  (let [collision-data (map (fn [e] (get-collision-data e (:projectiles state))) (:enemies state))
        collided-proj  (colliding-proj-from-data collision-data)
        updated-enemies (map apply-damage collision-data)]
    (-> state
     (assoc-in [:enemies] updated-enemies)
     (assoc-in [:collided] (flatten (conj (:collided state) collided-proj))))))

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
      (enemies-shoot)
      (clean-projectiles)
      (clean-enemies)
      (update-state)))