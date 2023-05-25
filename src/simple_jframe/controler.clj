(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.entities :as e])
  (:require [simple-jframe.inputManager :as im]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(defn close-enough? [entity]
  (> 1000 (max (Math/abs (:x entity)) (Math/abs (:y entity)))))

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

(defn shoot [state projectile]
  (let [new-proj (conj (:projectiles state) projectile)]
    (-> state
        (assoc-in [:projectiles] new-proj)
        (assoc-in [:player] (e/update-timestamp (:player state) (:timestamp state))))))

(defn can-player-shoot?[state]
  (let [last-player-shot (:last-shot (:player state))]
    (and (> (:timestamp state) (+ last-player-shot 5))
         (im/contains (:inputs state) :click))))

(defn player-shoot [state]
  (if (can-player-shoot? state)
    (shoot state (e/create-projectile (:player state) (:mouse state)))
    state))

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
      (clean-projectiles)
      (clean-enemies)
      (update-state)))