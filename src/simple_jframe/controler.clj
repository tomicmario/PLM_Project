(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.entities :as e])
  (:require [simple-jframe.inputManager :as im]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(defn extract-from-data [type data]
  (filterv identity (map type data)))

(defn get-state [timestamp]
  (let [player @e/player_state
        enemies @e/enemies
        enemy-projectiles @e/enemy-projectiles
        player-proj @e/player-projectiles
        inputs @im/inputs
        mouse @im/mouse]
    {:player player :enemies enemies :e-proj enemy-projectiles :p-proj player-proj
     :inputs inputs :mouse mouse :timestamp timestamp}))

(defn save-state [state]
  (reset! e/player-projectiles (into [] (:p-proj state)))
  (reset! e/enemy-projectiles (into [] (:e-proj state)))
  (reset! e/enemies (into [] (:enemies state)))
  (reset! e/player_state (:player state)))

(defn update-state [state]
  (if (im/contains (:inputs state) :reset)
    (e/reset-all)
    (save-state state)))

(defn move-proj [state]
  (let [e-proj (map (fn [p] (e/move p)) (:e-proj state))
        p-proj (map (fn [p] (e/move p)) (:p-proj state))]
    (-> state
        (assoc :e-proj e-proj)
        (assoc :p-proj p-proj))))

(defn move-enemies [state]
  (let [enemies (map (fn [e] (e/move e (e/gen-vector e (:player state)))) (:enemies state))]
    (assoc state :enemies enemies)))

(defn move-player [state]
  (let [vector (im/input-to-vector (:inputs state))
        player (e/move (:player state) vector)]
    (assoc state :player player)))

(defmulti get-target (fn [entity & []] [(:type entity)]))

(defmethod get-target [:player] [[] state]
  (:mouse state))

(defmethod get-target [:axe-man] [[] state]
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
        (assoc :enemies updated-enemies)
        (assoc :e-proj (flatten (concat (:e-proj state) shot-projectiles))))))

(defn player-shoot [state]
  (let [proj-data (get-shoot-data (:player state) state)
        new-proj (:projectiles proj-data)
        updated-proj (if (nil? new-proj) (:p-proj state)
                     (conj (:p-proj state) new-proj))]
    (-> state
        (assoc :player (:entity proj-data))
        (assoc :p-proj updated-proj))))

(defn add-enemy [state]
  (if (< (count (:enemies state)) 10)
    (let [enemies (conj (:enemies state) (e/create-axeman))]
      (assoc state :enemies enemies))
    state))

(defn correct-positions [state]
  (let [player (:player state)
        enemies (:enemies state)]
    (-> state
        (assoc :player (e/correct-position player bounds))
        (assoc :enemies (map (fn [e] (e/correct-position e bounds)) enemies)))))

(defn close-enough? [entity]
  (> 1000 (max (Math/abs (:x entity)) (Math/abs (:y entity)))))

(defn proj-valid? [entity state] 
  (let [ttl (:max-ttl entity)]
    (if (nil? ttl) true (< (:timestamp state) ttl))))

(defn clean-projectiles [state]
  (-> state
      (assoc :p-proj (filterv close-enough? (:p-proj state)))
      (assoc :e-proj (filterv close-enough? (:e-proj state)))
      (assoc :p-proj (filterv (fn [p] (proj-valid? p state)) (:p-proj state)))
      (assoc :e-proj (filterv (fn [p] (proj-valid? p state)) (:e-proj state)))))

(defn clean-enemies [state]
  (assoc state :enemies (filterv (fn [e] (< 0 (:health e))) (:enemies state))))

(defn collision-temp [a b]
  (let [x1 (+ (:x a)) y1 (+ (:y a))
        x2 (+ (:x b)) y2 (+ (:y b))
        distance (Math/sqrt (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1))))]
    (< distance (+ (/ (:width a) 2) (/ (:width b) 2)))))

(defn colliding? [a b]
  (collision-temp a b))

(defn get-collide-damage [collisions]
  (reduce + (map (fn [x] (:health x)) collisions)))

(defn get-collision-data [entity projectiles]
  (let [collide-cond (fn [e] (colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles (if (empty? colliding) nil colliding)}))

(defn apply-damage [d]
  (e/damage-entity (get-collide-damage (:projectiles d)) (:entity d)))

(defn remove-collided [projectiles collided]
  (let [colliding-proj (into #{} collided)]
    (filterv (fn [p] (not (contains? colliding-proj p))) projectiles)))

(defn treat-collision-player [state]
  (let [collided-proj (get-collision-data (:player state) (:e-proj state))
        updated-player (apply-damage collided-proj)]
    (-> state
        (assoc :player updated-player)
        (assoc :e-proj (remove-collided (:e-proj state) collided-proj)))))


(defn treat-collision-enemies [state]
  (let [collision-data (map (fn [e] (get-collision-data e (:p-proj state))) (:enemies state))
        collided-proj  (into [] (extract-from-data :projectiles collision-data))
        updated-enemies (map apply-damage collision-data)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :p-proj (remove-collided (:p-proj state) collided-proj)))))

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

(println (player-shoot (get-state 0)))