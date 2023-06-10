(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.entities :as e])
  (:require [simple-jframe.state :as state]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(defn in-bounds? [entity]
  (and (>= (:x entity) (:min-x bounds)) (>= (:y entity) (:min-y bounds))
       (<= (:x entity) (:max-x bounds)) (<= (:y entity) (:max-y bounds))))

(defn extract-from-data [type data]
  (filterv identity (map type data)))

(defn distance [x1 y1 x2 y2]
  (Math/sqrt (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1)))))

; COLLISION HANDLING 
(defn colliding? [a b]
  (let [x1 (+ (:x a)) y1 (+ (:y a))
        x2 (+ (:x b)) y2 (+ (:y b))
        dist (distance x1 y1 x2 y2)]
    (< dist (+ (/ (:width a) 2) (/ (:width b) 2)))))

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
        updated-player (apply-damage collided-proj)
        new-proj (remove-collided (:e-proj state) (:projectiles collided-proj))]
    (-> state
        (assoc :player updated-player)
        (assoc :e-proj new-proj))))

(defn treat-collision-enemies [state]
  (let [collision-data (map (fn [e] (get-collision-data e (:p-proj state))) (:enemies state))
        collided-proj  (flatten (extract-from-data :projectiles collision-data))
        updated-enemies (map apply-damage collision-data)
        new-proj (remove-collided (:p-proj state) collided-proj)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :p-proj new-proj))))

(defn treat-collision [state]
  (-> state
      (treat-collision-player)
      (treat-collision-enemies)))

; DEAD ENTITY REMOVAL
(defn proj-valid? [entity state]
  (let [ttl (:max-ttl entity)]
    (if (nil? ttl) true (< (:timestamp state) ttl))))

(defn clean-projectiles [state]
  (let [condition (fn [p] (and (proj-valid? p state) (in-bounds? p)))]
    (-> state
        (assoc :p-proj (filterv condition (:p-proj state)))
        (assoc :e-proj (filterv condition (:e-proj state))))))

(defn clean-enemies [state]
  (assoc state :enemies (filterv (fn [e] (< 0 (:health e))) (:enemies state))))

(defn remove-dead-entities [state]
  (-> state
      (clean-projectiles)
      (clean-enemies)))

; ENTITY MOVING
(defn correct-positions [state]
  (let [player (:player state)
        enemies (:enemies state)]
    (-> state
        (assoc :player (e/correct-position player bounds))
        (assoc :enemies (map (fn [e] (e/correct-position e bounds)) enemies)))))

(defn move-proj [state]
  (let [e-proj (map (fn [p] (e/move p)) (:e-proj state))
        p-proj (map (fn [p] (e/move p)) (:p-proj state))]
    (-> state
        (assoc :e-proj e-proj)
        (assoc :p-proj p-proj))))

(defn move-enemies [state]
  (let [enemies (map (fn [e] (e/move e (e/gen-vector e (:player state)))) (:enemies state))]
    (assoc state :enemies enemies)))

(defn input-to-vector [inputs]
  (let [y (- (if (contains? inputs :down) 1 0) (if (contains? inputs :up) 1 0))
        x (- (if (contains? inputs :right) 1 0) (if (contains? inputs :left) 1 0))]
    {:vec-x x :vec-y y}))

(defn move-player [state]
  (let [vector (input-to-vector (:inputs state))
        player (e/move (:player state) vector)]
    (assoc state :player player)))

(defn move-entities [state]
  (-> state
      (move-proj)
      (move-player)
      (move-enemies)
      (correct-positions)))

; SHOOTING
(defmulti get-target (fn [entity & []] [(:type entity)]))

(defmethod get-target [:player] [[] state]
  (:mouse state))

(defmethod get-target [:axe-man] [[] state]
  (:player state))

(defmulti can-shoot? (fn [entity & []] [(:type entity)]))

(defmethod can-shoot? [:player] [entity state]
  (and (> (:timestamp state) (+ (:last-shot entity) 10))
       (contains? (:inputs state) :click)))

(defmethod can-shoot? [:axe-man] [entity state]
  (let [b (:player state)
        x1 (+ (:x entity)) y1 (+ (:y entity))
        x2 (+ (:x b)) y2 (+ (:y b))]
    (< (distance x1 y1 x2 y2) 30)))

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

(defn shoot-entities [state]
  (-> state
      (player-shoot)
      (enemies-shoot)))

; SPAWN LOGIC
(defn add-enemy [state]
  (if (< (count (:enemies state)) 10)
    (let [enemies (conj (:enemies state) (e/create-axeman))]
      (assoc state :enemies enemies))
    state))

; ENTIRE FRAME LOGIC
(defn next-tick []
  (-> (state/get-state)
      (treat-collision)
      (remove-dead-entities)
      (move-entities)
      (shoot-entities)
      (add-enemy)
      (state/update-state)))