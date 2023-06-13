(ns game.controler
  (:gen-class)
  (:require [game.entities :as e]
            [game.state :as state]))

(def exclusion-radius 150)

(def max-enemy 10)

(defn in-bounds? [entity state]
  (let [bounds (:bounds state)]
    (and (>= (:x entity) (:min-x bounds)) (>= (:y entity) (:min-y bounds))
         (<= (:x entity) (:max-x bounds)) (<= (:y entity) (:max-y bounds)))))

(defn extract-from-data [type data] ; util function for pairs, filters nils
  (filterv identity (mapv type data)))

(defn closer-than-distance? [a b d]
  ; Simplified version not using sqrt
  (let [dist (* d d)
        distY (* (- (:y a) (:y b)) (- (:y a) (:y b)))
        distX (* (- (:x a) (:x b)) (- (:x a) (:x b)))]
  (> dist (+ distX distY ))))

; COLLISION HANDLING 
(defn colliding? [a b]
  (let [max-dist (+ (/ (:width a) 2) (/ (:width b) 2))]
    (closer-than-distance? a b max-dist )))

(defn get-collide-damage [collisions]
  (reduce + (mapv :health collisions)))

; returns a map containing the entity and the projectiles that collided with it
(defn get-collision-data [entity projectiles]
  (let [collide-cond (fn [e] (colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles (if (empty? colliding) nil colliding)}))

(defn apply-damage [d] ; requires the map of get-collision data
  (e/damage-entity (get-collide-damage (:projectiles d)) (:entity d)))

(defn remove-collided [projectiles collided] ; requires the map of get-collision data on collided
  (let [colliding-proj (into #{} collided)
        has-not-collided? (fn [p] (not (contains? colliding-proj p)))]
    (filterv has-not-collided? projectiles)))

(defn treat-collision-player [state]
  (let [collided-proj (get-collision-data (:player state) (:e-proj state))
        updated-player (apply-damage collided-proj)
        new-proj (remove-collided (:e-proj state) (:projectiles collided-proj))]
    (-> state
        (assoc :player updated-player)
        (assoc :e-proj new-proj))))

(defn treat-collision-enemies [state]
  (let [collision-data (mapv (fn [e] (get-collision-data e (:p-proj state))) (:enemies state))
        collided-proj  (flatten (extract-from-data :projectiles collision-data))
        updated-enemies (mapv apply-damage collision-data)
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
  (let [is-valid? (fn [p] (and (proj-valid? p state) (in-bounds? p state)))]
    (-> state
        (assoc :p-proj (filterv is-valid? (:p-proj state)))
        (assoc :e-proj (filterv is-valid? (:e-proj state))))))

(defn clean-enemies [state]
  (let [updated-enemies (filterv e/is-alive? (:enemies state))
        killed (- (count (:enemies state)) (count updated-enemies)) 
        new-score (+ (:score state) killed)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :score new-score))))

(defn remove-dead-entities [state]
  (-> state
      (clean-projectiles)
      (clean-enemies)))
; END DEAD ENTITY REMOVAL

; SHOOTING
(defmulti get-target (fn [entity & []] [(:type entity)]))

(defmethod get-target [:player] [[] state]
  (:mouse state))

(defmethod get-target [:kamikaze] [[] state]
  (:player state))

(defmethod get-target [:shooter] [[] state]
  (:player state))

(defmulti can-shoot? (fn [entity & []] [(:type entity)]))

(defmethod can-shoot? [:player] [entity state]
  (and (> (:timestamp state) (+ (:last-shot entity) (:firerate entity)))
       (contains? (:inputs state) :click)
       (e/is-alive? entity)))

(defmethod can-shoot? [:shooter] [entity state]
  (> (:timestamp state) (+ (:last-shot entity) (:firerate entity))))

(defmethod can-shoot? [:kamikaze] [entity state]
  (let [player (:player state)
        shoot-distance 30]
    (and (closer-than-distance? entity player shoot-distance)
         (e/is-alive? player))))

(defn get-shoot-data [entity state]
  (let [timestamp (:timestamp state)
        target (get-target entity state)]
    (if (can-shoot? entity state)
      (let [updated-entity (e/update-timestamp entity timestamp)]
        {:entity updated-entity :projectiles (e/create-projectile updated-entity target)})
      {:entity entity :projectiles nil})))

(defn enemies-shoot [state]
  (let [proj-data (mapv (fn [e] (get-shoot-data e state)) (:enemies state))
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
; END ENTITY SHOOTING

; ENTITY MOVING
(defn correct-positions [state]
  (let [corrected-entity (fn [e] (e/correct-position e (:bounds state)))]
    (-> state
        (assoc :player (e/correct-position (:player state) (:bounds state)))
        (assoc :enemies (mapv corrected-entity (:enemies state))))))

(defn move-proj [state]
  (let [e-proj (mapv e/move (:e-proj state))
        p-proj (mapv e/move (:p-proj state))]
    (-> state
        (assoc :e-proj e-proj)
        (assoc :p-proj p-proj))))

(defn move-enemies [state]
  (let [get-entity-vec (fn [e] (e/gen-vector e (get-target e state)))
        move-enemy (fn [e] (e/move e (get-entity-vec e)))
        enemies (map move-enemy (:enemies state))]
    (assoc state :enemies enemies)))

(defn input-to-vector [player inputs] ; generates vector based on the inputs, for the player
  (let [speed (:speed player)
        y (- (if (contains? inputs :down) speed 0) (if (contains? inputs :up) speed 0))
        x (- (if (contains? inputs :right) speed 0) (if (contains? inputs :left) speed 0))]
    {:vec-x x :vec-y y}))

(defn move-player [state]
  (let [player (:player state)
        vec (input-to-vector player (:inputs state))
        vector (if (e/is-alive? player) vec {:vec-x 0 :vec-y 0})
        updated-player (e/move player vector)]
    (assoc state :player updated-player)))

(defn move-entities [state]
  (-> state
      (move-proj)
      (move-player)
      (move-enemies)
      (correct-positions)))
; END ENTITY MOVING

; SPAWN LOGIC
(defn spawn-coordinates [bounds player exclusion]
  (let [diameter (* exclusion 2)
        x (rand (- (:max-x bounds) diameter))
        y (rand (- (:max-y bounds) diameter))
        new-x (if (> (- (:x player) exclusion) x) x
                  (+ x diameter))
        new-y (if (> (- (:y player) exclusion) y) y
                  (+ y diameter))]
    {:x new-x :y new-y}))

(defn rand-coordinates [state]
  ; returns a coordinate not present withing a given amount of units from the player
  (let [bounds (:bounds state)
        player (:player state)
        rand-x (rand (:max-x bounds))
        rand-y (rand (:max-y bounds))
        close-enough? (closer-than-distance? {:x rand-x :y rand-y} player exclusion-radius)]
    (if close-enough? (spawn-coordinates bounds player exclusion-radius)
        {:x rand-x :y rand-y})))

(defn add-enemy [state]
  (if (< (count (:enemies state)) max-enemy)
    (let [en-fn (e/random-enemy) ; enemy create function
          rand-cor (rand-coordinates state)
          enemy (en-fn (:x rand-cor) (:y rand-cor))
          enemies (conj (:enemies state) enemy)]
      (assoc state :enemies enemies))
    state))

; for display purpose, calculates here at which angle the entities should be turned
(defn update-angle-player [state]
  (let [angle (e/calculate-angle (:mouse state) (:player state))
        updated-player (assoc (:player state) :angle angle)]
    (assoc state :player updated-player)))

(defn update-angle-enemies [state]
  (let [fn-target (fn [e] (e/calculate-angle (get-target e state) e))
        updated-enemy (fn [e] (assoc e :angle (fn-target e)))]
    (assoc state :enemies (mapv updated-enemy (:enemies state)))))

(defn update-angle-for-display [state]
  (-> state
      (update-angle-player)
      (update-angle-enemies)))

; ENTIRE FRAME LOGIC
(defn next-tick []
  (-> (state/get-state)
      (treat-collision)
      (remove-dead-entities)
      (move-entities)
      (shoot-entities)
      (add-enemy)
      (update-angle-for-display)
      (state/update-state)))