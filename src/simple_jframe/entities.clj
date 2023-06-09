(ns simple-jframe.entities
  (:gen-class))

(defn entity [x y health width height speed type & [more]]
  (merge {:x x :y y :health health :speed speed
   :width width :height height :type type} more))

(defn player []
  (entity 0 0 100 20 20 0.5 :player {:last-shot -1000}))

(def player_state (atom (player)))
(def enemies (atom []))
(def player-projectiles (atom []))
(def enemy-projectiles (atom []))

(defn reset-all []
  (reset! player_state (player))
  (reset! enemies  [])
  (reset! player-projectiles [])
  (reset! enemy-projectiles []))

(defn calculate-angle [target-x target-y x y]
  (Math/atan2 (- target-x x) (- target-y y)))

(defn projectile [x y radius speed vector & [max-ttl]]
  (let [proj-data (merge vector {:max-ttl max-ttl})]
  (entity x y 10 radius radius speed :projectile proj-data)))

(defn axe-man [x y health]
  (entity x y health 30 30 0.2 :axe-man))

(defn shooter [x y health]
  (entity x y health 10 10 0.5 :shooter))

(defn gen-vector [entity target]
  (let [x (:x entity)
        y (:y entity)
        speed (:speed entity)
        target-x (:x target)
        target-y (:y target)
        angle (- (/ Math/PI 4) (calculate-angle target-x target-y x y))
        vec-x (- (* (Math/cos angle) speed) (* (Math/sin angle) speed))
        vec-y (+ (* (Math/sin angle) speed) (* (Math/cos angle) speed))]
    {:vec-x vec-x :vec-y vec-y}))

(defmulti create-projectile (fn [entity & []] [(:type entity)]))

(defmethod create-projectile [:player] [entity mousePosition]
  (let [proj (projectile (:x entity) (:y entity) 10 5 nil)
        vec (gen-vector proj mousePosition)]
    (merge proj vec)))

(defmethod create-projectile [:axe-man] [entity target]
  (let [x (:x entity)
        y (:y entity)
        vec (gen-vector entity target)]
    (projectile x y 50 0 vec (+ (:last-shot entity) 2))))

(defn create-axeman []
  (let [x (rand-int 500)
        y (rand-int 500)]
    (axe-man x y 100)))

(defn create-shooter []
  (let [x (rand-int 500)
        y (rand-int 500)]
    (shooter x y 100)))

(defn damage-entity [damage entity]
  (assoc entity :health (- (:health entity) damage)))

(defn new-position [entity vector] 
  {:x (+ (:x entity) (:vec-x vector))
   :y (+ (:y entity) (:vec-y vector))})

(defn apply-position [entity pos] 
  (-> entity
      (assoc :x (:x pos))
      (assoc :y (:y pos))))

(defn update-timestamp [entity timestamp]
    (merge entity {:last-shot timestamp} ))

(defn correct-position [entity bounds]
  (let [new-x (min (:max-x bounds) (max (:min-x bounds) (:x entity)))
        new-y (min (:max-y bounds) (max (:min-y bounds) (:y entity)))
        new-pos {:x new-x :y new-y}]
    (apply-position entity new-pos)))

(defn default-move [entity vector] 
  (let [pos (new-position entity vector)]
    (apply-position entity pos)))

(defmulti move (fn [entity & []] [(:type entity)]))

(defmethod move [:projectile] [entity]
  (let [vec {:vec-x (:vec-x entity) :vec-y (:vec-y entity)}
        pos (new-position entity vec)]
  (apply-position entity pos)))

(defmethod move [:player] [entity vector]
  (default-move entity vector))

(defmethod move [:axe-man] [entity vector]
  (default-move entity vector))
