(ns game.entities
  (:gen-class))

(defn calculate-angle [a b]
  (Math/atan2 (- (:x a) (:x b)) (- (:y a) (:y b))))

(defn is-alive? [entity]
  (> (:health entity) 0))

(defn gen-vector [entity target]
  (let [speed (:speed entity)
        angle (- (/ Math/PI 4) (calculate-angle target entity))
        vec-x (- (* (Math/cos angle) speed) (* (Math/sin angle) speed))
        vec-y (+ (* (Math/sin angle) speed) (* (Math/cos angle) speed))]
    {:vec-x vec-x :vec-y vec-y}))

(defn damage-entity [damage entity]
  (assoc entity :health (- (:health entity) damage)))

(defn update-timestamp [entity timestamp]
  (merge entity {:last-shot timestamp}))

; ENTITIES DEFINITION
(defn entity [x y health width height speed type & [more]]
  (merge {:x x :y y :health health :speed speed
          :width width :height height :type type :angle 0 :max-health health} more))

(defn default-player [x y]
  (entity x y 100 20 20 1 :player {:last-shot 0 :firerate 10}))

(defn projectile [x y damage radius speed vector & [max-ttl]]
  (let [proj-data (merge vector {:max-ttl max-ttl})]
    (entity x y damage radius radius speed :projectile proj-data)))

(defn kamikaze [x y]
  (entity x y 100 20 20 0.6 :kamikaze))

(defn shooter [x y]
  (entity x y 200 30 30 0.25 :shooter {:last-shot 20 :firerate 100}))

(defn random-enemy []
  (rand-nth [kamikaze shooter]))
; END ENTITY DEFINITION

; MOVEMENT RELATED
(defn new-position [entity vector]
  {:x (+ (:x entity) (:vec-x vector))
   :y (+ (:y entity) (:vec-y vector))})

(defn apply-position [entity pos]
  (-> entity 
      (assoc :x (:x pos))
      (assoc :y (:y pos))))

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

(defmethod move [:kamikaze] [entity vector]
  (default-move entity vector))

(defmethod move [:shooter] [entity vector]
  (default-move entity vector))
; END MOVEMENT

; PROJECTILE CREATION
(defmulti create-projectile (fn [entity & []] [(:type entity)]))

(defmethod create-projectile [:player] [entity mousePosition]
  (let [proj (projectile (:x entity) (:y entity) 25 10 5 nil)
        vec (gen-vector proj mousePosition)]
    (merge proj vec)))

(defmethod create-projectile [:shooter] [entity mousePosition]
  (let [proj (projectile (:x entity) (:y entity) 15 30 1.5 nil)
        vec (gen-vector proj mousePosition)]
    (merge proj vec)))

(defmethod create-projectile [:kamikaze] [entity target]
  (let [x (:x entity)
        y (:y entity)
        vec (gen-vector entity target)]
    (projectile x y 1 50 0 vec (+ (:last-shot entity) 2))))