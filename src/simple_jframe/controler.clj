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
     :inputs inputs :mouse mouse :timestamp timestamp}))

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

(defn shoot [state]
  (let [proj (e/create-projectile (:player state) (:mouse state))
        new-proj (conj (:projectiles state) proj)]
    (-> state
        (assoc-in [:projectiles] new-proj)
        (assoc-in [:player] (e/update-timestamp (:player state) (:timestamp state))))))

(defn player-shoot [state]
  (let [player (:player state)
        timestamp (:timestamp state)]
  (if (and (im/contains (:inputs state) :click) (can-shoot? player timestamp)) 
    (shoot state) 
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

(defn remove-far-projectiles [state]
  (assoc-in state [:projectiles] (filter (fn [e] (close-enough? e)) (:projectiles state))))

(defn square-collides? [x1 y1 s1 x2 y2 s2] 
  (and (<= (Math/abs (- x1 x2)) (+ s1 s2))
       (<= (Math/abs (- y1 y2)) (+ s1 s2))))

(defn square-circle-collides? [sx sy ss cx cy cr]
  (let [square-boundary-x (if (< cx sx) 
                           (if (< (- sx ss) cx)
                             (- sx ss)
                             cx)
                           (if (< (+ sx ss) cx)
                             (+ sx ss)
                             cx))
        square-boundary-y (if (< cy sy)
                           (if (< (- sy ss) cy)
                             (- sy ss)
                             cy)
                           (if (< (+ sy ss) cy)
                             (+ sy ss)
                             cy))
        dx (- cx square-boundary-x)
        dy (- cy square-boundary-y)
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (<= distance cr)))

(square-circle-collides? 10 10 10 30 30 10)

(defn collide-entities [player enemies projectiles]
  (doall
   (let [colliding-enemies (filter (fn [x]
                                     (square-collides? (:x player) (:y player) 10 (:x x) (:y x) 10))
                                   enemies)]
     (map #(e/collide player %) colliding-enemies))
   (let [colliding (filter (fn [[enemy projectile]]
                             (square-circle-collides? (:x enemy) (:y enemy) 10 (:x projectile) (:y projectile) 10))
                           (for [enemy enemies
                                 projectile projectiles]
                             [enemy projectile]))]
     (doseq [[enemy projectile] colliding]
       (e/collide enemy projectile)))))

(defn move [timestamp]
  (-> (get-state timestamp)
      (move-proj)
      (move-player)
      (move-enemies)
      (correct-positions)
      (add-enemy)
      (player-shoot)
      (remove-far-projectiles)
      (save-state)))
