(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.entities :as e])
  (:require [simple-jframe.inputManager :as im]))

(defn update-proj [projectiles]
  (reset! e/projectiles (map (fn [p] (e/move p (:more p))) projectiles)))

(defn update-enemies [enemies player]
  (reset! e/enemies (map (fn [e] (e/move e (e/gen-vector e player))) enemies)))

(defn update-player [player inputs]
  (reset! e/player_state (e/move player (im/input-to-vector inputs))))

(defn spawn-projectiles [player inputs mouse]
  (if (im/contains inputs :click) (swap! e/projectiles conj (e/create-projectile player mouse) ) nil))

(defn add-enemy [enemies]
  (if (< (count enemies) 10) (swap! e/enemies conj (e/create-axeman)) nil))

(defn square-collides? [x1 y1 s1 x2 y2 s2] 
  (and (<= (Math/abs (- x1 x2)) (+ s1 s2))
       (<= (Math/abs (- y1 y2)) (+ s1 s2))))

(defn square-circle-collides? [sx sy ss cx cy cr]
  (let [square-boundary-x (if (< cx sx) 
                           (Math/max (- sx ss) cx)
                           (Math/min (+ sx ss) cx))
        square-boundary-y (if (cy sy)
                           (Math/max (- sy ss) cy)
                           (Math/min (+ sy ss) cy))
        dx (- cx square-boundary-x)
        dy (- cy square-boundary-y)
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (<= distance cr)))

(defn collide-entities [player enemies projectiles]
  (doall
   (let [colliding-enemies (filter (fn [x]
                                     (square-collides? (:x player) (:y player) 10 (:x x) (:y x) 10))
                                   enemies)]
     (map #(e/collide player %) colliding-enemies))))

(defn move []
  (let [player @e/player_state
        enemies @e/enemies
        projectiles @e/projectiles
        inputs @im/inputs
        mouse @im/mouse]
    
    (update-proj projectiles)
    (update-enemies enemies player)
    (update-player player inputs)
    (spawn-projectiles player inputs mouse)
    (add-enemy enemies)
    (collide-entities player enemies projectiles)
    nil))
