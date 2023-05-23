(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.entities :as e])
  (:require [simple-jframe.inputManager :as im]))

(defn update-proj [projectiles]
  (reset! e/projectiles (map (fn [p] (e/move p (:more p))) projectiles)))

(defn update-enemies [enemies player]
  (reset! e/enemies (map (fn [e] (e/move e (e/gen-vector player e))) enemies)))

(defn update-player [player]
  (reset! e/player_state (e/move player (im/input-to-vector))))

(comment
(defn spawn-projectile [player]
  (if (contains? @im/inputs :click) (swap! e/projectiles conj (e/create-projectile player @im/mouse)) nil))
)
(defn move []
  (let [player @e/player_state
        enemies @e/enemies
        projectiles @e/projectiles]
    (update-proj projectiles)
    (update-enemies enemies player)
    (update-player player)))

(update-proj @e/projectiles)
(update-enemies @e/enemies @e/player_state)
(update-player @e/player_state)
(move)