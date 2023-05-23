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
    nil))
