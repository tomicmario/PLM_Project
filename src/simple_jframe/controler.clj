(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.player :as p])
  (:require [simple-jframe.ennemy :as e])
  (:require [simple-jframe.projectile :as proj])
  (:require [simple-jframe.inputManager :as im]))

(defn move []
  (p/move-player)
  (if (contains? @im/inputs :click) (swap! proj/projectiles conj (proj/create-projectile @p/player @im/mouse)) nil)
  (if (< (count @e/ennemies) 10) (swap! e/ennemies conj (e/create-axeman)) nil)
  (proj/move-projectiles)
  (e/move-ennemies))