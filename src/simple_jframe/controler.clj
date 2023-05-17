(ns simple-jframe.controler
  (:gen-class)
  (:require [simple-jframe.player :as p])
  (:require [simple-jframe.projectile :as proj])
  (:require [simple-jframe.inputManager :as im]))

(defn move []
  (p/move-player)
  (if (contains? @im/inputs :click) (swap! proj/projectiles conj (proj/create-projectile @p/player @im/mouse)) nil)
  (proj/move-projectiles))
