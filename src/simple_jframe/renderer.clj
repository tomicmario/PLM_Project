(ns simple-jframe.renderer
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt Color Graphics2D])
  (:require [simple-jframe.entities :as e]))

(def image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB))

(defn clear []
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/WHITE)
      (.fill (Rectangle2D$Double. 0 0 1000 1000)))))

(defn draw [color shape]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor color)
      (.fill shape))))

(defn draw-circle [x y]
  (let [shape (Ellipse2D$Double. x y 10 10)]
    (draw Color/RED shape)))

(defn draw-rect [x y]
  (let [shape (Rectangle2D$Double. x y 10 10)]
    (draw Color/RED shape)))

(defn draw-projectile [projectile]
  (draw-circle (:x projectile) (:y projectile)))

(defn draw-projectiles [projectiles]
  (run! draw-projectile projectiles))

(defn draw-enemy [enemy]
  (draw-rect (:x enemy) (:y enemy)))

(defn draw-enemies [enemies]
  (run! draw-enemy enemies))

(defn draw-player [player]
  (draw-rect (:x player) (:y player)))

(defn render [] 
  (clear)
  (draw-projectiles @e/projectiles)
  (draw-player @e/player_state)
  (draw-enemies @e/enemies)
  image)
