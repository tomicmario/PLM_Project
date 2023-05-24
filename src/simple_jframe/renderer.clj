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

(defn draw-circle [x y width height]
  (let [shape (Ellipse2D$Double. x y width height)]
    (draw Color/RED shape)))

(defn draw-rect [x y w h]
  (let [shape (Rectangle2D$Double. x y w h)]
    (draw Color/RED shape)))

(defn draw-default-entity [entity fn]
  (let [x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))]
    (fn x y (:width entity) (:height entity))))

(defn draw-projectile [projectile]
  (draw-default-entity projectile draw-circle))

(defn draw-enemy [enemy]
  (draw-default-entity enemy draw-rect))

(defn draw-player [player]
  (draw-default-entity player draw-rect))

(defn draw-projectiles [projectiles]
  (run! draw-projectile projectiles))

(defn draw-enemies [enemies]
  (run! draw-enemy enemies))

(defn render [] 
  (clear)
  (draw-projectiles @e/projectiles)
  (draw-player @e/player_state)
  (draw-enemies @e/enemies)
  image)
