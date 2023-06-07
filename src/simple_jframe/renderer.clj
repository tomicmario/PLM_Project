(ns simple-jframe.renderer
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt Color Graphics2D Font])
  (:require [simple-jframe.entities :as e]))

(def image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB))
(def font (Font. "TimesRoman" Font/BOLD 20))

(defn clear []
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/WHITE)
      (.fill (Rectangle2D$Double. 0 0 1000 1000)))))

(defn draw-shape [color shape]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor color)
      (.fill shape))))

(defn draw-circle [x y width height]
  (let [shape (Ellipse2D$Double. x y width height)]
    (draw-shape Color/RED shape)))

(defn draw-rect [x y w h]
  (let [shape (Rectangle2D$Double. x y w h)]
    (draw-shape Color/RED shape)))

(defn draw-label [x y texte color]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setPaint color)
      (.setFont font)
      (.drawString texte x y))))

(defn draw-interface [player]
    (draw-label 10 20 (str "Life : " (:health player)) Color/BLACK))

(defn draw-default-entity [entity fn]
  (let [x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))]
    (fn x y (:width entity) (:height entity))))

(defmulti draw (fn [entity] [(:type entity)]))

(defmethod draw [:projectile] [projectile]
  (draw-default-entity projectile draw-circle))

(defmethod draw [:axe-man] [enemy]
  (draw-default-entity enemy draw-rect))

(defmethod draw [:player] [player]
  (draw-default-entity player draw-rect))

(defn render [] 
  (let [projectiles @e/projectiles
        player @e/player_state
        enemies @e/enemies]
    (clear)
    (run! draw projectiles)
    (draw player)
    (draw-interface player)
    (run! draw enemies) 
    image))
