(ns simple-jframe.renderer
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt Color Graphics2D])
  (:require [simple-jframe.player :as p])
  (:require [simple-jframe.projectile :as proj]))

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
  (doseq [proj @projectiles]
    (draw-projectile proj)))

(defn draw-player [player]
  (draw-rect (:x @player) (:y @player)))

(defn render []
  (clear)
  (draw-projectiles proj/projectiles)
  (draw-player p/player)
  image)