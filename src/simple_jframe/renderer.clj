(ns simple-jframe.renderer
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt Color Graphics2D Font])
  (:require [simple-jframe.state :as s]))

(def image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB))
(def font (Font. "TimesRoman" Font/BOLD 20))

(defn clear []
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/WHITE)
      (.fill (Rectangle2D$Double. 0 0 1000 1000)))))

(defn draw-shape [color shape]
  (let [c (if (nil? color) Color/RED color)
        graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor c)
      (.fill shape))))

(defn draw-circle [x y width height & [color]]
  (let [shape (Ellipse2D$Double. x y width height)]
    (draw-shape color shape)))

(defn draw-rect [x y w h & [color]]
  (let [shape (Rectangle2D$Double. x y w h)]
    (draw-shape color shape)))

(defn draw-label [x y text color]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setPaint color)
      (.setFont ^Font font)
      (.drawString ^String text x y))))

(defn draw-interface [player]
  (draw-label 10 20 (str "Life : " (:health player)) Color/BLACK))

(defn draw-default-entity [entity fn & [color]]
  (let [x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))]
    (fn x y (:width entity) (:height entity) color)))

(defn get-health-ratio [entity]
  (/ (:health entity) 100))

(defn draw-healthbar [entity]
  (let [x  (- (:x entity) (/ (:width entity) 2))
        y (+ (- (:y entity) (:height entity)) 5)
        width (* (get-health-ratio entity) (:width entity))
        c (if (< (:health entity) 25) Color/RED Color/GREEN)]
    (draw-rect x y width 5 c)))

(defn draw-default-ennemy [ennemy fn]
  (draw-healthbar ennemy)
  (draw-default-entity ennemy fn))

(defmulti draw (fn [entity] [(:type entity)]))

(defmethod draw [:projectile] [projectile]
  (draw-default-entity projectile draw-circle Color/ORANGE))

(defmethod draw [:axe-man] [enemy]
  (draw-default-ennemy enemy draw-rect))

(defmethod draw [:player] [player]
  (draw-default-entity player draw-rect Color/BLUE))

(defn render []
  (let [state (s/get-state)
        enemy-projectiles (:e-proj state)
        player-projectiles (:p-proj state)
        player (:player state)
        enemies (:enemies state)]
    (clear)
    (run! draw enemy-projectiles)
    (run! draw player-projectiles)
    (draw player)
    (draw-interface player)
    (run! draw enemies)
    image))
