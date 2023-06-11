(ns game.renderer
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt Color Graphics2D Font])
  (:require [game.state :as state]))

(def font (Font. "TimesRoman" Font/BOLD 20))

(defn new-image [x y]
  (let [image (BufferedImage. x y BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/WHITE)
      (.fill (Rectangle2D$Double. 0 0 x y)))
    image))

(defn draw-shape [image color shape]
  (let [c (if (nil? color) Color/RED color)
        graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor c)
      (.fill shape)))
  image)

(defn draw-circle [image x y width height & [color]]
  (let [shape (Ellipse2D$Double. x y width height)]
    (draw-shape image  color shape))
  image)

(defn draw-rect [image x y w h & [color]]
  (let [shape (Rectangle2D$Double. x y w h)]
    (draw-shape image color shape))
  image)

(defn draw-default-entity [image entity fn & [color]]
  (let [x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))]
    (fn image x y (:width entity) (:height entity) color)))

(defn get-health-ratio [entity]
  (/ (:health entity) 100))

(defn draw-healthbar [image entity]
  (let [x  (- (:x entity) (/ (:width entity) 2))
        y (+ (- (:y entity) (:height entity)) 5)
        width (* (get-health-ratio entity) (:width entity))
        c (if (< (:health entity) 25) Color/RED Color/GREEN)]
    (draw-rect image x y width 5 c)))

(defn draw-default-ennemy [image ennemy fn]
  (draw-default-entity image ennemy fn))

(defmulti draw (fn [image entity] [(:type entity)]))

(defmethod draw [:projectile] [image projectile]
  (draw-default-entity image projectile draw-circle Color/ORANGE))

(defmethod draw [:axe-man] [image enemy]
  (draw-default-ennemy image enemy draw-rect))

(defmethod draw [:shooter] [image enemy]
  (draw-default-entity image enemy draw-rect Color/MAGENTA))

(defmethod draw [:player] [image player]
  (draw-default-entity image player draw-rect Color/BLUE))

(defn adapt-ratio [entity x-ratio y-ratio]
  (-> entity
      (assoc :x (* (:x entity) x-ratio))
      (assoc :y (* (:y entity) y-ratio))
      (assoc :width (* (:width entity) x-ratio))
      (assoc :height (* (:height entity) y-ratio))))

(defn transform-state [state x y]
  (let [bounds (:bounds state)
        x-ratio (/ x (:max-x bounds))
        y-ratio (/ y (:max-y bounds))
        fn (fn [e] (adapt-ratio e x-ratio y-ratio))]
    (-> state
        (assoc :player (fn (:player state)))
        (assoc :p-proj (mapv fn (:p-proj state)))
        (assoc :e-proj (mapv fn (:e-proj state)))
        (assoc :enemies (mapv fn (:enemies state)))
        (assoc-in [:bounds :disp-x] (* x-ratio (:max-x bounds)))
        (assoc-in [:bounds :disp-y] (* y-ratio (:max-y bounds))))))

(defn draw-collection [image coll]
  (let [reducer (fn [e] (draw image e))]
    (when-not (or (nil? coll) (empty? coll))
      (run! reducer coll)))
  image)

(defn draw-label [image x y text color]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setPaint color)
      (.setFont ^Font font)
      (.drawString ^String text ^Integer x ^Integer y)))
  image)

(defn display-game-over [image state]
  (let [bounds (:bounds state)
        middle-x (/ (:disp-x bounds) 2)
        middle-y (/ (:disp-y bounds) 2)]
    (if (> (:health (:player state)) 0)
      image
      (draw-label image middle-x middle-y "Game Over" Color/BLACK))))

(defn draw-interface [image state]
  (let [player (:player state)
        enemies (:enemies state)] 
    (run! (fn [e] (draw-healthbar image e)) enemies)
    (-> image
        (draw-healthbar player)
        (draw-label 10 20 (str "Score : " (:score state)) Color/BLACK)
        (display-game-over state))))

(defn render [x y]
  (let [raw-state @state/entity-state
        display-state (transform-state raw-state x y)]
    (-> (new-image x y)
        (draw (:player display-state))
        (draw-collection (:e-proj display-state))
        (draw-collection (:p-proj display-state))
        (draw-collection (:enemies display-state))
        (draw-interface display-state))))
