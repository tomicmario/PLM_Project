(ns game.renderer
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double AffineTransform])
  (:import [java.awt Color Graphics2D Graphics Font])
  (:import [javax.imageio ImageIO])
  (:require [game.state :as state]
            [clojure.java.io :as io]))

(def font (Font. "TimesRoman" Font/BOLD 20))

; ENTITY IMAGES
(defn get-image-from-file [path]
  (ImageIO/read (io/file path)))

(def bg (get-image-from-file "resources/Space001.png"))

(def player-image (get-image-from-file "resources/player_ship.PNG"))

(def projectile-image (get-image-from-file "resources/energy_ball.png"))

(def kamikaze-image (get-image-from-file "resources/kamikaze.png"))

(def shooter-image (get-image-from-file "resources/shooter.png"))
; END ENTITY IMAGES

; BASIC DRAWING FUNCTIONS
(defn draw-label [image x y text color]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setPaint color)
      (.setFont ^Font font)
      (.drawString ^String text ^Integer x ^Integer y)))
  image)

(defn draw-image [image x y w h path]
    (.drawImage ^Graphics image path w h x y nil))

(defn new-image [x y]
  (let [image (BufferedImage. x y BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/WHITE)
      (.fill (Rectangle2D$Double. 0 0 x y)))
    (draw-image ^Graphics graphics x y 0 0 bg)
    image))

(defn draw-shape [image color shape]
  (let [c (if (nil? color) Color/RED color)
        graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor c)
      (.fill shape)))
  image)

(defn draw-rect [image x y w h & [color]]
  (let [shape (Rectangle2D$Double. x y w h)]
    (draw-shape image color shape))
  image)

(defn draw-image-rotation [image entity disp]
  (let [angle (- (:angle entity) Math/PI (/ Math/PI 4))
        x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))
        graphics (.createGraphics image)
        old-angle (.getTransform ^Graphics2D graphics)]
    (.translate ^Graphics graphics (:x entity) (:y entity))
    (.rotate ^Graphics2D graphics angle)
    (.translate ^Graphics graphics (- (:x entity)) (- (:y entity)))
    (draw-image ^Graphics graphics (:width entity) (:height entity) x y disp)
    (.setTransform ^Graphics2D graphics ^AffineTransform old-angle)
    image))

(defn draw-image-ent [image entity disp]
  (let [x (- (:x entity) (/ (:width entity) 2))
        y (- (:y entity) (/ (:height entity) 2))
        graphics (.createGraphics image)]
    (draw-image ^Graphics graphics (:width entity) (:height entity) x y disp) 
    image))

(defmulti draw (fn [image entity] [(:type entity)]))

(defmethod draw [:projectile] [image projectile]
  (draw-image-ent image projectile projectile-image))

(defmethod draw [:kamikaze] [image enemy]
  (draw-image-rotation image enemy kamikaze-image))

(defmethod draw [:shooter] [image enemy]
  (draw-image-rotation image enemy shooter-image))

(defmethod draw [:player] [image player]
  (draw-image-rotation image player player-image))
; END BASIC DRAW FUNCTIONS

; ADAPTING STATE TO MATCH DISPLAY
(defn get-health-ratio [entity]
  (/ (:health entity) (:max-health entity)))

(defn adapt-ratio [entity x-ratio y-ratio]
  (-> entity
      (assoc :x (* (:x entity) x-ratio))
      (assoc :y (* (:y entity) y-ratio))
      (assoc :width (* (:width entity) x-ratio))
      (assoc :height (* (:height entity) y-ratio))
      (assoc :angle (- (/ Math/PI 4) (:angle entity)))))

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
; END ADAPTING STATE FOR DISPLAY

; RENDER STEPS
(defn draw-collection [image coll]
  (let [reducer (fn [e] (draw image e))]
    (when-not (or (nil? coll) (empty? coll))
      (run! reducer coll)))
  image)

(defn display-game-over [image state]
  (let [bounds (:bounds state)
        middle-x (/ (:disp-x bounds) 2)
        middle-y (/ (:disp-y bounds) 2)]
    (if (> (:health (:player state)) 0)
      image
      (draw-label image middle-x middle-y "Game Over" Color/BLACK))))

(defn draw-healthbar [image entity]
  (let [x  (- (:x entity) (/ (:width entity) 2))
        y (+ (- (:y entity) (:height entity)) 5)
        ratio (get-health-ratio entity)
        width (* ratio (:width entity))
        c (if (< ratio 0.3) Color/RED Color/GREEN)]
    (draw-rect image x y width 5 c)))

(defn draw-interface [image state]
  (let [player (:player state)
        enemies (:enemies state)]
    (run! (fn [e] (draw-healthbar image e)) enemies)
    (-> image
        (draw-healthbar player)
        (draw-label 10 20 (str "Score : " (:score state)) Color/BLACK)
        (display-game-over state))))
; END RENDER STEPS

; Render the state, takes requires to know what the maximum resolution of the display is with x and y
(defn render [x y]
  (let [raw-state @state/entity-state
        display-state (transform-state raw-state x y)]
    (-> (new-image x y)
        (draw (:player display-state))
        (draw-collection (:e-proj display-state))
        (draw-collection (:p-proj display-state))
        (draw-collection (:enemies display-state))
        (draw-interface display-state))))