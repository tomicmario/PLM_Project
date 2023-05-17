(ns simple-jframe.swing
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Color Dimension Graphics2D])
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt.event KeyAdapter KeyEvent MouseEvent])
  (:import [javax.swing.event MouseInputAdapter])
  (:require [simple-jframe.inputManager :as im]))

(def ^:private frame (JFrame.))
(def ^:private panel (JPanel.))
(def ^:private dimension (Dimension. 500 500))
(def image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB))

(def player (atom {:x 0, :y 0}))
(def player-vector (atom {:x 0, :y 0}))
(def projectiles (atom []))

(defn calculate-angle [target-x target-y x y]
  (Math/atan2 (- target-x x) (- target-y y)))

(defn create-projectile [player mousePosition] 
  (let [x (:x player)
        y (:y player)
        mouse-x (:x mousePosition)
        mouse-y (:y mousePosition)
        angle (- 315 (calculate-angle mouse-x mouse-y x y))] 
    {:x x :y y 
     :vec-x (- (Math/cos angle) (Math/sin angle)) 
     :vec-y (+ (Math/sin angle) (Math/cos angle))}))

(defn set-direction [fn event]
  (let  [keycode (.getKeyCode event)]
    (if (= keycode KeyEvent/VK_W) (fn :up) nil) 
    (if (= keycode KeyEvent/VK_S) (fn :down) nil) 
    (if (= keycode KeyEvent/VK_A) (fn :left) nil) 
    (if (= keycode KeyEvent/VK_D) (fn :right) nil)))
  
(defn update-vector []
  (let [y (- (if (im/contains :down) 1 0) (if (im/contains :up) 1 0))
        x (- (if (im/contains :right) 1 0) (if (im/contains :left) 1 0))]
    (swap! player-vector assoc-in [:y] y)
    (swap! player-vector assoc-in [:x] x)))

(defn move-player []
  (update-vector)
  (swap! player assoc-in [:y] (+ (:y @player-vector) (:y @player)))
  (swap! player assoc-in [:x] (+ (:x @player-vector) (:x @player))))
             

(def mouse-listener
  (proxy [MouseInputAdapter] []
    (mouseMoved [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m))) 
    (mouseDragged [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m)))))
  
(def click-listener 
  (proxy [MouseInputAdapter] []
    (mousePressed [#^MouseEvent m]
      (im/add-input :click))
    (mouseReleased [#^MouseEvent m]
      (im/remove-input :click))))
  
(defn init [title] 
    (doto frame 
          (.setTitle title)
          (.setVisible true)
          (.setLocationRelativeTo nil) 
          (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
          (.setContentPane panel)
          (.addKeyListener
           (proxy [KeyAdapter] []
             ;(keyTyped [#^KeyEvent e] (handlePress e))
             (keyPressed [#^KeyEvent e] (set-direction im/add-input e))
             (keyReleased [#^KeyEvent e] (set-direction im/remove-input e))))) 
          
    (doto panel
          (.setSize dimension)
          (.setDoubleBuffered true)
          (.setBackground Color/WHITE)
          (.setSize dimension)
          (.setPreferredSize dimension)
          (.addMouseMotionListener mouse-listener)
          (.addMouseListener click-listener)) 
    (.pack frame))
    

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

(defn move-projectile [projectile]
  {:x (+ (:x projectile) (:vec-x projectile))
   :y (+ (:y projectile) (:vec-y projectile))
   :vec-x (:vec-x projectile)
   :vec-y (:vec-y projectile)})

(defn move-projectiles []
  (if (empty? @projectiles) nil
  (swap! projectiles (fn [proj] (map move-projectile proj)))))

(defn draw-projectile [projectile]
  (draw-circle (:x projectile) (:y projectile)))

(defn draw-projectiles []
  (doseq [proj @projectiles] 
     (draw-projectile proj)))

(defn display [] 
  ;(println inputState)
  ;(println mousePosition)
  (let [panelGraphics (.getGraphics panel)]
    (if (contains? @im/inputs :click ) (swap! projectiles conj (create-projectile @player @im/mouse)) nil)
    (move-projectiles)
    (draw-projectiles)
    (move-player)
    (draw-rect (@player :x) (@player :y))
    (doto ^Graphics2D panelGraphics
      (.drawImage ^BufferedImage image 0 0 nil)))) 
    
  