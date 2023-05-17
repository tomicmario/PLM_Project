(ns simple-jframe.swing
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Color Dimension Graphics2D])
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt.event KeyAdapter KeyEvent MouseEvent])
  (:import [javax.swing.event MouseInputAdapter])
  (:require [clojure.core.async :as a :refer [<!! >!! <! >!]]))

(def ^:private frame (JFrame.))
(def ^:private panel (JPanel.))
(def ^:private dimension (Dimension. 500 500))
(def image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB))
(def ^:private inputState (atom #{}))
(def ^:private mousePosition (atom {:x 0, :y 0}))
(get @mousePosition :x)
(def player (atom {:x 0, :y 0}))
(def playerVector (atom {:x 0, :y 0}))
(def projectiles (atom []))

(defn calculate-angle [target-x target-y x y]
  (Math/atan2 (- target-x x) (- target-y y)))

(defn createProjectile [player mousePosition] 
  (let [x (:x player)
        y (:y player)
        mouse-x (:x mousePosition)
        mouse-y (:y mousePosition)
        angle ^Double(calculate-angle mouse-x mouse-y x y)] 
    {:x x :y y 
     :vec-x (- (Math/cos angle) (Math/sin angle)) 
     :vec-y (+ (Math/sin angle) (Math/cos angle))}))




(defn inputList []
  (into [] @inputState))

(defn mouseCoordinates []
  @mousePosition)

(inputList)
(defn setDirection [fn event]
  (let  [keycode (.getKeyCode event)]
    (if (= keycode KeyEvent/VK_W) (swap! inputState fn :up)
        nil) 
    (if (= keycode KeyEvent/VK_S) (swap! inputState fn :down)
        nil) 
    (if (= keycode KeyEvent/VK_A) (swap! inputState fn :left)
        nil) 
    (if (= keycode KeyEvent/VK_D) (swap! inputState fn :right)
        nil)))
  

(defn handleRelease [event] 
  (setDirection disj event))
  
(defn handlePress [event] 
  (setDirection conj event))
  
(defn updateVector []
  (let [y (- (if (contains? @inputState :down) 1 0) (if (contains? @inputState :up) 1 0))
        x (- (if (contains? @inputState :right) 1 0) (if (contains? @inputState :left) 1 0))]
    (swap! playerVector assoc-in [:y] y)
    (swap! playerVector assoc-in [:x] x)))

(defn movePlayer []
  (updateVector)
  (swap! player assoc-in [:y] (+ (:y @playerVector) (:y @player)))
  (swap! player assoc-in [:x] (+ (:x @playerVector) (:x @player))))
             

(def mouseListener
  (proxy [MouseInputAdapter] []
    (mouseMoved [#^MouseEvent m]
      (swap! mousePosition assoc-in [:x] (.getX m))
      (swap! mousePosition assoc-in [:y] (.getY m)))
    (mouseDragged [#^MouseEvent m]
      (swap! mousePosition assoc-in [:x] (.getX m))
      (swap! mousePosition assoc-in [:y] (.getY m)))))
  

(def clickListener 
  (proxy [MouseInputAdapter] []
    (mousePressed [#^MouseEvent m]
      (swap! inputState conj :click))
    (mouseReleased [#^MouseEvent m]
      (swap! inputState disj :click))))
  

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
             (keyPressed [#^KeyEvent e] (handlePress e))
             (keyReleased [#^KeyEvent e] (handleRelease e))))) 
          
    (doto panel
          (.setSize dimension)
          (.setDoubleBuffered true)
          (.setBackground Color/WHITE)
          (.setSize dimension)
          (.setPreferredSize dimension)
          (.addMouseMotionListener mouseListener)
          (.addMouseListener clickListener)) 
    (.pack frame))
    

(defn clear []
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/WHITE)
      (.fill (Rectangle2D$Double. 0 0 1000 1000)))))
  

(defn drawCircle [x, y]
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
      (.setColor Color/RED)
      (.fill (Ellipse2D$Double. x y 10 10)))))

(defn drawRect [x, y] 
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
          (.setColor Color/RED)
          (.fill (Rectangle2D$Double. x y 10 10)))))

(defn moveProjectile [projectile]
  {:x (+ (:x projectile) (:vec-x projectile))
   :y (+ (:y projectile) (:vec-y projectile))
   :vec-x (:vec-x projectile)
   :vec-y (:vec-y projectile)})

(defn moveProjectiles []
  (if (empty? @projectiles) nil
  (swap! projectiles (fn [proj] (map moveProjectile proj)))))

(defn drawProjectile [projectile]
  (drawCircle (:x projectile) (:y projectile)))

(defn drawProjectiles []
  (doseq [proj @projectiles] 
     (drawProjectile proj)))

(defn display [] 
  ;(println inputState)
  ;(println mousePosition)
  (let [panelGraphics (.getGraphics panel)]
    (if (contains? @inputState :click ) (swap! projectiles conj (createProjectile @player @mousePosition)) nil)
    (moveProjectiles)
    (drawProjectiles)
    (movePlayer)
    (drawRect (@player :x) (@player :y))
    (doto ^Graphics2D panelGraphics
      (.drawImage ^BufferedImage image 0 0 nil)))) 
    
  