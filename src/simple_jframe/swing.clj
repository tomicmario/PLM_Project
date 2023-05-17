(ns simple-jframe.swing
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Color Dimension Graphics2D])
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double])
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

(updateVector)

(defn movePlayer []
  (updateVector)
  (swap! player assoc-in [:y] (+ (:y @playerVector) (:y @player)))
  (swap! player assoc-in [:x] (+ (:x @playerVector) (:x @player))))

(movePlayer)
             

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
  

(defn drawRect [x, y] 
  (let [graphics (.createGraphics image)]
    (doto ^Graphics2D graphics
          (.setColor Color/RED)
          (.fill (Rectangle2D$Double. x y 10 10)))))
  

(defn display [] 
  ;(println inputState)
  ;(println mousePosition)
  (let [panelGraphics (.getGraphics panel)] 
    (movePlayer)
    (drawRect (@player :x) (@player :y))
    (doto ^Graphics2D panelGraphics
      (.drawImage ^BufferedImage image 0 0 nil)))) 
    
  