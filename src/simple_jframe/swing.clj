(ns simple-jframe.swing
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Color Dimension Graphics2D])
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.geom Rectangle2D$Double Ellipse2D$Double])
  (:import [java.awt.event KeyAdapter KeyEvent MouseEvent])
  (:import [javax.swing.event MouseInputAdapter])
  (:require [simple-jframe.inputManager :as im])
  (:require [simple-jframe.player :as p])
  (:require [simple-jframe.projectile :as proj])
  )

(def ^:private frame (JFrame.))
(def ^:private panel (JPanel.))
(def ^:private dimension (Dimension. 500 500))
(def image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB))

(defn set-direction [fn event]
  (let  [keycode (.getKeyCode event)]
    (if (= keycode KeyEvent/VK_W) (fn :up) nil) 
    (if (= keycode KeyEvent/VK_S) (fn :down) nil) 
    (if (= keycode KeyEvent/VK_A) (fn :left) nil) 
    (if (= keycode KeyEvent/VK_D) (fn :right) nil)))
  
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

(defn draw-projectile [projectile]
  (draw-circle (:x projectile) (:y projectile)))

(defn draw-projectiles [projectiles]
  (doseq [proj @projectiles]
    (draw-projectile proj)))

(defn draw-player [player]
  (draw-rect (:x @player) (:y @player)))

(defn render []
  (draw-projectiles proj/projectiles)
  (draw-player p/player))

(defn display [] 
  (render)
  (let [panelGraphics (.getGraphics panel)] 
    (doto ^Graphics2D panelGraphics
      (.drawImage ^BufferedImage image 0 0 nil)))) 
    
  