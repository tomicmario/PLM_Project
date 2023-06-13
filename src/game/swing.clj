(ns game.swing
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Color Dimension Graphics2D])
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.event KeyAdapter KeyEvent MouseEvent])
  (:import [javax.swing.event MouseInputAdapter])
  (:require [game.state :as im]
            [game.renderer :as r]))

(def ^:private frame (JFrame.))
(def ^:private panel (JPanel.))
(def ^:private dimension (Dimension. 1000 500))

(defn set-direction [fn event] ; A bit of a hack, because switch case didn't want to work
  (let  [keycode (.getKeyCode event)]
    (when (= keycode KeyEvent/VK_W) (fn :up))
    (when (= keycode KeyEvent/VK_S) (fn :down))
    (when (= keycode KeyEvent/VK_A) (fn :left))
    (when (= keycode KeyEvent/VK_D) (fn :right))
    (when (= keycode KeyEvent/VK_R) (fn :reset))))

(def mouse-listener
  (proxy [MouseInputAdapter] []
    (mouseMoved [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (.getWidth panel) (.getHeight panel)))
    (mouseDragged [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (.getWidth panel) (.getHeight panel)))))

(def click-listener
  (proxy [MouseInputAdapter] []
    (mousePressed [#^MouseEvent m]
      (im/add-input :click))
    (mouseReleased [#^MouseEvent m]
      (im/remove-input :click))))

(def key-listener
  (proxy [KeyAdapter] []
    ;(keyTyped [#^KeyEvent e] (handlePress e))
    (keyPressed [#^KeyEvent e] (set-direction im/add-input e))
    (keyReleased [#^KeyEvent e] (set-direction im/remove-input e))))

(defn init [title] ; init of ui components
  (doto frame
    (.setTitle title)
    (.setVisible true)
    (.setLocation 0 0)
    (.setMinimumSize (Dimension. 480 360))
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setContentPane panel)
    (.addKeyListener key-listener))

  (doto panel
    (.setSize dimension)
    (.setDoubleBuffered true)
    (.setBackground Color/WHITE)
    (.setPreferredSize dimension)
    (.addMouseMotionListener mouse-listener)
    (.addMouseListener click-listener))
  (.pack frame))

(defn display [] ; Simply displays a render of the current state
  (let [panelGraphics (.getGraphics panel)
        image (r/render (.getWidth panel) (.getHeight panel))]
    (doto ^Graphics2D panelGraphics
      (.drawImage ^BufferedImage image 0 0 nil))))