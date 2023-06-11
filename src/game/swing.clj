(ns game.swing
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Color Dimension Graphics2D])
  (:import [java.awt.image BufferedImage])
  (:import [java.awt.event KeyAdapter KeyEvent MouseEvent])
  (:import [javax.swing.event MouseInputAdapter])
  (:require [game.state :as im]
            [game.swing :as display]
            [game.renderer :as r]))

(def ^:private frame (JFrame.))
(def ^:private panel (JPanel.))
(def ^:private dimension (Dimension. 1000 500))

(defn set-direction [fn event]
  (let  [keycode (.getKeyCode event)]
    (if (= keycode KeyEvent/VK_W) (fn :up) nil)
    (if (= keycode KeyEvent/VK_S) (fn :down) nil)
    (if (= keycode KeyEvent/VK_A) (fn :left) nil)
    (if (= keycode KeyEvent/VK_D) (fn :right) nil)
    (if (= keycode KeyEvent/VK_R) (fn :reset) nil)))

(def mouse-listener
  (proxy [MouseInputAdapter] []
    (mouseMoved [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (.getWidth dimension) (.getHeight dimension)))
    (mouseDragged [#^MouseEvent m]
      (im/update-mouse (.getX m) (.getY m) (.getWidth dimension) (.getHeight dimension)))))

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

(defn init [title]
  (doto frame
    (.setTitle title)
    (.setVisible true)
    (.setLocationRelativeTo nil)
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

(defn display []
  (let [panelGraphics (.getGraphics panel)
        image (r/render (.getWidth dimension) (.getHeight dimension))]
    (doto ^Graphics2D panelGraphics
      (.drawImage ^BufferedImage image 0 0 nil))))