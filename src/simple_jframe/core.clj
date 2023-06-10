(ns simple-jframe.core
  (:gen-class)
  (:require [simple-jframe.swing :as display])
  (:require [simple-jframe.controler :as controler]))

(def frame-time-ms 10)

(defn simulate-game []
  (let [last-render-time (atom (System/currentTimeMillis))]
    (while true
      (let [current-time (System/currentTimeMillis)]
        (when (>= (- current-time @last-render-time) frame-time-ms)
          (controler/next-tick)
          (reset! last-render-time current-time))))))

(defn render-game []
  (display/init "Projet PLM")
  (while true
    (display/display)))

(defn start-threads []
  (let [move-thread (future (simulate-game))
        display-thread (future (render-game))]
    (deref move-thread)
    (deref display-thread)))

;; launch the game
(defn -main []
  (start-threads))


