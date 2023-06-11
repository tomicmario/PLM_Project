(ns game.core
  (:gen-class)
  (:require [game.swing :as display])
  (:require [game.controler :as controler]))

(def frame-time-ms 10)

(defn simulate-game []
  (let [last-render-time (atom (System/currentTimeMillis))]
    (while true
      (let [current-time (System/currentTimeMillis)
            time-diff (- current-time @last-render-time)]
        ;(when (> time-diff frame-time-ms) (println (str" Slow frame by " (- time-diff frame-time-ms) "ms")))
        (when (>= time-diff frame-time-ms)
          (controler/next-tick)
          (reset! last-render-time current-time))
        (when (< time-diff frame-time-ms) 
          (Thread/sleep 1))))))

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


