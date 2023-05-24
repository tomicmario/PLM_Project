(ns simple-jframe.core
  (:gen-class)
  (:require [simple-jframe.swing :as display])
  (:require [simple-jframe.controler :as controler]))


(defn move []
  (loop [x 0]
    (controler/move x)
    (Thread/sleep 10)
    (recur (inc x))))

(defn display []
  (display/init "Example Display")
  (while true
    (display/display)))

(defn start-threads []
  (let [move-thread (future (move))
        display-thread (future (display))]
    (deref move-thread)
    (deref display-thread)))

;; original
(defn -main
  "I don't do a whole lot."
  []
  (println "Hello, World!")
  (start-threads))


