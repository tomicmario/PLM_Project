(ns simple-jframe.core
  (:gen-class)
  (:require [simple-jframe.swing :as display])
  (:require [simple-jframe.controler :as controler]))



;; original
(defn -main
  "I don't do a whole lot."
  []
  (println "Hello, World!")


  (display/init "Example Display")
  (loop [x 0]
    (controler/move) 
    (display/display)
    (Thread/sleep 16)
    (recur (+ x 1))))


