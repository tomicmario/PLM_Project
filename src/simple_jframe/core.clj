(ns simple-jframe.core
  (:gen-class)
  (:require [simple-jframe.swing :as display]))


;; original
(defn -main
  "I don't do a whole lot."
  []
  (println "Hello, World!")

  (display/init "Example Display") 
  (loop [x 0] 
          (display/clear)
          ;(display/drawRect x 10) 
          (display/display)
          (Thread/sleep 16)
          (recur (+ x 1)))
  )


