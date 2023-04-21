(ns simple-jframe.core-test
  (:require [clojure.test :refer :all]
            [simple-jframe.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


(def mousePosition (atom {:x 0, :y 0})) 


(defn inputMap []
  (into [] '(deref inputState)))

(println inputMap)
