(ns simple-jframe.projectile
  (:gen-class))

(def projectiles (atom []))

(defn calculate-angle [target-x target-y x y]
  (Math/atan2 (- target-x x) (- target-y y)))

(defn create-projectile [player mousePosition]
  (let [x (:x player)
        y (:y player)
        mouse-x (:x mousePosition)
        mouse-y (:y mousePosition)
        angle (- 315 (calculate-angle mouse-x mouse-y x y))]
    {:x x :y y
     :vec-x (- (Math/cos angle) (Math/sin angle))
     :vec-y (+ (Math/sin angle) (Math/cos angle))}))

(defn move-projectile [projectile]
  {:x (+ (:x projectile) (:vec-x projectile))
   :y (+ (:y projectile) (:vec-y projectile))
   :vec-x (:vec-x projectile)
   :vec-y (:vec-y projectile)})

(defn move-projectiles []
  (if (empty? @projectiles) nil
      (swap! projectiles (fn [proj] (map move-projectile proj)))))