;; https://gist.github.com/yogthos/3411106

(ns ex14-metaballs
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long SIZE 600)
(def ^:const ^long SIZE-2 (- SIZE 2))
(def ^:const zero (Vec3. 0.0 0.0 0.0))

(deftype Ball [^double x ^double y ^double vx ^double vy ^double radius ^Vec3 color])

(defn make-ball
  ^Ball []
  (Ball. (r/irand SIZE)
         (r/irand SIZE)
         (r/irand 1 7)
         (r/irand 1 7)
         (r/irand 10 65)
         (Vec3. (r/drand 255.0) (r/drand 255.0) (r/drand 255.0))))

(defmacro direction
  [p v]
  `(if (or (> ~p SIZE) (neg? ~p)) (- ~v) ~v))

(defn move
  ^Ball [^Ball ball]
  (let [vx (direction (.x ball) (.vx ball))
        vy (direction (.y ball) (.vy ball))]
    (Ball. (+ (.x ball) vx) (+ (.y ball) vy) vx vy (.radius ball) (.color ball))))

(defn influence
  ^double [^Ball ball ^double px ^double py]
  (/ (.radius ball) (+ m/EPSILON (m/dist (.x ball) (.y ball) px py))))

(defn compute-color
  ^Vec3 [x y ^Vec3 cur ^Ball ball]
  (let [infl (influence ball x y)
        rgb (.color ball)]
    (v/add cur (v/mult rgb infl))))

(defn draw
  [canvas balls]
  (loop [y (int 0)]
    (loop [x (int 0)]
      
      (let [^Vec3 c (reduce (fn [current ball]
                              (compute-color x y current ball)) zero balls)]
        (set-color canvas c)
        (rect canvas x y 2 2))
      
      (when (< x SIZE-2) (recur (+ 2 x))))
    (when (< y SIZE-2) (recur (+ 2 y)))))

(defn draw-balls
  [n canvas window framecount result]
  (let [balls (map move result)]
    (draw canvas balls)
    balls))

(defn example-14
  [n]
  (let [c (canvas SIZE SIZE :low)]
    (show-window {:canvas c
                  :window-name "metaballs"
                  :draw-fn (partial draw-balls n)
                  :draw-state (repeatedly n make-ball)
                  :refresher :fast})))

(def window (example-14 (r/irand 2 6)))

(comment save window "results/ex14/metaballs.jpg")

;; [[../results/ex14/metaballs.jpg]]

