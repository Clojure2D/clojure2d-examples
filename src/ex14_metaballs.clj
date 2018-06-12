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

(deftype Ball [^double x ^double y ^double vx ^double vy ^double radius ^Vec3 color])

(defn make-ball
  ""
  ^Ball []
  (Ball. (r/irand SIZE)
         (r/irand SIZE)
         (r/irand 1 7)
         (r/irand 1 7)
         (r/irand 40 55)
         (Vec3. (r/drand 255) (r/drand 255) (r/drand 255))))

(defn direction
  ""
  ^double [^double p ^double v]
  (if (bool-or (> p SIZE) (neg? p))
    (- v)
    v))

(defn move
  ""
  [^Ball ball]
  (let [vx (direction (.x ball) (.vx ball))
        vy (direction (.y ball) (.vy ball))]
    (Ball. (+ (.x ball) vx) (+ (.y ball) vy) vx vy (.radius ball) (.color ball))))

(defn influence 
  ""
  ^double [^Ball ball ^double px ^double py]
  (let [dx (- (.x ball) px)
        dy (- (.y ball) py)]
    (/ (.radius ball) (+ m/EPSILON (m/hypot-sqrt dx dy)))))

(defn compute-color
  ""
  ^Vec3 [x y ^Vec3 cur ^Ball ball]
  (let [infl (influence ball x y)
        ^Vec3 rgb (.color ball)]
    (Vec3. (+ (.x cur) (* infl (.x rgb)))
           (+ (.y cur) (* infl (.y rgb)))
           (+ (.z cur) (* infl (.z rgb))))))

(defn draw
  ""
  [canvas balls]
  (loop [y (int 0)]
    (loop [x (int 0)]
    
      (let [^Vec3 c (reduce (partial compute-color x y) (Vec3. 0.0 0.0 0.0) balls)]
        (set-color canvas c)
        (rect canvas x y 2 2))
    
      (when (< x (- SIZE 2)) (recur (+ 2 x))))
    (when (< y (- SIZE 2)) (recur (+ 2 y)))))

(defn draw-balls
  ""
  [n canvas window framecount result]
  (let [balls (map move (or result (take n (repeatedly make-ball))))]
    (draw canvas balls)
    balls))

(defn example-14
  ""
  [n]
  (let [c (canvas SIZE SIZE :low)]
    (show-window {:canvas c
                  :window-name "metaballs"
                  :draw-fn (partial draw-balls n)
                  :refresher :fast})))

(def window (example-14 (r/irand 2 6)))

(save window "results/ex14/metaballs.jpg")

;; [[../results/ex14/metaballs.jpg]]

