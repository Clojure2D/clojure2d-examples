(ns neuroevolution.draw
  (:require [neuroevolution.environment :as env]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn update-env
  [env ks]
  (let [rot (cond (ks \l) 0.5 (ks \j) -0.5 :else 0.0)
        acc (cond (ks \i) 0.5 (ks \k) -0.5 :else 0.0)]
    (env/step env acc rot)))

(defn draw-car
  [canvas car]
  (let [v (v/mult (:velocity car) 10)
        ang (double (:angle car))]
    (-> canvas
        (c2d/push-matrix)
        (c2d/translate (:position car))
        (c2d/set-color :red)
        (c2d/line 0.0 0.0 (v 0) (v 1))
        (c2d/rotate (m/- ang m/HALF_PI))
        (c2d/set-color :gray)
        (c2d/line 0.0 0.0 -21.0 0.0)
        (c2d/line 0.0 0.0 21.0 0.0)
        (c2d/set-color :docc/venice-green)
        (c2d/crect 0.0 0.0 20.0 40.0)
        (c2d/set-color :white)
        (c2d/line -5.0 15.0 -5.0 25.0)
        (c2d/line 5.0 15.0 5.0 25.0)
        (c2d/set-stroke 2)
        (c2d/set-color :violet)
        (c2d/ellipse -30.0 0.0 18.0 18.0 true)
        (c2d/set-color :orange)
        (c2d/ellipse 30.0 0.0 18.0 18.0 true)
        (c2d/set-stroke 1.0)
        (c2d/set-stroke-custom {:dash [20 20]})
        (c2d/set-color (c/gray 100 200))
        (c2d/ellipse 0 0 200 200 true)
        (c2d/rotate 0.14154)
        (c2d/line 0.0 0.0 0.0 1000.0)
        (c2d/rotate -0.28308)
        (c2d/line 0.0 0.0 0.0 1000.0)
        (c2d/pop-matrix))))

(defn draw-dot
  [canvas dot]
  (-> canvas
      (c2d/set-color (:col dot))
      (c2d/ellipse (:dot dot) 13.0 13.0)))

(defn print-state
  [canvas env]
  (-> canvas
      (c2d/set-font-attributes 15)
      (c2d/text (str "Time:      " (format "%.2f" (:tm env)) " sec.") 20 20)
      (c2d/text (str "Dots left: " (count (:dots env))) 20 40)))

(defn draw
  [canvas env]
  (if (:game-over? env)
    (-> canvas
        (c2d/set-color :white)
        (c2d/set-font-attributes 100)
        (c2d/translate 400 400)
        (c2d/text "GAME OVER" 0.0 0.0 :center)
        (c2d/set-font-attributes 50)
        (c2d/translate 0 100)
        (c2d/set-color :gray)
        (c2d/text (name (:game-over? env)) 0.0 0.0 :center)
        (c2d/translate 0 100)
        (c2d/set-color :red)
        (c2d/text (str "Final score: " (format "%.2f" (:score env))) 0.0 0.0 :center))
    (-> canvas
        (c2d/set-background (c/gray 20))        
        (print-state env)
        (draw-dot (:dot env))
        (draw-car (:car env)))))

