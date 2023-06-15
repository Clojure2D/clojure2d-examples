(ns GG.M.M-5-1-01
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]))

(def ^:const recursion-level 6)
(def ^:const start-radius 200)

(defn draw-branch
  [canvas x y radius level]
  (-> canvas
      (c2d/set-stroke (* level 2) :square)
      (c2d/set-color 0 130 164 100)
      (c2d/arc x y (* radius 2) (* radius 2) 0 (- m/PI))
      (c2d/set-color :black)
      (c2d/ellipse x y (* level 1.5) (* level 1.5)))
  (when (pos? level)
    (draw-branch canvas (- x radius) (+ y (/ radius 2)) (/ radius 2) (dec level))
    (draw-branch canvas (+ x radius) (+ y (/ radius 2)) (/ radius 2) (dec level))))

(def window (c2d/show-window {:canvas (c2d/canvas 800 800)
                            :setup (fn [canvas _]
                                     (-> canvas
                                         (c2d/set-background :white)
                                         (c2d/translate (/ (c2d/width canvas) 2)
                                                        (/ (c2d/width canvas) 2))
                                         (draw-branch 0 0 start-radius recursion-level)))}))
