(ns GG.M.M-5-1-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]))

(def ^:const recursion-level 6)
(def ^:const start-radius 200)

(defn draw-branch
  ""
  [canvas x y radius level]
  (-> canvas
      (set-stroke (* level 2) :square)
      (set-color 0 130 164 100)
      (arc x y (* radius 2) (* radius 2) 0 (- m/PI))
      (set-color :black)
      (ellipse x y (* level 1.5) (* level 1.5)))
  (when (pos? level)
    (draw-branch canvas (- x radius) (+ y (/ radius 2)) (/ radius 2) (dec level))
    (draw-branch canvas (+ x radius) (+ y (/ radius 2)) (/ radius 2) (dec level))))

(def window (show-window {:canvas (canvas 800 800)
                          :setup (fn [canvas _]
                                   (-> canvas
                                       (set-background :white)
                                       (translate (/ (width canvas) 2)
                                                  (/ (width canvas) 2))
                                       (draw-branch 0 0 start-radius recursion-level)))}))
