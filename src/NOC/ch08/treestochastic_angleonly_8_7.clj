(ns examples.NOC.ch08.treestochastic-angleonly-8-7
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(def cnvs (canvas 800 200))
(def window (show-window cnvs "Tree Stochastic - angle only 8_7"))

(defn branch
  ""
  [canvas ^double h]
  (let [sw (m/norm h 2.0 120.0 1.0 5.0)
        newh (* 0.66 h)
        theta (r/drand m/THIRD_PI)]
    (-> canvas
        (set-stroke sw)
        (line 0 0 0 (- h))
        (translate 0 (- h)))

    (when (> newh 2.0)
      (-> canvas
          (push-matrix)
          (rotate theta)
          (branch newh)
          (pop-matrix)
          (push-matrix)
          (rotate (- theta))
          (branch newh)
          (pop-matrix))))
  canvas)

(defn new-tree
  "Create new tree"
  []
  (with-canvas-> cnvs
    (set-background :white)
    (set-color :black)
    (text "Click mouse to generate a new tree" 10 (- (height cnvs) 10))
    (translate (/ (width cnvs) 2.0) (height cnvs))
    (branch 60.0)))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _]
  (new-tree))

(new-tree)
