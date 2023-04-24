;; https://bl.ocks.org/mbostock/e6f9e160585c153fa5ec543bd12b81e9

(ns ex54-random-walk
  (:require [clojure2d.core :as c2d]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const len 1300)

(def grad (c/gradient :rainbow))

(defn next-step
  "Next brownian motion step."
  [v] (v/add v (v/mult (v/vec2 (r/grand 0.3 1.0) (r/grand)) 2.0)))

(def main-path (take len (iterate next-step [20 300])))

(def c (c2d/canvas 1000 600 :highest))

(c2d/with-canvas [c c]
  (-> c
      (c2d/set-background :white)
      (c2d/set-color :black)
      (c2d/set-stroke 1.5)
      (c2d/path main-path)
      (c2d/set-stroke 1.0)
      (c2d/set-composite (p/composite :multiply)))
  (doseq [[^long idx pos] (take-nth 2 (map-indexed vector main-path))
          :let [parts (partition-all 10 9 (take 200 (iterate next-step pos)))]]
    (doseq [[^long idx2 p] (map-indexed vector parts)]
      (c2d/set-color c (grad (/ idx (double len))) (* 255.0 (/ (- 20 idx2 1) 20.0)))
      (c2d/path c p)))  )

(utils/show-image c)

(comment (c2d/save c "results/ex54/result.jpg"))
