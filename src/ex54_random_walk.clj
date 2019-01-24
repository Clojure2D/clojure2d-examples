;; https://bl.ocks.org/mbostock/e6f9e160585c153fa5ec543bd12b81e9

(ns examples.ex54-random-walk
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]))

(def ^:const len 1300)

(def grad (c/gradient-presets :rainbow))

(defn next-step
  "Next brownian motion step."
  [v] (v/add v (v/mult (v/vec2 (r/grand 0.3 1.0) (r/grand)) 2.0)))

(def main-path (take len (iterate next-step [20 300])))

(def c (canvas 1000 600 :highest))

(with-canvas [c c]
  (-> c
      (set-color :black)
      (set-stroke 1.5)
      (path main-path))
  (set-stroke c 1.0)
  (set-composite c (p/composite :multiply))
  (doseq [[idx pos] (take-nth 2 (map-indexed vector main-path))
          :let [parts (partition-all 10 9 (take 200 (iterate next-step pos)))]]
    (doseq [[idx2 p] (map-indexed vector parts)]
      (set-color c (grad (/ idx len)) (* 255.0 (/ (- 20 idx2 1) 20)))
      (path c p)))  )

(utils/show-image c)

(comment (save c "results/ex54/result.jpg"))
