(ns examples.NOC.ch08.treestatic-8-6
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]))

(def cnvs (canvas 800 200 :mid))
(def window (show-window cnvs "Tree Static 8_6"))

(defn branch
  "Generate branches."
  [canvas len]
  (let [nlen (* 0.66 len)]

    (-> canvas
        (line 0 0 0 (- len))
        (translate 0 (- len)))

    (if (> nlen 2.0)
      (-> canvas
          (push-matrix)
          (rotate (/ m/PI 5.0))
          (branch nlen)
          (pop-matrix)
          (push-matrix)
          (rotate (/ m/PI -5.0))
          (branch nlen)
          (pop-matrix))
      canvas)))

(with-canvas-> cnvs
  (set-background :white)
  (set-color :black)
  (set-stroke 2.0)
  (translate (/ (width cnvs) 2) (height cnvs))
  (branch 60))
