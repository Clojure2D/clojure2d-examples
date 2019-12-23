(ns rt-in-weekend.ch1-image
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.extra.utils :as u]
            [fastmath.vector :as v]))


(def img (p/pixels 800 400))

(dotimes [j (height img)]
  (dotimes [i (width img)]
    (let [r (/ (double i) (width img))
          g (/ (double (- (dec (height img)) j)) (height img))
          col (v/mult (v/vec4 r g 0.2 1.0) 255.0)]
      (p/set-color! img i j col))))

(u/show-image img)

;; (save img "results/rt-in-weekend/colors.jpg")
