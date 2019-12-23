(ns examples.ex55-composing
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.random :as r]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const h 1000)
(def ^:const w 800)
(def ^:const l 200)

(def pal (conj (c/palette (c/gradient [(c/gray 0) (c/gray 220)]) 6) :red :maroon))

(def positions (repeatedly 6 (fn []
                               (repeatedly (count pal)
                                           (fn []
                                             (let [spr (r/drand 0.001 0.1)]
                                               [(repeatedly 70 (fn [] (r/drand (- spr) spr)))
                                                (r/drand -10 10)]))))))

(defn thread
  [canvas [spr1 ^double a] [spr2 ^double b]]
  (doseq [[^double r1 ^double r2] (map vector spr1 spr2)]
    (let [s (+ a r1)
          e (+ b r2)
          x1 (* w (r/noise s))
          x2 (* w (r/noise e))]
      (bezier canvas x1 0 x1 (/ l 2) x2 (/ l 2) x2 (- l 2)))))

(defn segment
  [canvas as bs]
  (doseq [[col a b] (map vector pal as bs)]
    (set-color canvas col 100)
    (thread canvas a b)))

(defn draw-me
  [canvas method]
  (set-composite canvas (p/composite method))
  (set-stroke canvas 2.0 :square)
  (doseq [[as bs] (partition 2 1 positions)]
    (segment canvas as bs)
    (translate canvas 0 l))
  canvas)

(def img (with-canvas-> (canvas w h)
           (set-background :white)
           (image (with-canvas-> (canvas w h :highest) (draw-me :difference))))) ;; try `:screen`

(utils/show-image img)

(comment save img "results/ex55/difference.jpg")
