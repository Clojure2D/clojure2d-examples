(ns ex55-composing
  (:require [clojure2d.core :as c2d]
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
      (c2d/bezier canvas x1 0 x1 (/ l 2) x2 (/ l 2) x2 (- l 2)))))

(defn segment
  [canvas as bs]
  (doseq [[col a b] (map vector pal as bs)]
    (c2d/set-color canvas col 100)
    (thread canvas a b)))

(defn draw-me
  [canvas method]
  (c2d/set-composite canvas (p/composite method))
  (c2d/set-stroke canvas 2.0 :square)
  (doseq [[as bs] (partition 2 1 positions)]
    (segment canvas as bs)
    (c2d/translate canvas 0 l))
  canvas)

(def img (c2d/with-canvas-> (c2d/canvas w h)
         (c2d/set-background :white)
         (c2d/image (c2d/with-canvas-> (c2d/canvas w h :highest) (draw-me :difference))))) ;; try `:screen`

(utils/show-image img)

(comment c2d/save img "results/ex55/difference.jpg")
