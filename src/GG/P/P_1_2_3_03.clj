(ns GG.P.P-1-2-3-03
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn make-parts
  "Create fragments"
  [^long part-count]
  (loop [ii (int 0)
         mx part-count
         coll []] 
    (if (< ii mx)
      (if (r/brand 0.075)
        (let [fragments (r/irand 2 20)]
          (recur (inc ii) (+ mx fragments) (vec (concat coll (repeatedly fragments #(r/drand 2))))))
        (recur (inc ii) mx (conj coll (r/drand 2 20))))
      coll)))

(defn draw
  "Draw stripes"
  [canvas]
  (let [iter (make-counter)
        colors (mapv #(c/from-HSB* (if (even? %)
                                     (v/vec4 (r/drand 256) 255 (r/drand 255) 255)
                                     (v/vec4 138 (r/drand 255) 255 255))) (range 20))
        row-count (r/irand 5 30) 
        row-height (/ (height canvas) (double row-count))
        rows (map #(vector % (make-parts (inc ^long %))) (range (dec row-count) -1 -1))]
    (set-background canvas :black)
    (doseq [[^long i parts] rows]
      (let [sum-parts-total (reduce clojure.core/+ parts)
            cumulative (reductions clojure.core/+ 0 parts)]
        (mapv (fn [cum p]
                (let [x (m/norm cum 0 sum-parts-total 0 (width canvas))
                      y (* i row-height)
                      w (m/norm p 0 sum-parts-total 0 (width canvas))
                      h (* 1.5 row-height)]
                  (gradient-mode canvas x y :black x (+ y h) (c/set-alpha (colors (mod ^long (iter) 20)) 69))
                  (rect canvas x y w h))) cumulative parts)))))

(def cnvs (canvas 800 800))
(def window (show-window cnvs "P_1_2_3_03"))

(defmethod mouse-event [(:window-name window) :mouse-released] [_ _] 
  (with-canvas-> cnvs
    draw))

(with-canvas-> cnvs
  draw)
