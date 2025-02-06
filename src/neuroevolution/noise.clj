(ns neuroevolution.noise
  (:require [clojure2d.color :as c]
            [clojure2d.pixels :as p]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.core :as c2d]
            [fastmath.calculus :as calc]
            [fastmath.vector :as v]
            [clojure2d.extra.utils :as utils]))

(def ^:const SCALE 900.0)

(def n (comp m/sq (r/random-noise-fn)))

(defn ->background
  ([] (->background 800))
  ([^long size]
   (let [buff (p/pixels size size)
         g (c/gradient [:black 0x8889ab])]
     (doseq [^long x (range size)
             ^long y (range size)
             :let [xx (m// x SCALE)
                   yy (m// y SCALE)
                   n (n xx yy)]]
       (p/set-color! buff x y (g n)))
     (c2d/to-image buff))))

(defn ->grad-map
  ([] (->grad-map 800))
  ([^long size]
   (let [noise-grad (calc/gradient (fn [[x y]] (n x y)))]
     (vec (for [^long x (range size)
                :let [xx (m// x SCALE)]]
            (vec (for [y (range size)
                       :let [yy (m// y SCALE)]]
                   (v/limit (v/sub (v/mult (v/seq->vec2 (noise-grad [xx yy])) 0.15)) 2.0))))))))

(def background (->background))

(utils/show-image background)

(def grad-map (->grad-map))
