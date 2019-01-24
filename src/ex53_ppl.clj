;; https://www.youtube.com/watch?v=makaJpLvbow

(ns examples.ex53-ppl
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long size 200)
(def ^:const ^long csize (* 3 size))
(def rand-size #(r/drand size))
(def wrapper (partial m/wrap 0 size))
(def pal (vec (c/resample 20 (reverse (c/palette-presets :spectral-11)))))
(def ^:const ^long lim (dec (count pal)))

(defrecord Spore [^long id ^Vec2 pos ^double phi ^double v ^double r ^double alpha ^double beta col])

(defn spore [id v r alpha beta col]
  (->Spore id (v/generate-vec2 rand-size) (r/drand m/TWO_PI) v r alpha beta col))

(defn prim+
  ([^long x] x)
  ([^long x ^long y] (+ x y)))

(defn change-move
  [^Spore s spores]
  (let [in-radius (filterv #(and (not= (.id s) (.id ^Spore %)) 
                                 (< ^double (v/dist (.pos s) (.pos ^Spore %)) (.r s))) spores)
        cnt (count in-radius)
        ^double delta (if (pos? cnt)
                        (* cnt (.beta s) (m/sgn (reduce prim+ 0 (mapv #(m/sgn (v/cross (.pos s) (.pos ^Spore %))) in-radius))))
                        0)
        nphi (rem (+ m/TWO_PI (.phi s) (.alpha s) delta) m/TWO_PI)]
    (->Spore (.id s)
             (v/fmap (v/add (.pos s) (v/from-polar (v/vec2 (.v s) nphi))) wrapper)
             nphi
             (.v s)
             (.r s)
             (.alpha s)
             (.beta s)
             (pal (m/constrain cnt 0 lim)))))

(def spores-init (take 1200 (map #(spore % 0.67 15.0 (m/radians 180) (m/radians 17) (first pal)) (range))))

(defn draw
  [canvas _ _ spores]
  (set-background canvas :black 100)
  (doseq [^Spore s spores
          :let [^Vec2 p (v/mult (.pos s) 3.0)]]
    (set-color canvas (.col s))
    (ellipse canvas (.x p) (.y p) 8 8))
  (mapv #(change-move % spores) spores))

(show-window {:canvas (canvas csize csize)
              :draw-fn draw
              :draw-state spores-init})
