(ns games.the-king.map
  (:require [clojure2d.core :as c2d]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [games.the-king.gfx :as gfx]
            [fastmath.vector :as v]))

(defn calculate-grid
  "Returns random 16x16 map,

  * keys - position on the map
  * value - a triplet: [gradient-x, gradient-y, elevation]
  
  Elevation (0-9): 0 - water, 9 - mountain rocks

  gradient noise gives easier levels and is selected more often at the beginning of the game"
  ([level] (calculate-grid level 0.25))
  ([level step]
   (let [noise-type (if (zero? (mod level 10))
                      :simplex
                      (r/randval (m/norm level 1 100 0.6 0.01) :gradient :value))
         noise (r/single-noise {:noise-type noise-type}) ;; create random value or gradient noise, one octave
         offset-x (r/grand 5) ;; random noise offset
         offset-y (r/grand 5)
         offset-z (r/grand 5)]
     (->> (for [x (range 16)
                y (range 16)
                :let [xx (* (+ x offset-x) step)
                      yy (* (+ y offset-y) step)
                      xx0 (- xx step)
                      yy0 (- yy step)
                      xx1 (+ xx step)
                      yy1 (+ yy step)
                      ;; ;; gradient, simplified
                      dx (- (m/sq (noise xx1 yy offset-z))
                            (m/sq (noise xx0 yy offset-z)))
                      dy (- (m/sq (noise xx yy1 offset-z))
                            (m/sq (noise xx yy0 offset-z)))
                      v (int (m/floor (* 10.0 (noise xx yy offset-z))))]]
            [[x y] [(- dx) (- dy) v]])
          (into {})))))

(defn- ensure-vector
  "Sometimes position contains doubles or is Vec2 type"
  [v] (v/fmap (vec v) int))

(defn elevation
  "Helper function which returns elevation from grid"
  ([grid position] (get-in grid [(ensure-vector position) 2]))
  ([grid x y] (elevation grid [x y])))

(defn gradient
  "Helper function which returns gradient"
  ([grid position] (v/seq->vec2 (get grid (ensure-vector position))))
  ([grid x y] (gradient grid [x y])))

(defn water-sprite
  "Find water sprite.

  Function selects proper sprite checking if water spans more area left or right."
  [grid x y]
  (let [sprite-id (condp = x
                    0 (if (zero? (elevation grid 1 y)) :right :small) ;; left map border
                    15 (if (zero? (elevation grid 14 y)) :left :small) ;; right map border
                    (let [left? (zero? (elevation grid (dec x) y)) ;; interior
                          right? (zero? (elevation grid (inc x) y))]
                      (cond
                        (and left? right?) :big
                        (and left? (not right?)) :left
                        (and (not left?) right?) :right
                        :else :small)))]
    (get-in gfx/sprites [:water sprite-id])))

(defn init-map
  "Draws a map, based on given grid"
  [grid]
  (c2d/with-canvas [c (c2d/canvas gfx/canvas-size gfx/canvas-size :low)]
    (doseq [x (range 16)
            y (range 16)
            :let [n (elevation grid x y)
                  sprite (condp = n
                           0 (water-sprite grid x y)
                           1 (gfx/sprites :mud)
                           (rand-nth (get-in gfx/sprites [:terrain (- n 2)])))]]
      (c2d/image c sprite (* x gfx/sprite-size) (* y gfx/sprite-size)))
    (c2d/to-image c)))

