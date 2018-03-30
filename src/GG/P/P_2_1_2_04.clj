(ns GG.P.P-2-1-2-04
  (:require [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int tile-count 20)
(def ^:const ^double rect-size 30.0)

(defn draw 
  "Draw rects"
  [canvas window _ _]
  (when (mouse-in-window? window)
    (let [rng (r/rng :default (get-state window))
          mx (/ (max 2 ^int (mouse-x window)) 20)
          my (/ (max 2 ^int (mouse-y window)) 20)]
      (-> canvas
          (set-background :white)
          (set-color (c/from-HSB (c/color 136 255 164 154))))
      (doseq [^long grid-x (range tile-count)
              ^long grid-y (range tile-count)]
        (let [px (* grid-x (/ ^int (width window) tile-count))
              py (* grid-y (/ ^int (height window) tile-count))
              p [(v/vec2 (+ px ^double (r/drandom rng (- mx) mx))
                         (+ py ^double (r/drandom rng (- my) my)))
                 (v/vec2 (+ px rect-size ^double (r/drandom rng (- mx) mx))
                         (+ py ^double (r/drandom rng (- my) my)))
                 (v/vec2 (+ px rect-size ^double (r/drandom rng (- mx) mx))
                         (+ py rect-size ^double (r/drandom rng (- my) my)))
                 (v/vec2 (+ px ^double (r/drandom rng (- mx) mx))
                         (+ py rect-size ^double (r/drandom rng (- my) my)))]]
          (path canvas p true false))))))

(def window (show-window {:canvas (canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_2_04"
                          :state 0}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _] (r/irand))
