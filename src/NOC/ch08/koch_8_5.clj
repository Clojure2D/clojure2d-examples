(ns examples.NOC.ch08.koch-8-5
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(def cnvs (canvas 800 250 :mid))

(def initial-state {:counter 0
                    :segments [[(v/vec2 0 (- (height cnvs) 20))
                                (v/vec2 (width cnvs) (- (height cnvs) 20))]]})

(defn render
  "Render segments"
  [canvas {segments :segments}]
  (run! #(let [[^Vec2 a ^Vec2 b] %]
           (line canvas (.x a) (.y a) (.x b) (.y b))) segments))


(defn iterate-segment
  "Return new segments for given pair"
  [[a e]]
  (let [diff (v/div (v/sub e a) 3.0)
        rot (v/rotate diff (m/radians -60))
        b (v/add a diff)
        c (v/add a (v/add rot diff))
        d (v/sub e diff)]
    [[a b] [b c] [c d] [d e]]))

(defn next-level
  "Calculate next depth"
  [{segments :segments counter :counter}]
  (if (<= counter 5)
    {:counter (inc counter)
     :segments (reduce #(concat %1 (iterate-segment %2)) [] segments)}
    initial-state))

(defn draw
  ""
  [canvas _ _ state]
  (let [state (or state initial-state)]
    (-> canvas
        (set-background :white)
        (set-color :black)
        (render state))
    (next-level state)))

(def window (show-window cnvs "Koch 8_5" 1 draw))
