(ns GG.M.M-4-1-01
  (:require [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const w 600)
(def ^:const h 600)
(def ^:const min-x 5)
(def ^:const min-y 5)
(def ^:const max-x (- w 5))
(def ^:const max-y (- h 5))

(defrecord Node [pos velocity damping])

(defn update-node
  ""
  [{:keys [pos velocity ^double damping]}]
  (let [npos (v/add pos velocity)
        ^double x (npos 0)
        ^double y (npos 1)
        vx (if (or (< x min-x)
                   (> x max-x))
             (- ^double (velocity 0))
             (velocity 0))
        vy (if (or (< y min-y)
                   (> y max-y))
             (- ^double (velocity 1))
             (velocity 1))]
    (Node. npos (v/mult (v/vec2 vx vy) (- 1.0 damping)) damping)))

(defn random-velocity
  ""
  [n]
  (assoc n :velocity (v/vec2 (r/drand -5 5) (r/drand -5 5))))

(defn generate-nodes
  ""
  [cnt]
  (for [i (range cnt)]
    (Node. (v/vec2 (r/drand w) (r/drand h))
           (v/vec2 (r/drand -3 3) (r/drand -3 3))
           0.01)))

(defn draw
  ""
  [canvas window _ nodes]
  (let [nodes (if (and (key-pressed? window)
                       (= (key-char window) \s))
                (map random-velocity nodes)
                nodes)]
    (-> canvas
        (set-color :white 10)
        (rect 0 0 w h)
        (set-color :black))
    
    (doseq [^Node node nodes
            :let [[x y] (.pos node)]]
      (ellipse canvas x y 10 10))

    (map update-node nodes)))

(def window (show-window {:canvas (canvas w h)
                          :draw-fn draw
                          :setup (fn [canvas _]
                                   (set-background canvas :white)
                                   (generate-nodes 20))}))
