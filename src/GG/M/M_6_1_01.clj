(ns GG.M.M-6-1-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec2]))

(def ^:const wname "M-6-1-01")

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 800)
(def ^:const ^int h 800)
(def ^:const ^int min-x 5)
(def ^:const ^int min-y 5)
(def ^:const ^int max-x (- w 5))
(def ^:const ^int max-y (- h 5))
(def ^:const ^double radius 200.0)
(def ^:const ^double rradius (/ radius))
(def ^:const ^double strength -1.0)
(def ^:const ^double ramp 1.0)
(def ^:const ^double damping 0.5)

(deftype Node [^int id pos velocity])

(defn update-node
  ""
  [^Node node]
  (let [^Vec2 velocity (.velocity node)
        ^Vec2 npos (v/add (.pos node) velocity)
        x (m/constrain (.x npos) 0 (dec w))
        y (m/constrain (.y npos) 0 (dec h))
        vx (if (or (< x min-x)
                   (> x max-x))
             (- (.x velocity))
             (.x velocity))
        vy (if (or (< y min-y)
                   (> y max-y))
             (- (.y velocity))
             (.y velocity))]
    (Node. (.id node) (v/vec2 x y) (v/mult (v/vec2 vx vy) (- 1.0 damping)))))

(defn attract
  ""
  [^Node node ^Node other-node]
  (if (== ^int (.id node) ^int (.id other-node))
    node
    (let [dxy (v/sub (.pos other-node) (.pos node))
          d (v/mag dxy)]
      (if (and (pos? d) (< d radius))
        (let [s (m/pow (* d rradius) (/ ramp))
              f (/ (* 9.0 s strength (+ (/ (inc s)) (/ (- s 3.0) 4.0))) d)]
          (Node. (.id node) (.pos node) (v/add (.velocity node) (v/mult dxy f))))
        node))))

(defn init-nodes
  ""
  []
  (for [i (range 200)]
    (Node. i (v/vec2 (+ (/ w 2) (r/drand -1.0 1.0)) (+ (/ h 2) (r/drand -1.0 1.0))) (v/vec2 0.0 0.0))))

(defn draw
  ""
  [canvas window _ nodes]  
  (-> canvas
      (set-color :white 15)
      (rect 0 0 w h)
      (set-color :black))

  (let [nodes (if (and (key-pressed? window)
                       (= (key-char window) \r))
                (init-nodes)
                nodes)]
    
    (doseq [^Node node nodes]
      (let [pos (.pos node)]
        (ellipse canvas (pos 0) (pos 1) 10 10)))
    
    (map update-node (for [node nodes]
                       (reduce attract node nodes)))))

(def window (show-window {:canvas (canvas w h)
                          :draw-fn draw
                          :window-name wname
                          :setup (fn [canvas _]
                                   (set-background canvas :white)
                                   (init-nodes))}))
