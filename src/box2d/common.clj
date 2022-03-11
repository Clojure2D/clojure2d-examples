;; https://github.com/lambdaisland/cljbox2d/blob/main/src/lambdaisland/cljbox2d/quil.cljc
(ns box2d.common
  (:require [lambdaisland.cljbox2d :as b]
            [lambdaisland.cljbox2d.math :as math]
            [clojure2d.core :as c2d])
  (:import [org.jbox2d.dynamics World Body Fixture]
           [org.jbox2d.collision.shapes PolygonShape CircleShape EdgeShape]
           [org.jbox2d.common Mat22]))

(defprotocol IDraw
  (draw*! [fixture canvas])
  (draw-shape! [shape body canvas]))

(defn draw! [canvas entity]
  (if-let [draw (:draw (b/user-data entity))]
    (draw entity canvas)
    (draw*! entity canvas)))

(extend-protocol IDraw
  World
  (draw*! [w canvas] (run! (partial draw! canvas) (b/bodies w)))
  Body
  (draw*! [b canvas] (run! (partial draw! canvas) (b/fixtures b)))
  Fixture
  (draw*! [f canvas]
    (if-let [draw (:draw (b/user-data f))]
      (draw f canvas)
      (draw-shape! (b/shape f) (b/body f) canvas)))

  PolygonShape
  (draw-shape! [shape body canvas]
    (c2d/path canvas
              (for [[x y] (map b/world->screen (b/world-vertices body shape))]
                [x y]) ;; Vec2 is not seqable...
              true))

  CircleShape
  (draw-shape! [shape body canvas]
    (let [^Mat22 matrix (b/transform b/*camera*)
          scale-x (.-x (.-ex matrix))
          scale-y (.-y (.-ey matrix))
          [x y] (b/world->screen (b/world-point body (b/centroid shape)))
          radius (b/radius shape)]
      (-> canvas
          (c2d/push-matrix)
          (c2d/rotate (math/mat-angle matrix))
          (c2d/ellipse x y (* scale-x radius 2.0) (* scale-y radius 2.0) true)
          (c2d/pop-matrix))))
  
  EdgeShape
  (draw-shape! [shape body canvas]
    (let [[[x1 y1] [x2 y2]] (map b/world->screen (b/world-vertices body shape))]
      (c2d/line canvas x1 y1 x2 y2))))
