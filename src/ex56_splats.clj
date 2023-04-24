(ns ex56-splats
  (:require [clojure2d.core :as c2d]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.color.blend :as b]))

(m/use-primitive-operators)

(def c (c2d/canvas 800 800))
(def g (c/random-gradient))
(def t (r/drand 10.0))
(def splats-no 10000)
(def n (r/random-noise-fn))

(defn splat
  [canvas x y ^double size]
  (let [mid (v/generate-vec2 #(r/drand -3 3))
        r (r/grand)
        p (for [angle (range 0 m/TWO_PI (r/drand 0.001 0.1))
                :let [v (v/add mid (v/from-polar (v/vec2 r angle)))
                      off (* 5.0 (r/noise (v 0) (v 1)))]]
            (v/add (v/vec2 x y) (v/from-polar (v/vec2 (* size (+ r off)) angle))))]
    (c2d/path canvas p true (r/brand 0.3))))

(def img (let [blend-name (rand-nth b/blends-list)]
         (println blend-name)
         (c2d/with-canvas [c c]
           (c2d/set-composite c (p/composite blend-name))
           (dotimes [i splats-no]
             (let [s (m/norm i 0 splats-no 0 20)]
               (-> (c2d/set-color c (g (/ i (double splats-no))) (r/drand 2 10))
                   (splat (* 800 ^double (n t s 0.5))
                          (* 800 ^double (n s t))
                          (r/grand (* 20.0 (r/simplex t (/ s 10.0))))))))
           c)))

(def final (c2d/with-canvas-> (c2d/canvas (c2d/width c) (c2d/height c))
           (c2d/set-background :white)
           (c2d/image (p/filter-channels p/gaussian-blur-2 true (p/to-pixels img)))))

(utils/show-image final)

(comment (c2d/save final "results/ex56/splat.jpg"))

