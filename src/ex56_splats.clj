(ns examples.ex56-splats
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.extra.utils :as utils]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.color.blend :as b]))

(m/use-primitive-operators)

(def c (canvas 800 800))
(def g (c/random-gradient))
(def t (r/drand 10.0))
(def splats-no 5000)
(def n (r/random-noise-fn))

(defn splat
  [canvas x y ^double size]
  (let [mid (v/generate-vec2 #(r/drand -3 3))
        r (r/grand)
        p (for [angle (range 0 m/TWO_PI (r/drand 0.001 0.1))
                :let [v (v/add mid (v/from-polar (v/vec2 r angle)))
                      off (* 5.0 (r/noise (v 0) (v 1)))]]
            (v/add (v/vec2 x y) (v/from-polar (v/vec2 (* size (+ r off)) angle))))]
    (path canvas p true false)))

(def img (with-canvas [c c]
           (set-composite c (p/composite (rand-nth b/blends-list)))
           (dotimes [i splats-no]
             (let [s (m/norm i 0 splats-no 0 20)]
               (-> (set-color c (g (/ i (double splats-no))) (r/drand 2 10))
                   (splat (* 800 ^double (n t s 0.5))
                          (* 800 ^double (n s t))
                          (r/grand (* 20.0 (r/simplex t (/ s 10.0))))))))
           c))

(def final (with-canvas-> (canvas (width c) (height c))
             (set-background :white)
             (image (p/filter-channels p/gaussian-blur-2 true (p/to-pixels img)))))

(utils/show-image final)

(comment (save final "results/ex56/splat.jpg"))

