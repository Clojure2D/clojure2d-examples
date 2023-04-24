(ns ex58-circle-inversion
  (:require [clojure2d.extra.utils :as utils]
            [fastmath.fields :as f]
            [fastmath.core :as m]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec2]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)
(m/use-primitive-operators)

(def ^:const c-cnt (long (r/randval (r/irand 3 10)
                                  (r/irand 3 6))))

(println {:c-cnt c-cnt})

;; circle inversion functions, two are centered in origin
(def circles (vec (conj (repeatedly (- c-cnt 2)
                                  #(r/randval 0.8
                                              (f/field :circle-inverse 1.0
                                                       {:x0 (r/randval 0.0 (r/randval (r/irand -2 2)
                                                                                      (r/drand -2.0 2.0)))
                                                        :y0 (r/randval 0.0 (r/randval (r/irand -2 2)
                                                                                      (r/drand -2.0 2.0)))
                                                        :r (r/drand 0.01 4.0)})
                                              (f/random-field)))
                      (f/field :circle-inverse 1.0 {:x0 0.0 :y0 0.0 :r (r/drand 0.5 2.5)})
                      (f/field :circle-inverse 1.0 {:x0 0.0 :y0 0.0 :r (r/drand 1.0 2.0)}))))

;; vector fields
(def fields (vec (repeatedly c-cnt f/random-field)))


;; palette
(def c (vec (repeatedly c-cnt #(c/clamp (r/randval
                                       (c/random-color :intense-light)
                                       (c/random-color :bright))))))

(def buffer (p/renderer 1000 1000))

(defn draw-path
  [^long cnt ^Vec2 in]
  (when (and (< -1.0e18 (v/sum in) 1.0e18)
             (pos? cnt))
    (let [id (r/irand c-cnt)
          inv (circles id) ;; random circle inversion function
          col (c id) ;;  random color from palette
          ^Vec2 nin (if (r/brand 0.1)
                      (let [vf (fields id)]
                        (inv (vf in))) ;; maybe include other vector field
                      (inv in))
          ^Vec2 nin (if (r/brand 0.2)
                      ;; maybe add some noise
                      (v/add nin (v/mult (let [^Vec2 diff (v/sub nin in)]
                                           (Vec2. (- (r/noise (.x diff) (.y diff) (.x in)) 0.5)
                                                  (- (r/noise (.y diff) (.x diff) (.y in)) 0.5))) 0.3))
                      nin)]
      (p/set-color! buffer
                    (m/norm (.x nin) -3.2 3.2 0.0 1000.0)
                    (m/norm (.y nin) -3.2 3.2 0.0 1000.0)
                    col)
      (recur (dec cnt) nin))))

(dotimes [t 5000] ;; change to 1e7
  (when (zero? (mod t 10000)) (println t))
  (let [p (r/randval
           (Vec2. (r/grand) (r/grand))
           (Vec2. (r/drand -2.0 2.0) (r/drand -2.0 2.0)))]
    (draw-path 2000 p))) ;; path length

(do
  (def img (p/to-pixels buffer {:brightness 1.5 :contrast 1.5 :saturation 1.5
                              :gamma-alpha 1.0 :gamma-color 1.0}))
  (utils/show-image img))

(comment
  (c2d/save img "results/ex58/img14.jpg"))
