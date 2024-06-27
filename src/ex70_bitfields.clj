;; https://www.reddit.com/r/generative/comments/1c2whnp/more_bitfield_experiments_made_with_r_code_linked/
;; https://gist.github.com/georgemsavva/6f37d263833385f618e71c40a292c707

(ns ex70-bitfields
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [clojure2d.color.blend :as bl]
            [clojure2d.extra.utils :as utils]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.extra.overlays :as o]))

(m/use-primitive-operators)
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn bor ^long [^long x ^long y] (m/bit-or x y))
(defn band ^long [^long x ^long y] (m/bit-and x y))
(defn bxor ^long [^long x ^long y] (m/bit-xor x y))
(defn bnor ^long [^long x ^long y]  (m/bit-not (m/bit-or x y)))
(defn bnand ^long [^long x ^long y] (m/bit-not (m/bit-and x y)))
(defn bnxor ^long [^long x ^long y] (m/bit-not (m/bit-xor x y)))

(def optional-ops
  (for [n [:add :madd :subtract :msubtract :linearburn :mlinearburn :darken :lighten
           :hardmix :or :xor :and :average :negation :normal]
        :let [f (bl/blends n)]]
    (fn [^long x ^long y] (c/pack (bl/blend-colors f x y)))))

#_(defn zzz [^long x ^long y] (c/pack (bl/blend-colors bl/pinlight x y)))

(def noise-ovl (o/noise-overlay 800 800 {:alpha 60}))

(defn bitfield []
  (c2d/with-canvas [c (c2d/canvas 800 800)]
    (c2d/set-background c :cornsilk)
    (c2d/push-matrix c)
    (c2d/translate c 16 16)
    (dotimes [layer (r/irand 3 15)]
      (let [t (r/drand 1 (* (inc layer) 25)) ;; modulus base
            N (m/fpow 2 (r/irand 5)) ;; size of the pixel
            f (r/randval 0.8 (rand-nth [bor band bxor bnor bnand bnxor])
                         (rand-nth optional-ops)) ;; which function to use
            t' (int (/ t (r/drand 2 5))) ;; which part of the values we should draw
            pal (c/palette (c/random-palette) t') ;; palette
            div (r/drand 1 20) ;; modulus offset denominator
            offx (r/irand 256) ;; x offset
            offy (r/irand 256)] ;; y offset
        (doseq [^long x (range 768)
                ^long y (range 768)
                :let [nx (/ x N)
                      ny (/ y N)
                      z (mod ^long (f (+ nx ny offx)
                                      (+ (- ny nx) offy))
                             (int (+ t (/ ny div))))]
                :when (< z t')]
          (c2d/set-color c (pal z) 220)
          (c2d/rect c x y 1 1))))
    (c2d/pop-matrix c)
    (c2d/image c noise-ovl)
    (utils/show-image c)))

(bitfield)
