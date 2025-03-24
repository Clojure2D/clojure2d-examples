(ns ex68-langton-ant
  (:require [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [clojure2d.extra.utils :as utils]
            [fastmath.core :as m]))

(defn modadd
  [[^long x1 ^long y1] [^long x2 ^long y2] ^long w ^long h]
  [(m/mod (m/+ x1 x2) w)
   (m/mod (m/+ y1 y2) h)])

(defn move [buff [[x y :as pos] [^long dx ^long dy] rules]]
  (let [w (c2d/width buff)
        h (c2d/height buff)
        c1 (c/set-alpha (p/get-color buff x y) 255.0)
        [c2 r] (rules c1)
        ndir (if (= \L r) [dy (m/- dx)] [(m/- dy) dx])]
    (p/set-color! buff x y c2)
    [(modadd pos ndir w h) ndir rules]))

(defn draw [canvas window _ {:keys [^long speed ^long steps ^long scale buff pos-dir] :as data}]
  (if (c2d/key-pressed? window)
    data
    (let [[pos dir] pos-dir
          hsc (m// scale 2)
          r (* hsc 5)
          pos2 (v/shift (v/mult pos scale) hsc)
          dir2 (v/add pos2 (v/mult dir scale))]
      (-> canvas
          (c2d/set-background 20 20 20)
          (c2d/image buff)
          (c2d/set-color :blue 200)
          (c2d/rect 10 10 150 35)
          (c2d/set-color :white)
          (c2d/set-font-attributes 20)
          (c2d/text (str steps) 20 35)
          (c2d/set-color :red)
          (c2d/set-stroke 2)
          (c2d/ellipse (pos2 0) (pos2 1) r r true)
          (c2d/line (pos2 0) (pos2 1) (dir2 0) (dir2 1)))
      (assoc data
             :steps (+ speed steps)
             :pos-dir (nth (iterate (partial move buff) pos-dir) speed)))))

(def pal (concat [(c/color :black)] (c/palette :category20)))

(defn rule->data [rule]
  (->> (take (count rule) pal)
       (cycle)
       (partition 2 1)
       (take (count rule))
       (map (fn [r [c1 c2]] [c1 [c2 r]]) rule)
       (into {})))

(def r1 "RRLLLRLLLRRR")
(def r2 "LLRRRLRLRLLR")
(def r3 "LRRRRRLLR")
(def r4 "LRRL")
(def r5 "RRLRR")
(def r6 "RRRL")
(def r7 "RRRRL")

(defn init [{:keys [^long scale rule] :as data}]
  (let [ps (m// 800 scale)]
    (assoc data
           :steps 0
           :buff (p/pixels ps ps)
           :pos-dir [[(m/inc (m// ps 2)) (m/inc (m// ps 2))] [0 1] (rule->data rule)])))

(def window (c2d/show-window {:canvas (c2d/canvas 800 800 :low)
                            :background :black
                            :draw-fn draw
                            :draw-state (init {:scale 2 :speed 10000 :rule "RLRRLRLL"})}))

(comment (utils/show-palette pal))
