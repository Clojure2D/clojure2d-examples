(ns GG.M.M-2-5-01
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const wname "M_2_5_01")

(def ^:const w 800)
(def ^:const h 800)
(def ^:const line-weight 1.0)
(def ^:const line-color :black)
(def ^:const line-alpha 50.0)
(def ^:const connection-radius 100.0)
(def ^:const point-count 1000)
(def ^:const scale-factor (/ m/TWO_PI point-count))
(def ^:const scale-x (- (/ w 2) 30))
(def ^:const scale-y (- (/ h 2) 30))

(def color-mode (c/color-converter :RGB 255.0 255.0 255.0 100.0))

(defn calculate-lissajous-points
  [{:keys [^double freq-x ^double freq-y ^double mod-freq-x ^double mod-freq-y ^double phi]}]
  (for [^long i (range (inc point-count))
        :let [angle (* i scale-factor)
              x (* scale-x (m/cos (* angle mod-freq-x))
                   (m/sin (+ (* angle freq-x) (m/radians phi))))
              y (* scale-y (m/cos (* angle mod-freq-y))
                   (m/sin (* angle freq-y)))]]
    (v/vec2 x y)))

(def cnvs (c2d/canvas w h))
(def window (c2d/show-window {:canvas cnvs
                            :window-name wname                          
                            :state {:phi 15.0
                                    :freq-x 4.0
                                    :freq-y 7.0
                                    :mod-freq-x 3.0
                                    :mod-freq-y 2.0}}))

(defn all-pairs
  "Stack overflow version"
  [coll]
  (when-let [s (next coll)]
    (let [start (first coll)]
      (lazy-cat (for [y s
                      :let [d (v/dist start y)]
                      :when (<= d connection-radius)]
                  [start y d])
                (all-pairs s)))))

(defn draw-lissajous
  [canvas points]
  (-> canvas
      (c2d/set-background :white)
      (c2d/set-stroke line-weight :round)
      (c2d/translate (/ w 2) (/ h 2)))
  (doseq [[p1 p2 ^double d] (all-pairs points)]
    (let [a (m/pow (/ (inc (/ d connection-radius))) 6.0)]
      (-> canvas
          (c2d/set-color (color-mode (c/color line-color (* a line-alpha))))
          (c2d/line p1 p2)))))

(defn draw
  [s]
  (c2d/with-canvas-> cnvs (draw-lissajous (calculate-lissajous-points s)))
  s)

(defmethod c2d/key-pressed [wname \1] [_ s] (draw (update s :freq-x #(max 1 (dec ^double %)))))
(defmethod c2d/key-pressed [wname \2] [_ s] (draw (update s :freq-x #(inc ^double %))))
(defmethod c2d/key-pressed [wname \3] [_ s] (draw (update s :freq-y #(max 1 (dec ^double %)))))
(defmethod c2d/key-pressed [wname \4] [_ s] (draw (update s :freq-y #(inc ^double %))))

(defmethod c2d/key-pressed [wname \7] [_ s] (draw (update s :mod-freq-x #(max 1 (dec ^double %)))))
(defmethod c2d/key-pressed [wname \8] [_ s] (draw (update s :mod-freq-x #(inc ^double %))))
(defmethod c2d/key-pressed [wname \9] [_ s] (draw (update s :mod-freq-y #(max 1 (dec ^double %)))))
(defmethod c2d/key-pressed [wname \0] [_ s] (draw (update s :mod-freq-y #(inc ^double %))))

(defmethod c2d/key-pressed [wname c2d/virtual-key] [e s]
  (case (c2d/key-code e)
    :left (draw (update s :phi #(+ ^double % 15.0)))
    :right (draw (update s :phi #(- ^double % 15.0)))
    s))

(draw (c2d/get-state window))
