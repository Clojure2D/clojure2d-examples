(ns GG.M.M-2-5-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const wname "M_2_5_01")

(def ^:const ^int w 800)
(def ^:const ^int h 800)
(def ^:const line-weight 1.0)
(def ^:const line-color :black)
(def ^:const line-alpha 50.0)
(def ^:const connection-radius 100.0)
(def ^:const point-count 1000)
(def ^:const scale-x (- (/ w 2) 30))
(def ^:const scale-y (- (/ h 2) 30))

(def color-mode (c/color-converter :RGB 255.0 255.0 255.0 100.0))

(defn calculate-lissajous-points
  ""
  [{:keys [^double freq-x ^double freq-y ^double mod-freq-x ^double mod-freq-y ^double phi]}]
  (for [i (range (inc point-count))
        :let [angle (m/norm i 0 point-count 0 m/TWO_PI)
              x (* scale-x (m/cos (* angle mod-freq-x))
                   (m/sin (+ (* angle freq-x) (m/radians phi))))
              y (* scale-y (m/cos (* angle mod-freq-y))
                   (m/sin (* angle freq-y)))]]
    (v/vec2 x y)))

(def cnvs (canvas w h))
(def window (show-window {:canvas cnvs
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
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn draw-lissajous
  ""
  [canvas points]
  (-> canvas
      (set-background :white)
      (set-stroke line-weight :round)
      (translate (/ w 2) (/ h 2)))
  (doseq [[p1 p2] (all-pairs points)
          :let [d (v/dist p1 p2)
                a (m/pow (/ (inc (/ d connection-radius))) 6.0)]]
    (when (<= d connection-radius)
      (-> canvas
          (set-color (color-mode (c/color line-color (* a line-alpha))))
          (line (p1 0) (p1 1) (p2 0) (p2 1))))))

(defn draw
  ""
  [s]
  (with-canvas-> cnvs
    (draw-lissajous (calculate-lissajous-points s)))
  s)

(defmethod key-pressed [wname \1] [_ s] (draw (update s :freq-x #(max 1 (dec ^double %)))))
(defmethod key-pressed [wname \2] [_ s] (draw (update s :freq-x #(inc ^double %))))
(defmethod key-pressed [wname \3] [_ s] (draw (update s :freq-y #(max 1 (dec ^double %)))))
(defmethod key-pressed [wname \4] [_ s] (draw (update s :freq-y #(inc ^double %))))

(defmethod key-pressed [wname \7] [_ s] (draw (update s :mod-freq-x #(max 1 (dec ^double %)))))
(defmethod key-pressed [wname \8] [_ s] (draw (update s :mod-freq-x #(inc ^double %))))
(defmethod key-pressed [wname \9] [_ s] (draw (update s :mod-freq-y #(max 1 (dec ^double %)))))
(defmethod key-pressed [wname \0] [_ s] (draw (update s :mod-freq-y #(inc ^double %))))

(defmethod key-pressed [wname virtual-key] [e s]
  (case (key-code e)
    :left (draw (update s :phi #(+ ^double % 15.0)))
    :right (draw (update s :phi #(- ^double % 15.0)))
    s))

(draw (get-state window))
