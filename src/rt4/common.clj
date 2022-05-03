(ns rt4.common
  (:require [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn make-pixels-and-show
  "Create buffer, show window and display rendering"
  [width height]
  (let [p (p/pixels width height)
        draw-fn (fn [canvas _ _ _]
                  (c2d/image canvas p))]
    {:window (c2d/show-window {:canvas (c2d/canvas width height :low)
                               :draw-fn draw-fn
                               :fps 5})
     :pixels p}))

(defn active?
  "Check if window is active"
  [in]
  (and (map? in) (:window in) (c2d/window-active? (:window in))))

(defn save
  [in filename]
  (c2d/save (c2d/to-image (:pixels in)) filename))

(defmacro pdotimes
  "Parallel dotimes"
  {:style/indent 1}
  [[line-sym lines not-shuffle?] & forms]
  `(let [l# (if ~not-shuffle? (range ~lines) (shuffle (range ~lines)))]
     (->> l#
          (pmap (fn [y#]
                  (let [~line-sym (long y#)]
                    ~@forms)))
          (dorun))))

;;

(defn random-in-unit-disc [] (r/ball-random 2))
(defn random-in-unit-sphere [] (r/ball-random 3))
(defn random-unit-vector [] (v/normalize (random-in-unit-sphere)))
(defn random-on-hemisphere [normal]
  (let [on-unit-sphere (random-unit-vector)]
    (if (pos? (v/dot on-unit-sphere normal))
      on-unit-sphere (v/sub on-unit-sphere))))
(defn random-vec3
  ([] (v/generate-vec3 r/drand))
  ([^double mn ^double mx] (v/generate-vec3 #(r/drand mn mx))))


;;

(defn reflect
  [v n]
  (->> (v/dot v n)
       (* 2.0)
       (v/mult n)
       (v/sub v)))

(defn refract
  [uv n ^double etai-over-etat]
  (let [cos-theta (min (v/dot (v/sub uv) n) 1.0)
        r-out-perp (v/mult (v/add uv (v/mult n cos-theta))  etai-over-etat)
        r-out-parallel (v/mult n (- (m/sqrt (abs (- 1.0 (v/magsq r-out-perp))))))]
    (v/add r-out-perp r-out-parallel)))
