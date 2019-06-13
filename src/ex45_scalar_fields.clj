(ns ex45-scalar-fields
  "Draw scalar fields derived from vector fields.

  * Press 1-8 - to draw scalar field
  * Press 0 - to draw folded plane by vector field"
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.fields :as f]
            [fastmath.vector :as v]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn random-config
  "Randomize field"
  []
  (binding [f/*skip-random-fields* true]
    (let [cfg (f/random-configuration)]
      (println cfg)
      {:field (f/combine cfg)
       :typ f/heading
       :cache {}})))

(def cnvs (canvas 820 840 :low))
(def window (show-window {:canvas cnvs
                          :window-name "ex45"
                          :state (random-config)}))

(defn draw-me
  "Draw scala field"
  [{:keys [field typ] :as cfg}]
  (let [nfield (typ field)]
    (with-canvas [c (canvas 820 820 :low)]
      (set-background c :black)
      (translate c 10 10)
      (dotimes [x 800]
        (let [xx (m/norm x 0 800 (- m/PI) m/PI)]
          (dotimes [y 800]
            (let [yy (m/norm y 0 800 (- m/PI) m/PI)
                  col (-> (v/vec2 xx yy)
                          (nfield)
                          (m/sin)
                          (m/abs)
                          (* 255.0))]
              (set-color c col col col)
              (rect c x y 1 1)))))
      (get-image c))))

(defn fold-me
  "Draw vector field as fold"
  [{:keys [field] :as cfg}]
  (with-canvas [c (canvas 820 820)]
    (set-background c :black)
    (set-color c :white 30)
    (translate c 10 10)
    (dotimes [i 1000000]
      (let [x (+ (r/grand 0.001) (r/drand (- m/PI) m/PI))
            y (+ (r/grand 0.001) (r/drand (- m/PI) m/PI))
            res (field (v/vec2 x y))
            nx (m/norm (res 0) -4.5 4.5 0 800)
            ny (m/norm (res 1) -4.5 4.5 0 800)]
        (point c nx ny)))
    (get-image c)))

(defn cache-me
  "Cache image"
  [draw-fn nme typ state]
  (with-canvas-> cnvs
    (set-color :maroon)
    (rect 5 395 810 20)
    (set-color :white)
    (text "PLEASE WAIT..." 410 410 :center))
  (let [nstate (assoc state :typ typ)
        img (or (get-in state [:cache nme]) (draw-fn nstate))]
    (with-canvas-> cnvs
      (set-background :black)
      (image img 0 0 820 820)
      (set-color :white)
      (text nme 10 830))
    (assoc-in nstate [:cache nme] img)))

(defmethod mouse-event ["ex45" :mouse-pressed] [_ _] (cache-me fold-me "Fold" nil (random-config)))

(defmethod key-pressed ["ex45" \1] [_ s] (cache-me draw-me "Angle" f/heading s))
(defmethod key-pressed ["ex45" \2] [_ s] (cache-me draw-me "Divergence" f/divergence s))
(defmethod key-pressed ["ex45" \3] [_ s] (cache-me draw-me "Curl" f/curl s))
(defmethod key-pressed ["ex45" \4] [_ s] (cache-me draw-me "Det of Jacobian" f/jacobian s))
(defmethod key-pressed ["ex45" \5] [_ s] (cache-me draw-me "Magnitude" f/magnitude s))
(defmethod key-pressed ["ex45" \6] [_ s] (cache-me draw-me "Cross" f/cross s))
(defmethod key-pressed ["ex45" \7] [_ s] (cache-me draw-me "Dot" f/dot s))
(defmethod key-pressed ["ex45" \8] [_ s] (cache-me draw-me "Magnitude of difference" (fn [f] (fn [v] (v/mag (v/sub (f v) v)))) s))
(defmethod key-pressed ["ex45" \0] [_ s] (cache-me fold-me "Fold" nil s))

(defmethod key-pressed ["ex45" \space] [_ s] (save cnvs (next-filename "results/ex45/" ".jpg")) s)

;;

(set-state! window (cache-me fold-me "Fold" nil (get-state window)))
