(ns ex45-scalar-fields
  "Draw scalar fields derived from vector fields.

  * Press 1-8 - to draw scalar field
  * Press 0 - to draw folded plane by vector field"
  (:require [clojure2d.core :as c2d]
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

(def cnvs (c2d/canvas 820 840 :low))
(def window (c2d/show-window {:canvas cnvs
                            :window-name "ex45"
                            :state (random-config)}))

(defn draw-me
  "Draw scala field"
  [{:keys [field typ]}]
  (let [nfield (typ field)]
    (c2d/with-canvas [c (c2d/canvas 820 820 :low)]
      (c2d/set-background c :black)
      (c2d/translate c 10 10)
      (dotimes [x 800]
        (let [xx (m/norm x 0 800 (- m/PI) m/PI)]
          (dotimes [y 800]
            (let [yy (m/norm y 0 800 (- m/PI) m/PI)
                  col (-> (v/vec2 xx yy)
                          (nfield)
                          (m/sin)
                          (m/abs)
                          (* 255.0))]
              (c2d/set-color c col col col)
              (c2d/rect c x y 1 1)))))
      (c2d/get-image c))))

(defn fold-me
  "Draw vector field as fold"
  [{:keys [field]}]
  (c2d/with-canvas [c (c2d/canvas 820 820)]
    (c2d/set-background c :black)
    (c2d/set-color c :white 30)
    (c2d/translate c 10 10)
    (dotimes [_ 1000000]
      (let [x (+ (r/grand 0.001) (r/drand (- m/PI) m/PI))
            y (+ (r/grand 0.001) (r/drand (- m/PI) m/PI))
            res (field (v/vec2 x y))
            nx (m/norm (res 0) -4.5 4.5 0 800)
            ny (m/norm (res 1) -4.5 4.5 0 800)]
        (c2d/point c nx ny)))
    (c2d/get-image c)))

(defn cache-me
  "Cache image"
  [draw-fn nme typ state]
  (c2d/with-canvas-> cnvs
    (c2d/set-color :maroon)
    (c2d/rect 5 395 810 20)
    (c2d/set-color :white)
    (c2d/text "PLEASE WAIT..." 410 410 :center))
  (let [nstate (assoc state :typ typ)
        img (or (get-in state [:cache nme]) (draw-fn nstate))]
    (c2d/with-canvas-> cnvs
      (c2d/set-background :black)
      (c2d/image img 0 0 820 820)
      (c2d/set-color :white)
      (c2d/text nme 10 830))
    (assoc-in nstate [:cache nme] img)))

(defmethod c2d/mouse-event ["ex45" :mouse-pressed] [_ _] (cache-me fold-me "Fold" nil (random-config)))

(defmethod c2d/key-pressed ["ex45" \1] [_ s] (cache-me draw-me "Angle" f/heading s))
(defmethod c2d/key-pressed ["ex45" \2] [_ s] (cache-me draw-me "Divergence" f/divergence s))
(defmethod c2d/key-pressed ["ex45" \3] [_ s] (cache-me draw-me "Curl" f/curl s))
(defmethod c2d/key-pressed ["ex45" \4] [_ s] (cache-me draw-me "Det of Jacobian" f/jacobian s))
(defmethod c2d/key-pressed ["ex45" \5] [_ s] (cache-me draw-me "Magnitude" f/magnitude s))
(defmethod c2d/key-pressed ["ex45" \6] [_ s] (cache-me draw-me "Cross" f/cross s))
(defmethod c2d/key-pressed ["ex45" \7] [_ s] (cache-me draw-me "Dot" f/dot s))
(defmethod c2d/key-pressed ["ex45" \8] [_ s] (cache-me draw-me "Magnitude of difference" (fn [f] (fn [v] (v/mag (v/sub (f v) v)))) s))
(defmethod c2d/key-pressed ["ex45" \0] [_ s] (cache-me fold-me "Fold" nil s))

(defmethod c2d/key-pressed ["ex45" \space] [_ s] (c2d/save cnvs (c2d/next-filename "results/ex45/" ".jpg")) s)

;;

(c2d/set-state! window (cache-me fold-me "Fold" nil (c2d/get-state window)))
