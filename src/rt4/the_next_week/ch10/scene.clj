(ns rt4.the-next-week.ch10.scene
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]
            [rt4.the-next-week.ch10.hittable :as hittable]
            [rt4.the-next-week.ch10.interval :as interval]
            [rt4.the-next-week.ch10.material :as material]
            [rt4.common :as common]
            [rt4.color :as color]
            [rt4.the-next-week.ch10.camera :as camera]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c])
  (:import [rt4.the_next_week.ch10.hittable HitData]
           [rt4.the_next_week.ch10.material MaterialData]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def zero (v/vec3 0.0 0.0 0.0))

(defn ray-color [r world background ^long depth]
  (if (zero? depth)
    zero
    (if-let [^HitData rec (hittable/hit world r (interval/interval 0.001 ##Inf))]
      (let [color-from-emission (material/emitted (.mat rec) (.u rec) (.v rec) (.p rec))]
        (if-let [^MaterialData scatter (material/scatter (.mat rec) r rec)]
          (v/add color-from-emission
                 (v/emult (.attenuation scatter) (ray-color (.scattered scatter) world background (dec depth))))
          color-from-emission))
      background)))

(defprotocol SceneProto
  (render [scene]))

(defrecord Scene [cam world config]
  SceneProto
  (render [_]
    (let [image (if (:renderer? config)
                  (common/make-renderer-and-show (:image-width config) (:image-height config))
                  (common/make-pixels-and-show (:image-width config) (:image-height config)))
          ^long image-width (:image-width config)
          ^long image-height (:image-height config)
          image-width- (dec image-width)
          image-height- (dec image-height)
          max-depth (:max-depth config)
          background (:background config)]
      (common/pdotimes [j image-height (not (:shuffle? config))]
        (when (common/active? image)
          (dotimes [i image-width]
            (if (:renderer? config)
              (dotimes [_ (:samples-per-pixel config)]
                (let [u (/ (+ i (r/drand)) image-width-)
                      v (/ (+ j (r/drand)) image-height-)]
                  (-> (camera/get-ray cam u v)
                      (ray-color world background max-depth)
                      (c/scale-up)
                      (->> (p/set-color! (:pixels image) (+ i u) (- image-height- (+ j v)))))))
              (-> (reduce v/add zero
                          (repeatedly (:samples-per-pixel config)
                                      #(let [u (/ (+ i (r/drand)) image-width-)
                                             v (/ (+ j (r/drand)) image-height-)]
                                         (ray-color (camera/get-ray cam u v) world background max-depth))))
                  (color/->color (:samples-per-pixel config))
                  (->> (p/set-color! (:pixels image) i (- image-height- j))))))))
      image)))

(def default-config {:samples-per-pixel 10
                   :image-width 100
                   :max-depth 50
                   :background (v/vec3 0.0 0.0 0.0)
                   :aspect-ratio (/ 16.0 9.0)})

(defn scene
  ([cam world] (scene cam world {}))
  ([cam world config]
   (let [{:keys [^long image-width ^double aspect-ratio] :as config} (merge default-config config)]
     (->Scene cam world (assoc config :image-height (long (/ image-width aspect-ratio)))))))

