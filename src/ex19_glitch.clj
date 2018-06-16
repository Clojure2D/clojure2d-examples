;; Several glitch examples

(ns ex19-glitch
  (:require [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.fields :as v]
            [clojure2d.extra.glitch :as g])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def cnvs (canvas (width img) (height img)))
(def window (show-window cnvs "Glitch"))

(defmethod key-pressed ["Glitch" \space] [_ _]
  (save cnvs (next-filename "results/ex19/" ".jpg")))

;; slitscan

(let [s (g/slitscan-random-config)]
  (println s)
  (p/set-canvas-pixels! cnvs (p/filter-channels (g/slitscan s) img)))

;; channel shift

(p/set-canvas-pixels! cnvs (p/filter-channels (g/shift-channels {:x-shift 0.1
                                                                 :y-shift 0.1})
                                              nil
                                              (g/shift-channels {:x-shift -0.1
                                                                 :y-shift -0.1})
                                              nil img))

(p/set-canvas-pixels! cnvs (->> img
                                (p/filter-colors c/to-HWB*)
                                (p/filter-channels (g/shift-channels {:x-shift 0.1
                                                                      :y-shift 0.0})
                                                   nil
                                                   (g/shift-channels {:x-shift -0.1
                                                                      :y-shift 0.0})
                                                   nil)
                                (p/filter-colors c/from-HWB*)))

;; random shift
(p/set-canvas-pixels! cnvs (p/filter-channels (g/shift-channels)
                                              (g/shift-channels)
                                              (g/shift-channels)
                                              nil img))

;; mirror image

(defn random-mirror
  ""
  []
  (partial p/filter-channels 
           (g/mirror (g/mirror-random-config))
           (g/mirror (g/mirror-random-config))
           (g/mirror (g/mirror-random-config))
           nil))

(p/set-canvas-pixels! cnvs (->> img
                                ((random-mirror))
                                ((random-mirror))))


;; slitscan 2

(binding [v/*skip-random-fields* true]
  (let [field-config (v/random-configuration)]
    (binding [p/*pixels-edge* :wrap]
      (println field-config)
      (p/set-canvas-pixels! cnvs (p/filter-channels (g/slitscan2 {:fields field-config :r 2.03})
                                                    (g/slitscan2 {:fields field-config :r 2.0})
                                                    (g/slitscan2 {:fields field-config :r 1.97}) nil img)))))


;; fold

(binding [v/*skip-random-fields* true]
  (let [field-config (v/random-configuration)]
    (binding [p/*pixels-edge* :wrap]
      (println field-config)
      (p/set-canvas-pixels! cnvs (p/filter-channels (g/fold {:fields field-config :r 2.03})
                                                    (g/fold {:fields field-config :r 2.0})
                                                    (g/fold {:fields field-config :r 1.97}) nil img)))))

;; pix2line

(p/set-canvas-pixels! cnvs (p/filter-channels (g/pix2line (g/pix2line-random-config))
                                              (g/pix2line (g/pix2line-random-config))
                                              (g/pix2line (g/pix2line-random-config)) nil img))


;; blend machine

(p/set-canvas-pixels! cnvs (let [blend-conf (g/blend-machine-random-config)
                                 p2l-conf (assoc (g/pix2line-random-config) :whole true)
                                 p2 (p/filter-channels (g/pix2line p2l-conf) img)]
                             (g/blend-machine blend-conf img p2)))
