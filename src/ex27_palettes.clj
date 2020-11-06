(ns ex27-palettes
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 800)
(def ^:const ^int h 400)

(def ^:const ^int halfw (int (/ w 2)))
(def ^:const ^int halfh (int (/ h 2)))

(def ^:const ^int ww (int (* 0.8 w)))
(def ^:const ^int hh (int (* 0.8 h)))
(def ^:const ^int bw (/ (- w ww) 2))
(def ^:const ^int bh (/ (- h hh) 2))

(def cnvs (c2d/canvas w h))
(def window (c2d/show-window cnvs "Palettes" 15 nil))

(defn draw-palette
  ""
  [canvas values ^long box-size]
  (doseq [[^long id col] values]
    (c2d/set-color canvas col)
    (c2d/rect canvas id bh box-size hh)))

(defn do-it
  ""
  []
  (let [palette (c/random-palette)
        box-size (int (/ ww (count palette)))
        values (map-indexed (fn [^long id v] [(+ bw (* id box-size)) v])
                            palette)]
    
    (c2d/with-canvas-> cnvs
      (c2d/set-color 20 20 20)
      (c2d/rect 0 0 w halfh)
      (c2d/set-color 235 235 235)
      (c2d/rect 0 halfh w halfh)
      (draw-palette values box-size))

    palette))

(defmethod c2d/key-pressed ["Palettes" \s] [_ _]
  (c2d/save cnvs (c2d/next-filename "results/ex27/" ".jpg")))

(defmethod c2d/key-pressed ["Palettes" \space] [_ _]
  (do-it))

(do-it)
