(ns examples.NOC.ch08.mandelbrot-8-2
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double xmin -2.5)
(def ^:const ^double ymin -1.0)
(def ^:const ^double w 4.0)
(def ^:const ^double h 2.0)
(def ^:const ^int maxiterations 200)

(def ^:const ^int wdth 863)
(def ^:const ^int hght (/ wdth 2))

(def cnvs (canvas wdth hght))
(def window (show-window cnvs "Mandelbrot 8_2"))

(defn calc-mandelbrot
  "Render mandelbrot in pixels"
  []
  (let [pixels (p/pixels wdth hght)
        xmax (+ xmin w)
        ymax (+ ymin h)]
    (dotimes [j hght]
      (dotimes [i wdth]
        (let [x (m/norm i 0 wdth xmin xmax)
              y (m/norm j 0 hght ymin ymax)
              ^int nn (loop [n (int 0)
                             a x
                             b y]
                        (let [aa (m/sq a)
                              bb (m/sq b)
                              twoab (* 2.0 a b)
                              na (+ (- aa bb) x)
                              nb (+ twoab y)
                              aabb (+ aa bb)]
                          (if (and (< aabb 16.0)
                                   (< n maxiterations))
                            (recur (inc n) na nb)
                            n)))
              nc (mod (* nn 16) 255)]
          (p/set-color! pixels i j (if (== nn maxiterations)
                                     :black
                                     (c/color nc nc nc))))))
    pixels))

(p/set-canvas-pixels! cnvs (calc-mandelbrot))
