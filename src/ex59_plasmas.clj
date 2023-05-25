(ns ex59-plasmas
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [fastmath.fields :as f]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.extra.utils :as u]
            [clojure2d.extra.overlays :as o]
            [clojure.pprint :as pp]
            [clojure2d.pixels :as p]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; store configs
(defonce configs (atom {}))

(def ^:const SIZE 800)

(defn headings ^double [p1 p2] (m/+ (v/heading p1) (v/heading p2)))
(defn heading+ ^double [p1 p2] (v/heading (v/add p1 p2)))
(defn heading- ^double [p1 p2] (v/heading (v/sub p1 p2)))
(defn angle ^double [p1 p2] (* 2.0 (v/angle-between p1 p2)))
(defn magnitudes ^double [p1 p2] (m/+ (v/mag p1) (v/mag p2)))
(defn magnitude+ ^double [p1 p2] (v/mag (v/add p1 p2)))
(defn magnitude- ^double [p1 p2] (v/mag (v/sub p1 p2)))
(defn dist ^double [p1 p2] (v/dist p1 p2))
(defn dist-abs ^double [p1 p2] (v/dist-abs p1 p2))
(defn dist-ang ^double [p1 p2] (v/dist-ang p1 p2))
(defn dist-emd ^double [p1 p2] (v/dist-emd p1 p2))
(defn dist-cheb ^double [p1 p2] (v/dist-cheb p1 p2))
(defn dist-canberra ^double [p1 p2] (v/dist-canberra p1 p2))
(defn dist-delta ^double [p1 p2] (if (< (v/dist p1 p2) 0.1) 1.0 0.1))
(defn dot ^double [p1 p2] (v/dot p1 p2))
(defn cross ^double [p1 p2] (v/cross p1 p2))
(defn sum ^double [p1 p2] (v/sum (v/add p1 p2)))
(defn lprod ^double [p1 p2] (let [r (v/emult p1 p2)]
                           (m/+ (m/log (inc (m/abs (r 0))))
                                (m/log (inc (m/abs (r 1)))))))

(def primitive-functions
  {:headings headings
   :magnitudes magnitudes
   :heading+ heading+
   :magnitude+ magnitude+
   :heading- heading-
   :magnitude- magnitude-
   :angle angle
   :dot dot
   :cross cross
   :dist dist
   :dist-abs dist-abs
   :dist-emd dist-emd
   :dist-cheb dist-cheb
   :dist-ang dist-ang
   :dist-canberra dist-canberra
   :dist-delta dist-delta
   :sum sum
   :lprod lprod})

(defn make-config []
  (binding [f/*skip-random-fields* (r/brand 0.8)]
    {:f1 (f/random-configuration)
     :f2 (f/random-configuration)
     :scale1 (r/drand 0.1 2.0)
     :scale2 (r/drand 0.1 2.0)
     :scale3 (r/drand 0.1 2.0)
     :n1 (r/random-noise-cfg)
     :n2 (r/random-noise-cfg)
     :n3 (r/random-noise-cfg)
     :n1-scale (r/drand m/PI)
     :n2-scale (r/drand m/PI)
     :n3-scale (r/drand m/PI)
     :in-scale (r/drand 0.1 3.0)
     :in-rot (r/drand m/TWO_PI)
     :in-shift [(r/drand -1.0 1.0) (r/drand -1.0 1.0)]
     :colorspace (rand-nth c/colorspaces-list) ;; random colorspace
     :fns (repeatedly 3 #(rand-nth (keys primitive-functions)))
     :pow1 (r/drand 0.3 7.0)
     :pow2 (r/drand 0.3 7.0)
     :pow3 (r/drand 0.3 7.0)
     :saturation (r/drand 0.5 1.2)}))

(defn plasma->buf
  [{:keys [f1 f2 ^double scale1 ^double scale2 ^double scale3
           n1 n2 n3 ^double in-scale ^double in-rot in-shift
           colorspace ^double n1-scale ^double n2-scale ^double n3-scale
           ^double pow1 ^double pow2 ^double pow3
           fns]}]
  (let [-in-scale (- in-scale)
        [fn1 fn2 fn3] (map primitive-functions fns)
        field1 (f/combine f1)
        field2 (f/combine f2)
        noise1 (r/random-noise-fn n1)
        noise2 (r/random-noise-fn n2)
        noise3 (r/random-noise-fn n3)
        in-shift (apply v/vec2 in-shift)
        [_ convert-from] (c/colorspaces* colorspace)
        buf (p/renderer SIZE SIZE :gaussian 1.45 1.0) ;; blur a little bit
        js (take 4 (drop (r/irand 100) (r/jittered-sequence-generator :r2 2))) ;; 4 points per pixel
        is (range (count js))
        xss (mapv first js)
        yss (mapv second js)]
    (doseq [^long x (range SIZE)
            ^long y (range SIZE)
            i is
            :let [^double xs (xss i) ;; shift x
                  ^double ys (yss i) ;; shift y
                  x1 (m/+ x xs)
                  y1 (m/+ y ys)
                  xx (m/norm x1 0 SIZE -in-scale in-scale) ;; scale screen to field coords
                  yy (m/norm y1 0 SIZE -in-scale in-scale)
                  in (-> (v/vec2 xx yy)    ;; input
                         (v/rotate in-rot) ;; rotate
                         (v/add in-shift)) ;; shift
                  p1 (field1 in)           ;; apply field 1
                  p2 (field2 in)           ;; apply field 2
                  ;; individual color channels
                  c1 (m/pow (m/abs (m/sin (m/* (m/+ ^double (fn1 p1 p2)
                                                    (m/* n1-scale
                                                         ^double (noise1 (p1 0) (p1 1))))
                                               scale1))) pow1)
                  c2 (m/pow (m/abs (m/sin (m/* (m/+ ^double (fn2 p1 p2)
                                                    (m/* n2-scale
                                                         ^double (noise2 (p2 0) (p2 1))))
                                               scale2))) pow2)
                  c3 (m/pow (m/abs (m/sin (m/* (m/+ ^double (fn3 p1 p2)
                                                    (m/* n3-scale
                                                         ^double (noise3 (p1 1) (p2 0) 0.3456)))
                                               scale3))) pow3)
                  col (-> (v/vec3 c1 c2 c3) (v/mult 255.0) convert-from)]]
      (p/set-color! buf x1 y1 col))
    buf))

(defonce noverlay (o/noise-overlay SIZE SIZE {:alpha 90}))

(defn draw-plasma [buf {:keys [saturation]}]
  (o/render-noise
   (c2d/get-image (p/to-pixels buf {:splats? true :saturation saturation}))
   noverlay))

;; store the file name
(def file-name (c2d/next-filename "results/ex59/"))

;; store config
(def config (let [config (make-config)]
            (pp/pprint file-name)
            (swap! configs assoc file-name config) ;; save config to an atom
            (pp/pprint (dissoc config :n1 :n2 :n3 :f1 :f2))
            config))

;; render image
(def buffer (->> (-> c2d/available-cores
                   (m// 2)
                   (m/max 1)
                   (repeatedly #(future (plasma->buf config)))) ;; parallel rendering, use half of cores
               (vec)       ;; run tasks (lazy seq)
               (map deref) ;; get results and combine
               (reduce p/merge-renderers)))

;; build the image (raw -> image, saturation adjustment, adding noise)
(def image (draw-plasma buffer config))

;; show the result
(u/show-image image)

(comment
  (do
    (spit (str file-name ".edn") config) ;; save configuration
    (c2d/save image (str file-name ".jpg"))) ;; save the result
  )
