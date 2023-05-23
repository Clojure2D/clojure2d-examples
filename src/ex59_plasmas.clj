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

(def ^:const SIZE 800)

(defn headings ^double [p1 p2] (m/+ (v/heading p1) (v/heading p2)))
(defn heading ^double [p1 p2] (v/heading (v/add p1 p2)))
(defn angle ^double [p1 p2] (* 2.0 (v/angle-between p1 p2)))
(defn magnitudes ^double [p1 p2] (m/+ (v/mag p1) (v/mag p2)))
(defn magnitude ^double [p1 p2] (v/mag (v/add p1 p2)))
(defn dist ^double [p1 p2] (v/dist p1 p2))
(defn dist-abs ^double [p1 p2] (v/dist-abs p1 p2))
(defn dist-ang ^double [p1 p2] (v/dist-ang p1 p2))
(defn dist-emd ^double [p1 p2] (v/dist-emd p1 p2))
(defn dist-cheb ^double [p1 p2] (v/dist-cheb p1 p2))
(defn dist-canberra ^double [p1 p2] (v/dist-canberra p1 p2))
(defn dot ^double [p1 p2] (v/dot p1 p2))
(defn cross ^double [p1 p2] (v/cross p1 p2))

(def primitive-functions
  {:headings headings
   :magnitudes magnitudes
   :heading heading
   :magnitude magnitude
   :angle angle
   :dot dot
   :cross cross
   :dist dist
   :dist-abs dist-abs
   :dist-emd dist-emd
   :dist-cheb dist-cheb
   :dist-ang dist-ang
   :dist-canberra dist-canberra})

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
     :in-shift (v/generate-vec2 #(r/drand -1.0 1.0))
     :colorspace (rand-nth c/colorspaces-list) ;; random colorspace
     :fns (take 3 (shuffle (keys primitive-functions)))
     :pow1 (r/drand 0.5 5.0)
     :pow2 (r/drand 0.5 5.0)
     :pow3 (r/drand 0.5 5.0)}))

(defn plasma->buf
  [{:keys [f1 f2 ^double scale1 ^double scale2 ^double scale3
           n1 n2 n3 ^double in-scale ^double in-rot ^double in-shift
           colorspace ^double n1-scale ^double n2-scale ^double n3-scale
           ^double pow1 ^double pow2 ^double pow3
           fns]
    :as config}]
  (pp/pprint (dissoc config :n1 :n2 :n3 :f1 :f2))
  (let [-in-scale (- in-scale)
        [fn1 fn2 fn3] (map primitive-functions fns)
        field1 (f/combine f1)
        field2 (f/combine f2)
        noise1 (r/random-noise-fn n1)
        noise2 (r/random-noise-fn n2)
        noise3 (r/random-noise-fn n3)
        [_ convert-from] (c/colorspaces* colorspace)
        buf (p/renderer SIZE SIZE :gaussian 2.05 1.0) ;; blur a little bit
        js (cons [0.0 0.0] (take 5 (r/jittered-sequence-generator :r2 2))) ;; 6 points per pixel
        is (range (count js))
        xss (mapv first js)
        yss (mapv second js)]
    (doseq [^long x (range SIZE)
            ^long y (range SIZE)
            i is
            :let [^double xs (xss i) ;; shift x
                  ^double ys (yss i) ;; shift y
                  xx (m/norm (m/+ x xs -0.5) 0 SIZE -in-scale in-scale) ;; scale screen to field coords
                  yy (m/norm (m/+ y ys -0.5) 0 SIZE -in-scale in-scale)
                  in (-> (v/vec2 xx yy)    ;; input
                         (v/rotate in-rot) ;; rotate
                         (v/add in-shift)) ;; shift
                  p1 (field1 in)           ;; apply field 1
                  p2 (field2 in)           ;; apply field 2
                  c1 (* 255.0 (m/pow (m/abs (m/sin (m/* (m/+ ^double (fn1 p1 p2)
                                                             (m/* n1-scale
                                                                  ^double (noise1 (p1 0) (p1 1))))
                                                        scale1))) pow1))
                  c2 (* 255.0 (m/pow (m/abs (m/sin (m/* (m/+ ^double (fn2 p1 p2)
                                                             (m/* n2-scale
                                                                  ^double (noise2 (p2 0) (p2 1))))
                                                        scale2))) pow2))
                  c3 (* 255.0 (m/pow (m/abs (m/sin (m/* (m/+ ^double (fn3 p1 p2)
                                                             (m/* n3-scale
                                                                  ^double (noise3 (p1 1) (p2 0) 0.3456)))
                                                        scale3))) pow3))
                  col (-> (v/vec3 c1 c2 c3) convert-from)]]
      (p/set-color! buf x y col))
    buf))

(defonce noverlay (o/noise-overlay SIZE SIZE {:alpha 90}))

(defn draw-plasma [buf]
  (o/render-noise
   (c2d/get-image (p/to-pixels buf {:splats? true}))
   noverlay))

(def config (make-config))
(def buffer (plasma->buf config))
(def image (draw-plasma buffer))
(u/show-image image)

(comment
  (c2d/save image (c2d/next-filename "results/ex59/" ".jpg"))
  )
