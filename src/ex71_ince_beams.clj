;; ince beams example - idea by @CTW (Thomas Clark)
;; https://en.wikipedia.org/wiki/Gaussian_beam#Ince-Gaussian_modes
;; https://www.researchgate.net/publication/8567409_Ince-Gaussian_modes_of_the_paraxial_wave_equation_and_stable_resonators
;; Clojure Conj 2024: https://youtu.be/_D5d6Ls6pBw?t=1951
(ns ex71-ince-beams
  (:require [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.polynomials :as poly]
            [clojure2d.extra.utils :as u]
            [clojure2d.color :as c]
            [fastmath.complex :as cplx]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn rayleigh-range
  ^double [^double waist ^double wavelength]
  (m// (m/* m/PI (m/sq waist)) wavelength))

(defn width
  ^double [^double z ^double zR ^double waist]
  (m/* waist (m/sqrt (m/inc (m/sq (m// z zR))))))

(defn csq
  ^double [^double ellipticity ^double waist]
  (m/inc (m/* 0.5 (m/sq waist) ellipticity)))

(defn IG0
  "An Ince-Gaussian beam function for z=0."
  [p m ellipticity waist wavelength]
  (let [ellipticity (double ellipticity)
        waist (double waist)
        zR (rayleigh-range waist wavelength)
        w (width 0.0 zR waist)
        ince (poly/ince-C p m ellipticity)
        ince-radial (poly/ince-C-radial p m ellipticity)
        k (m// m/TWO_PI (double wavelength))
        rscale (m// (m/* w (m/sqrt (m/* 0.5 ellipticity))))]

    (fn ^double [^double x ^double y]
      (let [^Vec2 xi-eta (cplx/acosh (cplx/scale (cplx/complex x y) rscale))
            r**2 (m/+ (m/sq x) (m/sq y))]
        (m/* (m// waist w)
             (double (ince-radial (xi-eta 0)))
             (double (ince (xi-eta 1)))
             (m/exp (m/* -1.0 k
                         (m// r**2 (m/* 2.0 zR)))))))))

(defn IG-profile
  ([IG-fn]
   (IG-profile IG-fn [0 0] [200 200]))
  ([IG-fn [^double x0 ^double y0] [^long width ^long height]]
   (let [hw (m// width 2.0)
         hh (m// height 2.0)]
     (for [^long x (range width)
           ^long y (range height)]
       (m/sq (IG-fn (m/+ x0 (m/- x hw))
                    (m/+ y0 (m/- y hh))))))))

(defn normalize
  "Peak normalize a tensor."
  ;; TODO: extend to deal with non-zero floored tensors.
  [d]
  (let [peak (double (reduce m/max d))]
    (map (fn [^double v] (m// v peak)) d)))

(defn profile [p m ellipticity waist]
  (-> (IG0 p m ellipticity waist 0.78)
      (IG-profile [0.0 0.0] [100 100])
      (normalize)))

(defn show-profile
  [p m ellipticity waist]
  (let [p (profile p m ellipticity waist)
        g (c/gradient [:black (c/gray 200) :white])]
    (c2d/with-canvas [c (c2d/canvas 800 800)]
      (c2d/set-background c :black)
      (doseq [[^long i ^double v] (map-indexed vector p)
              :let [x (m/* 8 (m/quot i 100))
                    y (m/* 8 (m/rem i 100))]]
        (c2d/set-color c (g v))
        (c2d/rect c x y 8 8))
      (u/show-image c))))

(show-profile 11 5 7 11)

(defn render-profile
  [p m ellipticity waist wavelength]
  (let [buffer (p/gradient-renderer 800 800 :sinc 1)
        r (range -100 100 0.05)
        n (m/make-norm -100 100 0 800)
        p (IG0 p m ellipticity waist wavelength)]
    (doseq [^double x r ^double y r
            :let [x (m/+ x (r/grand 0.05))
                  y (m/+ y (r/grand 0.05))
                  xx (n x) yy (n y)
                  v (m/sq (double (p x y)))]]
      (p/add-pixel! buffer xx yy v))
    (u/show-image (p/to-pixels buffer {:logarithmic? true}))))

(render-profile 7 5 3 15 0.78)

