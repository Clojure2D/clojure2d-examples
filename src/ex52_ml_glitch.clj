;; steps:
;;
;; * load image
;; * k-means clustering to reduce colors
;; * treat centroids as labels
;; * take some banch of pixels + surroundings, construct vector and add label
;; * train classification model
;; * predict pixel color from the image (render progressively, so vector used in prediction consist image colors and already predicted colors)
;; * draw an image

(ns examples.ex52-ml-glitch
  (:require [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [fastmath.classification :as cl]
            [fastmath.clustering :as clust]
            [fastmath.distance :as dist]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.kernel :as k]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; load image
(def img (p/load-pixels "results/test.jpg"))

;; reduce colors using clustering, clusters representatives will be our labels
(def clusters (clust/k-means img 50))

;; labels!
(def labels (vec (:representatives clusters)))

;;
(defn surrounding-idxs
  "Get indexes for pixels around given position."
  [^long x ^long y] 
  [[(dec x) y]
   [(inc x) y]
   [(dec x) (inc y)]
   [x (inc y)]
   [(inc x) (inc y)]
   [(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]
   ;; [x y]
   ])

(defn color-vector
  "Convert given position into vector of surrounding colors (skip alpha channel)."
  [img ^long x ^long y]
  (map #(/ ^double % 255.0) (mapcat (fn [[x y]] (take 3 (p/get-color img x y))) (surrounding-idxs x y))))

;; collect samples from image
(def samples (take 800 (map (fn [[^double x ^double y]]
                              (let [x (int (* x (width img)))
                                    y (int (* y (height img)))
                                    p (p/get-color img x y)
                                    l (labels (clusters p))]
                                [(color-vector img x y) l])) (r/jittered-sequence-generator :r2 2 0.5))))

;; take data and labels from samples
(def data (map first samples))
(def data-labels (map second samples))

;; train classifier
(def cl (cl/train (cl/ada-boost {:number-of-trees 10 :max-nodes 40} data data-labels)))
;; (def cl (cl/train (cl/decision-tree {:split-rules :entropy :max-nodes 10 :node-size 50} data data-labels)))
;; (def cl (cl/train (cl/gradient-tree-boost data data-labels))) ;; slow!!!
;; (def cl (cl/train (cl/knn data data-labels)))
;; (def cl (cl/train (cl/lda data data-labels)))
;; (def cl (cl/train (cl/liblinear {:solver :l2r-l1loss-svc-dual :max-iters 100 :C 100 :eps 0.1} data data-labels)))
;; (def cl (cl/train (cl/logistic-regression {:lambda 0.5 :iterations 100} data data-labels)))
;; (def cl (cl/train (cl/neural-net {:layers [200 100 70 50]} data data-labels)))
;; (def cl (cl/train (cl/rda {:alpha 0.7} data data-labels)))
;; (def cl (cl/train (cl/random-forest {:node-size 50 :number-of-trees 50 :subsample 0.9} data data-labels)))
;; (def cl (cl/train (cl/rbf-network {:distance dist/manhattan} data data-labels)))
;; (def cl (cl/train (cl/rbf-network {:rbf (take 9 (cycle [(k/rbf :inverse-multiquadratic) (k/rbf :wu-20) (k/rbf :thin-plate)])) :distance dist/chebyshev} data data-labels)))
;; (def cl (cl/train (cl/svm {:kernel (k/kernel :hyperbolic-tangent) :tolerance 0.1 :c-or-cp 10.0} data data-labels)))

(def c (canvas (width img) (height img)))
(def p (p/clone-pixels img))
(show-window {:canvas c
              :draw-fn (fn [c _ _ _]
                         (p/set-canvas-pixels! c p))})


;; reconstruct image partly from guessed pixels, partly from the image
(dotimes [y (height img)]
  (dotimes [x (width img)]
    (let [col (cl (color-vector p x y))]
      (p/set-color! p x y col))))

(comment (save p (next-filename "results/ex52/" ".jpg")))
