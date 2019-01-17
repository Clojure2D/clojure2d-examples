(ns examples.52-ml-glitch
  (:require [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [fastmath.classification :as cl]
            [fastmath.clustering :as clust]
            [fastmath.distance :as dist]
            [fastmath.core :as m]
            [fastmath.rbf :as rbf]
            [fastmath.random :as r]
            [fastmath.kernel.mercer :as kernel]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; load image
(def img (p/load-pixels "results/test.jpg"))

;; reduce colors using clustering, clasters representatives will be our labels
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
                              (let [x (int (* x ^int (width img)))
                                    y (int (* y ^int (height img)))
                                    p (p/get-color img x y)
                                    l (labels (clusters p))]
                                [(color-vector img x y) l])) (r/jittered-sequence-generator :r2 2 0.5))))

;; take data and labels from samples
(def data (map first samples))
(def data-labels (map second samples))

;; train classifier
(def cl (cl/ada-boost {:number-of-trees 10 :max-nodes 40} data data-labels))
;; (def cl (cl/decision-tree {:split-rules :entropy :max-nodes 10 :node-size 50} data data-labels))
;; (def cl (cl/gradient-tree-boost data data-labels)) ;; slow!!!
;; (def cl (cl/knn data data-labels))
;; (def cl (cl/lda data data-labels))
;; (def cl (cl/liblinear {:solver :l2r-l1loss-svc-dual :max-iters 100 :C 100 :eps 0.1} data data-labels))
;; (def cl (cl/logistic-regression {:lambda 0.5 :iterations 100} data data-labels))
;; (def cl (cl/neural-net {:layers [200 100 70 50] :activation-function :linear} data data-labels))
;; (def cl (cl/rda {:alpha 0.7} data data-labels))
;; (def cl (cl/random-forest {:node-size 50 :number-of-trees 50 :subsample 0.9} data data-labels))
;; (def cl (cl/rbf-network {:distance dist/manhattan} data data-labels))
;; (def cl (cl/rbf-network {:rbf (take 9 (cycle [(rbf/rbf :inverse-quadratic) (rbf/rbf :wu) (rbf/rbf :thinplate)])) :distance dist/chebyshev} data data-labels))
;; (def cl (cl/xgboost data data-labels))
;; (def cl (cl/svm {:kernel (kernel/kernel :hyperbolic-tangent) :tolerance 0.1 :c-or-cp 10.0} data data-labels))

(def c (canvas (width img) (height img)))
(def p (p/clone-pixels img))
(show-window {:canvas c
              :draw-fn (fn [c _ _ _]
                         (p/set-canvas-pixels! c p))})


;; reconstruct image partly from guessed pixels, partly from image
(dotimes [y (height img)]
  (dotimes [x (width img)]
    (let [col (cl (color-vector p x y))]
      (p/set-color p x y col))))

(comment (save p (next-filename "results/ex52/" ".jpg")))
