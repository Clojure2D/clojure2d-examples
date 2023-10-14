(ns examples.ex58-clustering
  (:require [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [fastmath.clustering :as mc]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [clojure2d.extra.utils :as xu]))


;; inspired by https://mattdesl.svbtle.com/pen-plotter-2

(def ^:const ^double iwidth 1000)
(def ^:const ^double margin (* 0.02 iwidth))
(def ^:const ^double linew (* 0.002 iwidth))
(def ^:const ^long cluster-count 3)
(def ^:const ^long point-count 90000)
(def ^:const ^double cl-perc (/ 1 (double cluster-count)))


(def points-low (repeatedly
              5000
              (fn* []
                (vector
                 (r/drand margin (- iwidth margin))
                 (r/drand margin (- iwidth margin))))))
(def points (repeatedly
              point-count
              (fn* []
                (vector
                 (r/drand margin (- iwidth margin))
                 (r/drand margin (- iwidth margin))))))

(def circle (repeatedly
              point-count
              (fn* []
                   (let [c (/ iwidth 2.0)                         
                         r (r/drand 0.0 (- c margin))
                         ang (r/drand 0 m/TWO_PI)]
                     (vector
                      (+ c (* (Math/cos ang) r ))
                      (+ c (* (Math/sin ang) r )))))))


(def circle-norm (repeatedly
              point-count
              (fn* []
                   (let [c (/ iwidth 2.0)
                         r (* (- c margin) (min 1.0 (r/grand 1 0.2 )))
;                         r (r/drand 0.0 (- c margin))
                         ang (r/drand 0 m/TWO_PI)]
                     (vector
                      (+ c (* (Math/cos ang) r ))
                      (+ c (* (Math/sin ang) r )))))))


(def guell [0xD9B993,0xC66403,0x1C477E,0xB12903,0x1A422A,0xD9B993,0xC66403,0x1C477E])

(defn cross [[ax ay] [bx by] [cx cy]]
  (- (* (- bx ax) (- cy ay)) (* (- by ay)(- cx ax))))


(defn convex-hull [ps]
  (let [pss (sort (fn [[x1 y1] [x2 y2]] (compare [x1 y1] [x2 y2]))     ps)
        n (count ps)]

    (if (< n 3) ps
        (let [hh (loop [sz 0
                        i 0
                        h []]
                   (cond
                     (= i n) h
                     (and (> sz 1) (<= (cross (nth h (- sz 2)) (nth h (- sz 1)) (nth pss i)) 0)) (recur (dec sz) i (vec (butlast h)))
                     :else (recur (inc sz) (inc i) (conj h (nth pss i)))) )
              j (count hh)]

          (loop [sz j
                 i (- n 3)
                 h hh]
            (cond
              (neg? i) (butlast h)
              (and (>= sz j) (<= (cross (nth h (- sz 2)) (nth h (- sz 1)) (nth pss i)) 0)) (recur (dec sz) i (vec (butlast h)))
              :else (recur (inc sz) (dec i) (conj h (nth pss i)))))))))


(defn step-clusters-pal [draw pal cvs clusters]

  (let [children (filter #(>= (count %1) 3) (map :data (apply concat (map (fn [c]  (mc/regroup (mc/neural-gas c cluster-count))) clusters)))) ; children with at least 3 points
        groups (group-by #(or (>= (count %1) (* cl-perc  point-count)) (and (pos? (r/drand -50 100)) (>= (count %1) 100))) children )
       
        huls (map convex-hull  (get groups false))] 
    (doseq [h huls]
      (set-color cvs (nth pal (r/irand (count pal))   ))
      (path cvs h true false)

      (set-color cvs draw)
      (path cvs h true true))

    (get groups true)))

(defn step-clusters-pal-div [draw pal cvs clusters]

  (let [children (filter #(>= (count %1) 3) (map :data (apply concat (map (fn [c]  (mc/regroup (mc/neural-gas c cluster-count))) clusters)))) ; children with at least 3 points
        groups (group-by #(>= (count %1) 200) children )
       
        huls (map convex-hull (get groups false))] 
    (doseq [h huls]
      (set-color cvs (nth pal (r/irand (count pal))   ))
      (path cvs h true false)

      (set-color cvs draw)
      (path cvs h true true))

    (get groups true)))




(defn draw-points-pal [bg draw pal c w f points]
  (when (zero? f)
    (set-background c bg))
  (set-stroke c linew)
  (step-points draw (nth pal (r/irand (count pal))   ) c points)
) 


(defn draw-clusters-pal [bg draw pal c w f clusters]
  (when (zero? f)
    (set-background c bg))
  (set-stroke c linew)
  (step-clusters-pal draw pal c clusters)
  )

(defn draw-clusters-pal-div [bg draw pal c w f clusters]
  (when (zero? f)
    (set-background c bg))
  (set-stroke c linew)
  (step-clusters-pal-div draw pal c clusters)
) 



(defn example-p [p s]
  (let [c (canvas iwidth iwidth)
        wnd-name "points"]
    (show-window {:canvas  c :window-name  wnd-name :draw-fn (partial draw-points-pal :white :white p  ):draw-state s :w 1000 :h 1000})
        (defmethod key-pressed [wnd-name \space] [_ _]
      (save c (next-filename "results/ex58/" ".png")))
    ))


(defn example-c [p s]
  (let [c (canvas iwidth iwidth)
        wnd-name "clusters"]
    (show-window {:canvas  c :window-name  wnd-name :draw-fn  (partial draw-clusters-pal :white :white p ) :draw-state s :w 1000 :h 1000})
        (defmethod key-pressed [wnd-name \space] [_ _]
      (save c (next-filename "results/ex58/" ".png")))
    ))


(defn example-cs [ s]
  (let [c (canvas iwidth iwidth)
        wnd-name "clusters no fill"]
    (show-window {:canvas  c :window-name  wnd-name :draw-fn  (partial draw-clusters-pal :white :black [:white] ) :draw-state s :w 1000 :h 1000})
        (defmethod key-pressed [wnd-name \space] [_ _]
      (save c (next-filename "results/ex58/" ".png")))
    ))



(defn example-cd [p s]
  (let [c (canvas iwidth iwidth)
        wnd-name "clusters divide all"]
    (show-window {:canvas  c :window-name  wnd-name :draw-fn  (partial draw-clusters-pal-div :white :white p ) :draw-state s :w 1000 :h 1000})
        (defmethod key-pressed [wnd-name \space] [_ _]
      (save c (next-filename "results/ex58/" ".png")))
    ))

(comment

  (example-p [:black] points) ;; single color
  (example-p [:black] circle) ;; single color  
  (example-p guell points)
  (example-p guell circle)

  (example-cd guell [points])
  (example-cd guell [circle])
  (example-cd guell [circle-norm])
  
  (example-c guell [circle])
  (example-c guell [points])
  (example-c guell [circle-norm])
  )

