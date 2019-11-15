(ns GG.M.M-6-1-02
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 600)
(def ^:const ^int h 600)
(def ^:const node-damping 0.1)
(def ^:const spring-damping 0.3)
(def ^:const stiffness 0.6)
(def ^:const length 100.0)

(defrecord Node [pos velocity])

(defn update-node
  ""
  [node]
  (let [velocity (v/limit (:velocity node) 10.0)
        npos (v/add (:pos node) velocity)]
    (Node. npos (v/mult velocity (- 1.0 node-damping)))))

(defrecord Spring [from-node to-node])

(defn update-spring
  ""
  [{:keys [from-node to-node]}]
  (let [diff (-> (v/sub (:pos to-node) (:pos from-node))
                 (v/normalize)
                 (v/mult length))
        target (v/add (:pos from-node) diff)
        force (-> (v/sub target (:pos to-node))
                  (v/mult (* 0.5 stiffness (- 1.0 spring-damping))))]
    (Spring.
     (update-node (Node. (:pos from-node) (v/add (:velocity from-node) (v/mult force -1.0))))
     (update-node (Node. (:pos to-node) (v/add (:velocity to-node) force))))))

(defn draw
  ""
  [canvas window _ spring]
  (let [{:keys [from-node to-node]} (or spring (Spring. (Node. (v/vec2 (+ (r/drand -50 50) (/ w 2))
                                                                       (+ (r/drand -50 50) (/ h 2)))
                                                               (v/vec2 0 0))
                                                        (Node. (v/vec2 (+ (r/drand -50 50) (/ w 2))
                                                                       (+ (r/drand -50 50) (/ h 2)))
                                                               (v/vec2 0 0))))
        
        from-node (if (and (mouse-pressed? window)
                           (pos? (mouse-x window))
                           (pos? (mouse-y window)))
                    (Node. (mouse-pos window) (:velocity from-node))
                    from-node)]
    
    (-> canvas
        (set-background :white)
        (set-stroke 4.0)
        (set-color 0 130 164)
        (line ((:pos to-node) 0)
              ((:pos to-node) 1)
              ((:pos from-node) 0)
              ((:pos from-node) 1))
        (set-color :black)
        (ellipse ((:pos to-node) 0)
                 ((:pos to-node) 1) 20 20)
        (ellipse ((:pos from-node) 0)
                 ((:pos from-node) 1) 20 20))

    (update-spring (Spring. from-node to-node))))

(def window (show-window {:canvas (canvas w h)
                          :draw-fn draw}))
