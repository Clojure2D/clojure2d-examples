(ns neuroevolution.simulate
  (:require [neuroevolution.chromosome :as chromosome]
            [neuroevolution.environment :as environment]
            [neuroevolution.car]
            [fastmath.vector :as v]
            [neuroevolution.nn :as nn]
            [fastmath.stats :as stats]
            [fastmath.core :as m])
  (:import [neuroevolution.environment Environment Dot]
           [fastmath.vector Vec2]
           [neuroevolution.car Car]
           [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn random-population
  [population-size profile]
  (repeatedly population-size #(chromosome/random-net profile)))

(defn step
  [^Environment env profile-eval chromosome]
  (let [^Car car (.car env)
        ^Dot dot (.dot env)        
        ^Vec2 dir (.direction car)
        ^Vec2 to-dot (v/sub (.dot dot) (.position car))
        dist (v/mag to-dot)
        cang (m/cos (v/angle-between to-dot dir))
        in? (or (m/< dist 100.0) (m/> cang 0.99))
        ^doubles res (nn/evaluate profile-eval chromosome
                                  (double-array [(if in? dist -10.0) 
                                                 cang
                                                 (condp = (.col dot)
                                                   :orange 10.0
                                                   :violet -10.0
                                                   0.0)
                                                 1]))]
    (environment/step env (Array/aget res 0) (Array/aget res 1))))

(defn run-game
  [^Environment env profile-eval chromosome]
  (if (.game-over? env)
    (.score env)
    (recur (step env profile-eval chromosome) profile-eval chromosome)))

(defn run-game-multiple-envs
  [environments profile-eval chromosome]
  [(stats/sum (map #(run-game % profile-eval chromosome) environments)) chromosome])

(defn crossover-and-mutate
  [population size elite-size profile]
  (->> (shuffle (concat population population (random-population elite-size profile)))
       (partition 2 1)
       (mapcat chromosome/crossover)
       (map (partial chromosome/mutate 0.2))
       (take size)))

(defn evolve
  [{:keys [population ^long elite-size ^long population-size profile]
    :as state}]
  (let [elite (take elite-size population)]
    (assoc state :population (concat elite
                                     (map (partial chromosome/mutate 0.4) elite)
                                     (crossover-and-mutate (take (m// population-size 2) population)
                                                           (m/- population-size (m/* 2 elite-size))
                                                           elite-size profile)))))

(defn init-state
  ([] (init-state {}))
  ([{:keys [^long population-size ^double elite-percent environments-count profile
            environment-config car-config]
     :or {population-size 600 elite-percent 0.1 environments-count 1 profile [3 3 2]}}]
   {:generation 0
    :population (random-population population-size profile)
    :population-size population-size
    :elite-size (unchecked-int (* elite-percent population-size))
    :environments-count environments-count
    :environments (repeatedly environments-count
                              #(environment/environment 800.0 environment-config car-config))
    :profile profile
    :profile-eval (vec (rest profile))}))

(defn run-all-games
  [{:keys [^long generation population environments ^long environments-count profile-eval]
    :as state}]
  (let [stage1 (->> (pmap (partial run-game-multiple-envs environments profile-eval) population)
                    (sort-by first >))
        scores (map first stage1)]
    (println "Generation:" generation
             "| avg. best:" (m// (double (first scores)) environments-count)
             "| avg. pop.:" (m// (stats/mean scores) environments-count))
    (-> state
        (assoc :generation (m/inc generation)
               :population (map second stage1))
        (evolve))))

(defn reset-envs
  [state]
  (assoc state :environments (repeatedly (:environments-count state) environment/environment)))

(defn run-all-games-n
  ([state ^long n reset-envs?]
   (run-all-games-n (if reset-envs? (reset-envs state) state) n))
  ([state ^long n]
   (if (m/zero? n)
     state
     (recur (run-all-games state) (m/dec n)))))

(defn mix-runs
  [state1 state2]
  (assoc state1
         :generations (/ (+ (long (:generations state1)) (long (:generations state2))) 2)
         :population (take (:population-size state1) (interleave (:population state1) (:population state2)))))


