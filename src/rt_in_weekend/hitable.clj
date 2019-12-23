(ns rt-in-weekend.hitable)

(deftype HitData [^double t p normal material])

(defprotocol HitableProto
  (hit [object ray t-min t-max]))

(defn hit-list
  "Traverse all scene objects."
  [xs ray t-min t-max]
  (reduce (fn [curr-hit object]
            (if-let [hit-object (hit object ray t-min (if curr-hit (.t ^HitData curr-hit) t-max))]
              hit-object
              curr-hit)) nil xs))
