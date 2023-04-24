(ns hooks.rt4)

(defmacro pdotimes
  [[l s _] & r]
  `(let [~l ~s] ~@r))
