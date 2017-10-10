(defn add
  {:in [Int Int] :out Int}
  [x y]
  (+ x y))

(defn main
  {:in [String] :out Int}
  [args]
  (add 1 2))