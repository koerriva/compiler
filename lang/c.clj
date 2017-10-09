(ffi print {:in [String] :out Int} [str])
(defn add
  {:in [Int Int] :out Int}
  [x y]
  (+ x y))
(defn main
  {:in [String] :out Int}
  [args]
  (print "sss"))