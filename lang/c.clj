(ffi print {:in [Int] :out Int} [char])

(defn add
  {:in [Int Int] :out Int}
  [x y]
  (+ x y))

(defn main
  {:in [String] :out Int}
  [args]
  (print 98))