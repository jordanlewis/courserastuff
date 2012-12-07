(ns algo2.prim
  [:use [clojure.string :only [split-lines split]]])

(defn prim [graph]
  (let [sortedgraph (sort-by #(nth % 2) graph)
        startv (->> graph (map #(take 2 %)) flatten set)]
    ((fn [v e]
      (if (= v startv) (reduce + (map #(nth % 2) e))
        (let [minedge (first (filter (fn [e] (= 1 (count (filter identity (map #(v (nth e %)) '(0 1)))))) sortedgraph))]
          (recur (apply conj v (take 2 minedge)) (conj e minedge)))))
    #{(ffirst graph)} #{})))

(defn read-prim [filename]
  (->> filename slurp split-lines rest
    (map #(map read-string (split % #" ")))
    prim))

(def g [['a 'b 7] ['a 'd 5] ['b 'c 8] ['b 'e 7] ['b 'd 9] ['c 'e 5] ['d 'e 15] ['d 'f 6] ['e 'g 9] ['e 'f 8] ['f 'g 11]])
(->> g (map (fn [edge] (take 2 edge))) flatten set)
(prim g)
(read-prim "edges.txt")
