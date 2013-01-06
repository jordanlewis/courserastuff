(ns algo2.clustering
  [:use [clojure.string :only [split-lines split]]
        [algo2.unionfind]])

;; clusters is a map from element to canonical element.
(defn do-cluster [n k graph set]
  (let [e (first graph)
        [set x] (get-canonical set (first e))
        [set y] (get-canonical set (second e))]
    (cond (= x y) (recur n k (rest graph) set)
          (= n k) (nth e 2)
          :else (let [set (connect set x y)]
                  (recur n (dec k) (rest graph) set)))))

(defn cluster [graph]
  (let [sorted-edges (time (sort-by #(nth % 2) graph))
        set (reduce #(-> % (add-singleton (first %2)) (add-singleton (second %2)))
                    (make-set)
                    sorted-edges)]
    (do-cluster 4 (count @(:elt-map set)) sorted-edges set)))

(defn read-clustering [filename]
  (->> filename slurp split-lines rest
       (map #(map read-string (split % #" ")))
       cluster))

(time (read-clustering "clustering1.txt"))
