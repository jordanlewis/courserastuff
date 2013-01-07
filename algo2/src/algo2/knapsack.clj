(ns algo2.knapsack
  [:use [clojure.string :only [split-lines split]]]
  (:gen-class))

(defn jmemoize [f]
  (let [mem (new java.util.HashMap)]
    (fn [& args]
      (if-let [e (.get mem args)]
        e
        (let [ret (apply f args)]
          (.put mem args ret)
          ret)))))

(def ^:dynamic l [])
(def ^:dynamic cap 0)
(def do-knapsack
  (jmemoize
    (fn [i x]
      (if (or (= i 0) (= x 0)) 0
        (let [newi (dec i)
              simple (do-knapsack newi x)
              [v w] (l newi)]
          (if (> w x) simple
            (max simple
                 (+ v (do-knapsack newi (- x w))))))))))

(defn knapsack [l]
  (let [[cap- n] (first l)
        r (into [] (rest l))]
    (binding [cap cap-
              l r]
      (do-knapsack n cap))))

;(do-knapsack 8 '[(15 1) (10 5) (9 3) (5 4)] 1 1)
;;(time (knapsack- '((8 4) (15 1) (10 5) (9 3) (5 4))))
;;(time (knapsack '((8 4) (15 1) (10 5) (9 3) (5 4))))

(defn read-knapsack [filename]
  (->> filename slurp split-lines (map #(map read-string (split % #" ")))
       time
       knapsack
       time))

(defn -main [& args]
  (prn (read-knapsack "knapsack2.txt")))

(read-knapsack "knapsack2.txt")
2493893
2595819
