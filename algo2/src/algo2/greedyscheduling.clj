(ns algo2.greedyscheduling
  [:use [clojure.string :only [split-lines split]]])

(def job-weight first)
(def job-length second)

(defn completion-times [jobs]
  (reductions + (map job-length jobs)))

(defn weighted-sum-of-completion-times [jobs]
  (reduce + (map * (completion-times jobs)
                   (map job-weight jobs))))

(defn job-difference-keyfn [job]
  [(apply - job) (job-weight job)])

(defn job-ratio-keyfn [job]
  (apply / job))

(defn sort-jobs-by-difference [jobs]
  (sort-by job-difference-keyfn jobs))

(defn sort-jobs-by-ratio [jobs]
  (sort-by job-ratio-keyfn jobs))

(job-difference-keyfn '(3 5))
(weighted-sum-of-completion-times (sort-jobs-by-difference [[3 5] [1 2]]))
(weighted-sum-of-completion-times (sort-jobs-by-ratio [[3 5] [1 2]]))

(defn min-weighted-sum-of-completion-times [filename keyfn]
  (->> filename slurp split-lines rest
    (map #(map read-string (split % #" ")))
    (sort-by keyfn (comp - compare))
    weighted-sum-of-completion-times))

(min-weighted-sum-of-completion-times "jobs.txt" job-difference-keyfn)
(min-weighted-sum-of-completion-times "jobs.txt" job-ratio-keyfn)
