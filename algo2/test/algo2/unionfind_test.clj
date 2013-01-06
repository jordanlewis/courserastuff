(ns algo2.unionfind_test
  (:use midje.sweet
        algo2.unionfind))

(defn test-sets [make-empty-set]
  (fact "Singleton sets are their own leaders"
        (let [singleton (add-singleton (make-empty-set) 3)]
          (second (get-canonical singleton 3)) => 3))
  (fact "Singleton sets unioned with themselves are still their own leaders"
        (let [singleton (add-singleton (make-empty-set) 3)
              unioned (connect singleton 3 3)]
          (second (get-canonical unioned 3)) => 3))
  (fact "Connected sets have the same leader"
        (let [set (-> (make-empty-set)
                      (add-singleton 1)
                      (add-singleton 2)
                      (add-singleton 3)
                      (add-singleton 4)
                      (add-singleton 5)
                      (add-singleton 6)
                      (connect 1 2)
                      (connect 3 4)
                      (connect 2 3)
                      (connect 5 6))]
          (second (get-canonical set 1)) => (second (get-canonical set 2))
          (second (get-canonical set 2)) => (second (get-canonical set 3))
          (second (get-canonical set 3)) => (second (get-canonical set 4))
          (second (get-canonical set 5)) => (second (get-canonical set 6))
          (let [newset (connect set 2 6)]
            (second (get-canonical newset 4)) => (second (get-canonical newset 5))))))


(test-sets make-set)
(test-sets make-persistent-set)
