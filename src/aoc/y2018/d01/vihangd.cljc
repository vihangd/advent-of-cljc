(ns aos.y2018.d01.vihangd
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d01.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))

(def data
  (map u/read-string
       (str/split-lines input)))

(defn solve-1 []
  (apply + data)
)
(defn solve-2 []
  (loop [seen #{} coll (reductions + (cycle data))]
    (let [fst (first coll) rst (rest coll)]
      (if (contains? seen fst )
        fst
        (recur (conj seen fst) rst))
)))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))

