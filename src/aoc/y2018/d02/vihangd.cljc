(ns aoc.y2018.d02.vihangd
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d02.data :refer [input answer-1 answer-2]]
   [clojure.string :as str]
   [clojure.test :refer [is testing]]))


(def data (str/split-lines input))

(defn get-twothrees [freqs] (filter (partial contains? #{2 3}) freqs) )


(defn solve-1 []
  (->> data
       (mapcat (comp set get-twothrees vals  frequencies))
       frequencies
       vals
       (apply *)))

(defn solve-2 []
  (let [data (sort data)] 
    (loop [fst (first data) rst (rest data) nxt (first rst)]
      (let [zipped-strs (map vector fst nxt)
            size (count zipped-strs)
            equal-chars (filter #(apply identical? %) zipped-strs)
            equal-size (count equal-chars)]
        (if (= 1 (- size equal-size ))
          (apply str (map first equal-chars))
          (recur nxt (rest rst) (first rst)))))))

(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (solve-2))))
