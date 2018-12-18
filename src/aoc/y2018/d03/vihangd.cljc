(ns aoc.y2018.d03.vihangd
  (:require
   [aoc.utils :as u :refer [deftest]]
   [aoc.y2018.d03.data :refer [input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(def data (str/split-lines input))

(defn parse-claim [line]
  (map u/parse-int (re-seq #"\d+" line)))

(defn get-rects [_ xpos ypos x y]
  (for [xi (range xpos (+ xpos x)) yi (range ypos (+ ypos y))] [xi yi]) )

(defn solve-1 []
  (->> data
       (mapcat (comp #(apply get-rects %) parse-claim))
       frequencies
       vals
      (filter #(< 1 %))
       count))


(defn get-rects2 [id xpos ypos x y]
  (for [xi (range xpos (+ xpos x)) yi (range ypos (+ ypos y))] {:id id :x xi :y yi}) )

(defn map-function-on-map-vals [f m]
  (zipmap (keys m) (map f (vals m))))


(defn solve-2 []
  (let [claims (map parse-claim data)
        claim-map (into {} (for [[p q r s t]  claims]  [p (* s t)]))
        final-map (->> claims
                       (mapcat #(apply get-rects2 %) )
                       (group-by #(select-keys % [:x :y]))
                       (filter #(= 1 (count (second %))))
                       vals
                       (map first)
                       (group-by :id)
                       (map-function-on-map-vals count ))]
    (first (keys (filter #(true? (second %)) (merge-with = claim-map final-map))))))

(solve-2)
(deftest part-1
  (is (= answer-1 (solve-1))))

(deftest part-2
  (is (= answer-2 (str (solve-2)))))
