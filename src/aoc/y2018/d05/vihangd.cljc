(ns aoc.y2018.d05.vihangd
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d05.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn reacts? [a b]
  (cond
    (nil? a) false
    (= a b) false
    (= (clojure.string/lower-case a) (str b)) true
    (= (clojure.string/upper-case a) (str b)) true))

(defn reduce-polymer [input]
  (loop [acc [] fst (first input) rst (rest input)]
    (if fst
      (if (reacts? (peek acc) fst)
        (recur (pop acc) (first rst) (rest rst))
        (recur (conj acc fst) (first rst) (rest rst)))
      acc)))

(defn solve-1 []
  (let [input (->> input  vec)]
    (count (reduce-polymer input))))


(defn solve-2 []
  (apply min
         (for [ch "abcdefghijklmnopqrstuvwxyz"]
           (count (reduce-polymer  (remove #{ch (->> ch clojure.string/upper-case vec first )} input ))))))


(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)
)
