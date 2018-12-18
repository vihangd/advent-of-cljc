(ns aoc.y2018.d04.vihangd
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d04.data :refer [input answer-1 answer-2]] ;; :reload
   [clojure.test :refer [is testing]]
   [clojure.string :as str]))

(def data (str/split-lines input))

(def regex #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (Guard #(\d+) )?(.*)")

(defn parse-line [line]
  (let [[_ year month day hour min _ guard-id action] (re-find regex line)
        [year month day hour min](map u/parse-int [year month day hour min])]
    {:year year
     :month month
     :day day
     :hour hour
     :min min
     :guard-id  guard-id
     :action (case action
               "begins shift" :begins
               "falls asleep" :sleeps
               "wakes up" :wakes)}))


(defn populate-guard-ids [actions]
  (loop [action-list [] fst (first actions) rst (rest actions) current-guard-id nil start-min nil]
    (if fst
      (let [guard-id (:guard-id fst)]
          (case (:action fst) 
            :sleeps (recur action-list (first rst) (rest rst) current-guard-id (:min fst))
            :wakes (recur (conj action-list {:guard-id  current-guard-id :range (range start-min (:min fst))} ) (first rst) (rest rst) current-guard-id nil) 
            :begins (recur action-list (first rst) (rest rst) guard-id nil)))
      action-list)))

(defn parse-n-group [data]
  (->> data
       (map parse-line)
       (sort-by (juxt :year :month :day :hour :min))
       populate-guard-ids
       (group-by :guard-id)))

(defn solve-1 []
  (let [minmap (->> data
                    parse-n-group
                    (map (fn [[k v]] {:gid k :min (->> v (mapcat :range) count) :range   (mapcat :range v) }))
                    (apply max-key :min))]
    (* (u/parse-int (:gid minmap))
       (->> minmap :range frequencies (apply max-key val) key))))

(defn solve-2 []
  (let [minmap  (->> data
                     parse-n-group
                     (map (fn [[k v]] {:gid k  :freq (apply max-key val  (frequencies  (mapcat :range v) ))}))
                     (apply max-key (fn [x]  (val (:freq x)))))]
    (* (u/parse-int (:gid minmap)) (key (:freq minmap)))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

