(ns logos.utils
  (:require [clojure.string :as s]))

;; ============= UTILS
(defn atom? [v]
  (instance? clojure.lang.Atom v))

(defn reset-counter [c]
  (swap! c (fn [_] 0)))

(defn store-val [coll v]
  (swap! coll (fn [c]
                (conj c (if (atom? v) @v v)))))

(defn reset-val-store [coll]
  (swap! coll (fn [_] [])))

(defn randrange [lo hi]
  (+ lo (rand (- (- hi lo) 1))))

(defn constrain [lo hi v]
  (cond (> v hi) hi
        (< v lo) lo
        :else v))

(defn words [s]
  (s/split s #"\s"))

(defn unwords [l]
  (s/join " " (flatten l)))
