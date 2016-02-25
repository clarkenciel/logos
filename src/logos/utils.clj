(ns logos.utils
  (:require [clojure.string :as s]))

;; ============= UTILS
(defn atom? [v]
  (instance? clojure.lang.Atom v))

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

(defn to-bool [v]
  (not (not v)))

(defn mod-inc [div num]
  (mod (inc num) div))

(defn ns-kw->str [ns-kw]
  (second (re-find #"/(.*)" (str ns-kw))))

(defn kw->str [kw]
  (if (namespace kw)
    (ns-kw->str kw)
    (second (re-find #":(.*)" (str kw)))))

(defn positions [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn id [x] x)

(defn take-sorted [n coll f]
  (take n (sort f coll)))

(defn n-greatest [n coll]
  (take-sorted n coll >))

(defn n-least [n coll]
  (take-sorted n coll <))

;; assoc-compare :: (Ord,Eq Val) =>
;;  (Val -> Val -> Bool) -> [(Key,Val)] -> (Key,Val) -> [(Key,Val)]
(defn assoc-compare [pred col p1]
  (let [k (first (keys p1))
        v1 (first (vals p1))]
    (if (not (some #{k} (keys col)))
      (merge col (hash-map k v1))
      (let [v2 (col k)]
        (if (pred v1 v2)
          (merge col (hash-map k v1))
          col)))))

(def assoc-gt (partial assoc-compare >))
(def assoc-gte (partial assoc-compare >=))
(def assoc-lt (comp not assoc-gte))
(def assoc-lte (comp not assoc-gt))

(defn random-walk [lo hi v]
  (+ v (randrange lo hi)))

(defn constrained-walk [clo chi wlo whi v]
  (constrain clo chi (random-walk wlo whi v)))
