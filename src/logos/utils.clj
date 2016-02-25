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

(defn n-greatest
  ([n coll]
   (n-greatest n coll >))
  ([n coll f]
   (take n (sort f coll))))

(defn n-least
  ([n coll]
   (n-least n coll <))
  ([n coll f]
   (take n (sort f  coll))))
