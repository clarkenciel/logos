(ns logos.core
  (:use [logos.q]
        [logos.applet]
        [logos.sc]))

;; SETUP

;; need to create at least one quil applet before booting overtone
;; for some reason: https://github.com/overtone/overtone/issues/313
;; define and call this function simultaneously
(defn safe-start []
  (do
    (throwaway)
    (sc-start)))

;; UTILS
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

;; MAIN
;; NB: need to have separate functions for each applet
(defn setup1 [] {:x 50 :y 50})
(defn updte1 [{x :x y :y :as s}]
  (assoc s
         :x (+ x (randrange -1 1))
         :y (+ y (randrange -1 1))))
(defn draw1 [{x :x y :y}]
  (circle x y 10))

(defn setup2 [] {:x 50 :y 50})
(defn updte2 [{x :x y :y :as s}]
  (assoc s
         :x (+ x (randrange -1 1))
         :y (+ y (randrange -1 1))))
(defn draw2 [{x :x y :y}]
  (circle x y 10))

(def speaker (make-viz [700 700] setup1 updte1 draw1))
(def audience (make-viz :fullscreen setup2 updte2 draw2))

(comment
  (run-viz speaker "hi" :p2d)
  (run-viz audience "hi" :p2d)
  (close-viz speaker)
  (close-viz audience)
  )
