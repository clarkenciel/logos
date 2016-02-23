(ns logos.core
  (:use [logos.slides]
        [logos.q]
        [logos.applet]
        [logos.sc])
  (:require [quil.middleware :as m]))

;; ============= SETUP

;; need to create at least one quil applet before booting overtone
;; for some reason: https://github.com/overtone/overtone/issues/313
;; define and call this function simultaneously
(defn safe-start []
  (do
    (throwaway)
    (sc-start)))

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

;; Slide Management
(defn atomic-pop-fac
  "Returns a function that will pop a value off of an atomic list
  and place it in another atom."
  [source proxy]
  (fn []
    (swap! proxy  (fn [_] (first @source)))
    (swap! source (fn [x] (rest x)))
    @proxy))

(def slides (atom (rest slide-source)))
(def current-slide (atom (first slide-source)))
(def next-slide (atomic-pop-fac slides current-slide))

;; ============= Slide Rendering
;; NB: need to have separate functions for each applet

(defn draw-slide [s]
  (let [d (get s :draw false)]
    (when d
      (text-box (s :slide)))))

;; Speaker
(def speaker-tb (text-box-fac))

(defn speaker-setup []
  (text-setup {:leading 10
               :size 15
               :font-color [0]
               :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  {:draw false})

;; TODO: Figure out how to do updating properly between the two
;; sets of slides
;; Only speaker-click advances the slide
(defn speaker-click [s e]
  (do (next-slide)
      (assoc s
             :draw true
             :slide (speaker-tb ((deref current-slide) :body)))))

(defn speaker-draw [s]
;;  (text-box (speaker-tb (s :slide)))
  (draw-slide s))

;; Audience
(def audience-tb (text-box-fac))

(defn aud-setup []
  (text-setup {:leading 10
               :size 30
               :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  {:draw false})

(defn aud-click [s e]
  (assoc s
         :draw true
         :slide (audience-tb ((deref current-slide) :important))))

(defn aud-draw []
  (draw-slide {:draw true
               :slide (merge (audience-tb ((deref current-slide) :important))
                             {:bg [(randrange 0 100)]})}))

;; fix make-viz api
(defapplet speaker 
  :size [700 700]
  :setup speaker-setup
  :draw speaker-draw
  :mouse-clicked speaker-click
  :middleware [m/fun-mode])

(defapplet audience
  :size :fullscreen
  :setup aud-setup
  :draw aud-draw
  :features [:present :resizable]
  :middleware [m/pause-on-error])

;; ============= MAIN

(comment

  (do (run-app speaker "Speaker" :p3d)
      (run-app audience "Audience" :p3d))

  (do (close-app speaker)
      (close-app audience))

  )
