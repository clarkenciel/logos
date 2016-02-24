(ns logos.core
  (:use [logos.utils]
        [logos.slides]
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
(def speaker-tb (text-box-fac
                 :font-color [0]
                 :top-margin 100
                 :left-margin 50
                 :word-limit 20))

(defn speaker-setup []
  (text-setup {:leading 10
               :size 15
               :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  {:draw false})

(defn speaker-click [s e]
  (do (next-slide)
      (assoc s
             :draw true
             :slide (speaker-tb ((deref current-slide) :body)))))

(defn speaker-draw [s]
  (draw-slide s))

;; Audience
(def audience-tb (text-box-fac))

(defn aud-setup []
  (text-setup {:leading 10
               :size 30
               :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  {:draw false
   :bg [240]})

(defn aud-update [s]
  (let [nuslide (or (when @current-slide
                      ((deref current-slide) :important))
                    (get s :slide nil))
        nubg (map #(constrain 0 255 (+ % (randrange -1 1))) (s :bg))]
    (assoc s
           :draw nuslide
           :slide (audience-tb nuslide :bg nubg))
    ))

(defn aud-draw [s]
  (draw-slide s))

;; fix make-viz api
((defn make-apps []
   (defapplet audience
     :size :fullscreen
     :setup aud-setup
     :update aud-update
     :draw aud-draw
     :features [:present :resizable]
     :middleware [m/fun-mode])

   (defapplet speaker 
     :size [700 700]
     :setup speaker-setup
     :draw speaker-draw
     :mouse-clicked speaker-click
     :key-pressend #(println %)
     :middleware [m/fun-mode])))

;; ============= MAIN

(comment

  (do
    (make-apps)
    (run-app speaker "Speaker" :p3d)
    (run-app audience "Audience" :p3d))
  
  (do (close-app speaker)
      (close-app audience))

  )
slide-source

(println "Ready!")
