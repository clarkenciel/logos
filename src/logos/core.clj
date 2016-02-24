;; TODO:
;; Transformations of slide texts can be mapped
;;   over the slides lazy seq

(ns logos.core
  (:use [logos.utils]
        [logos.slides]
        [logos.sc]
        [logos.applet]
        [logos.q])
  (:require [quil.middleware :as m]))



;; Slide Management
(def slide-index (atom -1))

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
  (let [nu-index (swap! slide-index (partial mod-inc (count slides)))]
    (when (and (sc-on?)
               (>= nu-index 0))
      (make-listeners))
    (assoc s :draw true
             :slide (speaker-tb ((get-slide slides nu-index) :body)))))

(defn speaker-draw [s]
  (draw-slide s))

;; Audience
(def audience-tb (text-box-fac))

(defn aud-setup []
  (text-setup {:leading 10
               :size 30
               :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  {:last-slide-index @slide-index
   :draw false
   :bg [240]})

(defn aud-update [s]
  (if (= @slide-index (s :last-slide-index))
    s
    (let [maybe-slide (get-slide slides @slide-index)
          nuslide     (if (empty? maybe-slide)
                        (get (get s :slide nil) :text "")
                        (maybe-slide :important))
          nubg        (map #(constrain 240 255 (+ % (randrange -1 1)))
                           (s :bg))]
      (assoc s :last-slide-index (inc (s :last-slide-index))
               :draw (to-bool nuslide)
               :bg nubg
               :slide (audience-tb nuslide :bg nubg)))))

(defn aud-draw [s]
  (draw-slide s))

;; fix make-viz api
(defn make-apps []
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
    :middleware [m/fun-mode]))

;; ============= MAIN

;; need to create at least one quil applet before booting overtone
;; for some reason: https://github.com/overtone/overtone/issues/313
;; define and call this function simultaneously
(defn safe-start []
  (do
    (throwaway)
    (sc-start)
    (make-busses)
    (sc-setup 0 0)
    (make-apps)
    (run-app audience "Audience" :p3d)
    (run-app speaker "Speaker" :p3d)))

(comment

  (do
    (make-apps)
    (run-app audience "Audience" :p3d)
    (run-app speaker "Speaker" :p3d))
  
  (do (close-app speaker)
      (close-app audience))


  (safe-start)

  (sc-stop)
  
  slides
  )

(println "Ready!")
