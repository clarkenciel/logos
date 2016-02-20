(ns logos.core
  (:use [logos.q])
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; SETUP

;; need to create at least one quil applet before booting overtone
;; for some reason: https://github.com/overtone/overtone/issues/313
;; define and call this function simultaneously
((defn safe-start []
    (do
      (q/defsketch foo :size [0 0])
      (quil.applet/applet-disposed foo)
      (require '(overtone [live :as ol]))
      (use 'logos.sc))))

;; UTILS

;; TODO: should move fft-polling out of draw and into update

(defn tap->rgb [ugen tap ulo uhi]
  (q/map-range (get-tap-val ugen tap)
               ulo uhi 0 255))



(defn draw-text [state]
  (q/background 0)
  (q/fill 255)
  (q/text (str 1) 250 250))

;; MAIN


