(ns logos.core
  (:use [logos.q]
        [logos.applet]))

;; SETUP

;; need to create at least one quil applet before booting overtone
;; for some reason: https://github.com/overtone/overtone/issues/313
;; define and call this function simultaneously
(defn safe-start []
  (do
    (throwaway)
    (require '(overtone [live :as ol]))
    (use 'logos.sc)))

;; UTILS

;; TODO: should move fft-polling out of draw and into update

;; MAIN
(defn setup [] nil)
(defn updte [s] {:x 50 :y 50})
(defn draw [{x :x y :y}]
  (circle x y 10))
(def vis (make-viz [200 200] setup updte draw))
(run-viz vis "hi" :p3d)
