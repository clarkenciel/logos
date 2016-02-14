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

(defn tap->rgb [ugen tap ulo uhi]
  (q/map-range (get-tap-val ugen tap)
               ulo uhi 0 255))

(defn draw-fft [state]
  (let [h (q/height)
        w (q/width)]
    ;;(q/background 0)
    (q/fill 0 0 0 50)
    (q/rect 0 0 w h)
    (let [data (fft-map #(q/constrain (* 100 %) 0 (/ h 2)))]
      (vec->hist 0 data w
                 (fn [x]
                   (map #(mod % 255) [x (state :b) (state :g)])))
      ;;(q/fill 255)
      ;;(vec->text 0 data (q/width))
      )))

(defn draw-text [state]
  (q/background 0)
  (q/fill 255)
  (q/text (str 1) 250 250))

;; MAIN

(defn q-setup []
  (do (q/frame-rate 120)
      (q/no-stroke)
      {:g 100 :b 100}))

(defn q-update [state]
  (let [g (state :g)
        b (state :b)
        [g b] (map #(mod (+ % (+ -9 (rand 10))) 256) [g b])]
    (assoc state :g g :b b)))

(q/defsketch q-logos
  :title ""
  :size [500 500]
  :setup q-setup
  :update q-update
  :draw draw-fft
  :renderer :opengl
  :middleware [m/fun-mode])
