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

(defn fft-setup []
  (do (q/frame-rate 120)
      (q/no-stroke)
      {:fft-data '()  :g 100 :b 100 :h (q/height) :w (q/width)}))

(defn fft-update [state]
  (let [g (state :g)
        b (state :b)
        [g b] (map #(+ % (+ -5 (rand 10))) [g b])
        fd (fft-map #(* 100 %))
        m  (apply max fd)]
    (assoc state :fft-data fd :g g :b b :biggest m :h (q/height) :w (q/width))))

(defn fft-draw [state]
  (let [h (state :h)
        w (state :w)
        g (state :g)
        b (state :b)
        m (clojure.string/join ["Max Value: " (str (state :biggest))])
        tx (* w 0.66)
        ty (* h 0.10)]
    (fade-bg 45 w h)
    (q/fill 120)
    (q/rect (- tx 10) (- ty 15) 200 20)
    (q/fill 255)
    (q/text m tx ty)
    (let [data (state :fft-data)]
      (col->hist 0 (/ h 2) w
                 (fn [x]
                   (map #(q/constrain (q/abs %) 0 255) [x g b x]))
                 data))))

(def q-logos (quil.applet/applet
              :title ""
              ;;  :renderer :p3d
              :size [500 500]
              :setup fft-setup
              :update fft-update
              :draw fft-draw
              :middleware [m/fun-mode]
              :features [:no-start]))
