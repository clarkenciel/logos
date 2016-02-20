(ns scraps
  (:use [logos.q]
        [logos.core])
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; OT stuff
;; (def v (voice-read))
;; (ol/node-free v)
;; 
;; (defonce fft-buf (ol/buffer 512))
;; (defonce in-bus (ol/audio-bus))
;; (defonce ana-group (ol/group "ana group"))
;; (defonce in-sig (mic-in [:head ana-group] in-bus))
;; (defonce in-fft (ffter [:after in-sig] in-bus fft-buf))
;; 
;; (defn fft-get []
;;   (ol/buffer-data fft-buf))
;; 
;; (defn fft-map [f]
;;   (map f (fft-get)))
;; 
;; (defn fft-reduce [op acc]
;;   (reduce op acc (fft-get)))

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
        tx (* w 0.1)
        ty (* h 0.10)
        ts 20
        tl (count m)]
    (fade-bg 45 w h)
    (q/fill 120)
    (q/rect (- tx 10) (- ty ts) (* 12.5 tl) (+ ts 7))
    (q/fill 255)
    (q/text-size ts)
    (q/text m tx ty)
    (let [data (state :fft-data)]
      (col->hist 0 (/ h 2) w
                 (fn [x]
                   (map #(q/constrain (q/abs %) 0 255) [x g b x]))
                 data))))

 
(q/defsketch fft-drawer
  :title ""
  :size [500 500]
  :setup fft-setup
  :update fft-update
  :draw fft-draw
  :middleware [m/fun-mode])
