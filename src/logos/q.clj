(ns logos.q ^{:doc "Collection of rendering functions for quil"}
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as s]))

;; ==================== QUIL
(defonce fonts (q/available-fonts))

;; ===== GENERAL UTILS
(defn fade-bg
  ([alpha]
   (fade-bg [0 0 0] alpha (q/width) (q/height)))
  ([rgb alpha]
   (fade-bg rgb alpha (q/width) (q/height)))
  ([rgb alpha w h]
   (apply q/fill (conj rgb alpha))
   (q/rect 0 0 w h)))

(defn val->rgb [val ulo uhi]
  (q/map-range val ulo uhi 0 255))

(defn circle [x y size]
  (q/fill 0)
  (q/ellipse x y size size))

;; ===== HISTOGRAMS
(defn draw-hist-rect
  ([x-start height width]
   (draw-hist-rect x-start (q/height) height width [125]))
  ([x-start y-start height width color-info]
   (let [top    (- y-start height)]
     (apply q/fill color-info)
     (q/rect x-start top width height))))

(defn col->hist
  ([x-start width data]
   (col->hist x-start (q/height) data width))
  ([x-start y-start width data]
   (col->hist x-start y-start data width (fn [x] [125])))
  ([x-start y-start width color-f data]
   (let [l (count data)
         w (/ width l)]
     (doseq [[d i] (zipmap data (range l))]
       (draw-hist-rect (+ x-start (* w i)) y-start d w (color-f d))))))

(defn vec->hist-label [x-start data width]
  (let [l (count data)
        w (/ width l)
        h (q/height)]
    (doseq [[d i] (zipmap data (range l))]
      (q/text (str d) (+ x-start (* w i)) (- h d)))))

;; ===== TEXT BOXES

(defn words [s]
  (s/split s #"\s"))

(defn unwords [l]
  (s/join " " (flatten l)))

(defn maybe-font [font-name size]
  (when (some #{font-name} fonts)
    (q/text-font (q/create-font font-name size))))

(defn insert-breaks [s]
  (let [l (words s)]
    (unwords (interpose "\n" (partition-all (/ (count l) 2) l)))))

(defn too-long?
  ([s]
   (too-long? s 10))
  ([s width]
   (> (count (words s)) width)))

(defn maybe-lines
  ([s]
   (if (not (too-long? s))
     s
     (insert-breaks s)))
  ([s w]
   (if (not (too-long? s w))
     s
     (insert-breaks s))))

(defn text-height
  "retrieves height of a block of text by multiplying
  the sum of text ascent and descent by the number of 
  newlines and returns
  "
  [s]
  (let [line-count (count (s/split-lines s))]
    (* line-count (+ (q/text-ascent) (q/text-descent)))))

(defn text-box [box-spec]
  (apply q/background (get box-spec :bg [255]))
  (apply q/fill (get box-spec :font-color [0]))
  (q/text (maybe-lines (box-spec :text)
                       (get box-spec :word-limit 10))
          (get box-spec :left-margin 250)
          (get box-spec :top-margin 250)))

(defn text-setup [info]
  (q/text-leading (get info :leading 10))
  (maybe-font (get info :font "Times New Roman")
              (get info :size 15)))

(defn text-box-fac
  "Returns a function that will take text and return a text-box map.
  This factory takes in the following params:
    - :bg
    - :word-limit
    - :top-margin
    - :left-margin
    - :font-color"
  [& params]
  (let [defaults   {:bg [255]
                    :word-limit 10
                    :top-margin #(* (q/height) 0.4)
                    :left-margin #(* (q/height) 0.1)
                    :font-color [0]}
        overwrites (apply hash-map params)
        p (merge defaults overwrites)]
    (fn [t]
      (let [tm (if (fn? (p :top-margin))
                 ((p :top-margin)) (p :top-margin))
            lm (if (fn? (p :left-margin))
                 ((p :left-margin)) (p :left-margin))]
        (merge p {:top-margin tm :left-margin lm :text t})))))

(comment

  (def texts (atom (for [n (range 100)] (str (rand n)))))
  (def text (atom ""))
    
  (defn setup1 []
    (do
      (text-setup  {:leading 10
                    :size 40
                    :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
      (let [f (text-box-fac)]
        {:draw false
         :text-box-f f
         :text-box (f "")})))

  (defn click-handle [s e]
    (assoc s
           :draw (not (s :draw))
           :text-box ((s :text-box-f) (atomic-pop texts text)))
    )
  
  (defn draw1 [s]
    (fade-bg 0)
    (when (s :draw)
        (text-box (s :text-box))))
  
  (q/defsketch quil-test
    :size :fullscreen
    :setup setup1
    :mouse-clicked click-handle
    :draw draw1
    :middleware [m/fun-mode]
    )
  
  )

