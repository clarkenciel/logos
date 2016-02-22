(ns logos.q ^{:doc "Collection of rendering functions for quil"}
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as s]))

;; ==================== QUIL
(defonce fonts (q/available-fonts))

;; ===== GENERAL UTILS
(defn fade-bg
  ([alpha]
   (fade-bg alpha (q/width) (q/height)))
  ([alpha w h]
   (q/fill 0 0 0 alpha)
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
  (s/split s #"\b"))

(defn unwords [l]
  (apply str (flatten l)))

(defn maybe-font [font-name size]
  (if (some #{font-name} fonts)
    (q/text-font font-name size)))

(defn insert-breaks [s]
  (let [l (words s)]
    (flatten (interpose "\n" (partition-all (/ (count l) 2) l)))))

(defn too-long?
  ([s]
   (too-long? s (q/width)))
  ([s width]
   (> (q/text-width s) width)))

(defn string->rows
  ([s]
   (string->rows s (q/width)))
  ([s width]
   (if (too-long? s width)
     (let [v (map (fn [s] (string->rows s width)) (insert-breaks s))]
       (println v)
       (unwords v))
     (do (println "not too long" (q/text-width s)) s))))

(defn text-box [words]
  (q/background 0)
  (q/fill 255)
  (q/text words 250 250))

(comment
  (defn draw1 [s]
    (q/background 0)
    (q/text s 0 500)
    (q/text (str (q/text-width s)) 500 350))

  (defn update1 [s]
    (let [v (unwords (take 5 (repeat "lotsa text right here oh boy!")))]
      (string->rows v 10)))
  
  (q/defsketch quil-test
    :size [700 700]
    :setup (fn [] "text")
    :update update1
    :draw draw1
    :middleware [m/fun-mode])
  
  )
