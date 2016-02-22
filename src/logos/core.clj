(ns logos.core
  (:use [logos.slides]
        [logos.q]
        [logos.applet]
        [logos.sc]))

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
(defn atomic-pop-fac [source proxy]
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
               :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  {:draw false})

;; TODO: Figure out how to do updating properly between the two
;; sets of slides
(defn speaker-click [s]
  (update-slide )
  (assoc s :draw true :slide (next-slide)))

(defn speaker-draw [s]
  (draw-slide s)
)

;; Audience
(def audience-tb (text-box-fac))

(defn aud-setup [] {:x 50 :y 50})

(defn aud-click [{x :x y :y :as s}]
  (assoc s
         :x (+ x (randrange -1 1))
         :y (+ y (randrange -1 1))))

(defn aud-draw [s]
  (draw-slide s))

(def speaker (make-viz [700 700] setup1 updte1 draw1))
(def audience (make-viz :fullscreen setup2 updte2 draw2))

;; ============= MAIN

(comment
  (run-viz speaker "hi" :p2d)
  (run-viz audience "hi" :p2d)
  (close-viz speaker)
  (close-viz audience)
  )

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
