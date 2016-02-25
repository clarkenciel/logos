;; TODO:
;; Transformations of slide texts can be mapped
;;   over the slides lazy seq

(ns logos.core
  (:use [logos.utils]
        [logos.slides]
        [logos.applet]
        [logos.q]
        [logos.sc]
        [clojure.pprint])
  (:require [quil.middleware :as m]
            [clojure.string :as s]))

;; Slide Management

;; slide-index :: State Integer
(def slides (get-slides))
(def slide-index (atom -1))

;; Event listeners

(defn inc-counter [c]
  (swap! c (fn [c] (inc c))))

(defn reset-num-atom [c v]
  (swap! c (fn [_] v)))

(def onset-counter (atom 0))

;; ============= Slide Rendering
;; NB: need to have separate functions for each applet

;; draw-slide :: Slide -> Maybe (IO TextBox)
(defn draw-slide [s]
  (let [d (get s :draw false)]
    (when d
      (text-box (s :slide)))))

;; onsets-compare
;; :: [(Integer, Slide)] -> (Integer -> Integer -> Bool) -> Integer -> Integer -> Bool
(defn onsets-compare [slides pred k1 k2]
  (pred ((slides k1) :onsets) ((slides k2) :onsets)))

;; greatest-onsets :: Integer -> [(Integer, Slide)] -> [(Integer, Slides)]
(defn n-greatest-onsets [n slides]
  (let [comp (partial onsets-compare slides >=)]
    (take n (into (sorted-map-by comp) slides))))

;; n-least-onsets :: Integer -> [(Integer, Slide)] -> [(Integer, Slide)]
(defn n-least-onsets [n slides]
  (let [comp (partial onsets-compare slides <=)]
    (take n (into (sorted-map-by comp) slides))))

;; word-freq-compare
;; :: [(String, Double)] -> (Double -> Double -> Bool) -> String -> String -> Bool
(defn word-freq-compare [word-map pred k1 k2]
  (pred (get word-map k1) (get word-map k2)))

;; n-frequent-words :: Integer -> [(String, Double)] -> [(String,Double)]
(defn n-frequent-words [n word-map]
  (let [comp (partial word-freq-compare word-map >=)]
    (take n (into (sorted-map-by comp) (if (not (map? word-map))
                                         (apply hash-map word-map)
                                         word-map)))))

;; n-infrequent-words :: Integer -> [(String, Double)] -> [(String,Double)]
(defn n-infrequent-words [n word-map]
  (let [comp (partial word-freq-compare word-map <=)]
    (take n (into (sorted-map-by comp) word-map))))

;; get-word-maps :: [(Integer, Slide)] -> [[(String,Double)]]
(defn get-word-maps [slides]
  (map (fn [[k v]] (v :percentages)) slides))

;; make-word-swap-map :: [String] -> [String] -> [(String, String)]
(defn make-word-swap-map [old-words new-words]
  (map #(vector (re-pattern  (str "\\b" %1 "\\b")) %2) old-words new-words))

;; apply-swap-map :: String -> [(String, String)] -> String
(defn apply-swap-map [s swap-map]
  (reduce #(apply s/replace %1 %2) s swap-map))

;; get-frequent-words :: Integer -> Slide -> [String]
(defn get-frequent-words [n slide]
  (-> slide (get :percentages) ((partial n-frequent-words n)) (keys)))

;; swap-n-frequent-words
;; :: Integer -> [(String,Double)] -> Slide -> Slide
(defn swap-n-frequent-words [n new-words slide]
  (let [slide-body (slide :body)
        swap-map (make-word-swap-map (get-frequent-words n slide) new-words)]
    (assoc slide :body (map #(apply-swap-map % swap-map) slide-body))))

;; words-n-slides
;;   :: Integer -> (Integer -> [(String,Double)] -> [(String,Double)])
;;      -> [(Integer,Slides)] -> [(String,Double)]
(defn words-n-slides [n word-freq-f slides]
  (let [freq-f (partial word-freq-f n)
        wms    (get-word-maps slides)
        freqs  (map #(apply hash-map %) (mapcat freq-f wms))]
    (reduce assoc-gt {}  freqs)))

(let [slides (get-slides)
      old-slide (slides 5)
      ws       (words-n-slides 10 n-frequent-words (take 10 slides))]
  (println "ws")
  (pprint ws)
  (println "old slide")
  (pprint (old-slide :body))
  (println "new slide")
  (pprint ((swap-n-frequent-words 100 (keys ws) old-slide) :body)))

;; make-listeners? :: SCServerState -> Integer -> PresState -> Bool
(defn need-listeners?
  "Return true if the server is running, we're past title slide, and we haven't
  made listeners yet.
  Return false if the server is off, we've not passed the title slide, or we already
  have listeners running."
  [sc-on? slide-index state]
  (and sc-on? (>= slide-index 1) (not (state :listeners-made))))

(defn maybe-make-listeners [needed?]
  (when needed?
    (make-listener :onset (fn [_] (inc-counter onset-counter)))))

;; Speaker

(def speaker-tb (text-box-fac
                 :font-color [0]
                 :top-margin 100
                 :left-margin 50
                 :word-limit 20))

;; speaker-setup :: PresState
(defn speaker-setup []
  (text-setup {:leading 10
               :size 15
               :font "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  (let [slides (get-slides)
        nuslides (apply merge (map (fn [[k v]] {k (assoc v :onsets 0)}) slides))]
    {:draw false
     :listeners-made false
     :slides nuslides
     :slide-count (count slides)}))

;; speaker-click :: PresState -> PresState
(defn speaker-click [s e]
  (let [slides   (s :slides)
        slide-count (s :slide-count)
        nu-index (swap! slide-index (partial mod-inc slide-count))]
    (do
      (reset-num-atom onset-counter 0)
      (assoc s
             :draw true
             :slide (-> nu-index
                        (slides)
                        (get :body)
                        (speaker-tb))
             :listeners-made (->> (need-listeners? (sc-on?) nu-index s)
                                  (maybe-make-listeners)
                                  (to-bool))))))

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
    (let [maybe-slide (get slides @slide-index nil)
          nuslide     (if (empty? maybe-slide)
                        (get (get s :slide nil) :text "")
                        (if (empty? (maybe-slide :important))
                          (get (get s :slide nil) :text "")
                          (maybe-slide :important)))
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
    (sc-setup 0 0)
    (make-apps)
    (run-app audience "Audience" :p3d)
    (run-app speaker "Speaker" :p3d)
    (event-monitor-on)
    'app-running))

(defn safe-stop []
  (do
    (when (sc-on?)
      (event-monitor-off)
      (sc-stop))
    (close-app speaker)
    (close-app audience)
    (reset-num-atom onset-counter 0)
    (reset-num-atom slide-index -1)
    'app-stopped))

(comment

  (safe-start)

  (safe-stop)

  (event-monitor)

  onset-counter
  
  slides
  )

(println "Ready!")
