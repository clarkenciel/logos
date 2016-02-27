
(ns logos.core 
  (:use [logos.utils]
        [logos.slides]
        [logos.applet]
        [logos.q]
        [logos.sc]
        [clojure.pprint])
  (:require [quil.middleware :as m]
            [clojure.string :as s])
  )

;; Slide Management

;; slide-index :: State Integer
(def slides (get-slides))
(def slide-index (atom -1))
(def onset-counter (atom 0))
(println (count slides) "slides loaded")

;; Event listeners

(defn inc-counter [c]
  (swap! c (fn [c] (inc c))))

(defn reset-num-atom [c v]
  (swap! c (fn [_] v)))

;; ============= Slide Rendering
;; NB: need to have separate functions for each applet

;; draw-slide :: Slide -> Maybe (IO TextBox)
(defn draw-slide [s]
  (let [d (get s :draw false)]
    (when d
      (text-box (s :slide)))))

;; onsets-compare
;; :: [(Integer, Slide)] -> (Integer -> Integer -> Bool)
;;    -> Integer -> Integer -> Bool
(defn onsets-compare [slides pred k1 k2]
  (pred ((slides k1) :onsets) ((slides k2) :onsets)))

;; greatest-onsets :: Integer -> [(Integer, Slide)] -> [(Integer, Slides)]
(defn n-greatest-onsets [n slides]
  (let [slides (if (map? slides) slides
                   (apply hash-map (flatten slides)))
        comp (partial onsets-compare slides >=)]
    (take n (into (sorted-map-by comp) slides))))

;; n-least-onsets :: Integer -> [(Integer, Slide)] -> [(Integer, Slide)]
(defn n-least-onsets [n slides]
  (let [comp (partial onsets-compare slides <=)]
    (take n (into (sorted-map-by comp) slides))))

;; word-freq-compare
;; :: [(String, Double)] -> (Double -> Double -> Bool)
;;    -> String -> String -> Bool
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
(defn swap-n-frequent-words
  "Return a new slide with n word replacements in its body"
  [n new-words slide]
  (if (empty? slide) slide
    (let [slide-body (slide :body)
          swap-map (make-word-swap-map (get-frequent-words n slide) new-words)]
      (assoc slide :body (map #(apply-swap-map % swap-map) slide-body)))))

;; words-n-slides
;;   :: Integer -> (Integer -> [(String,Double)] -> [(String,Double)])
;;      -> [(Integer,Slides)] -> [(String,Double)]
(defn words-n-slides
  "Return map of the n most frequent words in a collection of slides"
  [n word-freq-f slides]

  (let [freq-f (partial word-freq-f n)
        wms    (get-word-maps slides)
        freqs  (map #(apply hash-map %) (mapcat freq-f wms))]
    (reduce assoc-gt {} freqs)))

(defn mutate-future-slide
  "Mutate a future slide's body using the onset information
  from previous slides"
  [freq-word-count swap-mul slides current-index target-key]
  (let [slide-count (count slides)
        source-slides (partial take (dec current-index))
        onset-filter  (partial n-greatest-onsets 10)
        word-fetch-f  (partial words-n-slides freq-word-count n-frequent-words)
        swap-f (partial swap-n-frequent-words (* swap-mul current-index))]
    (vector target-key
            (-> slides
                ((comp onset-filter source-slides))
                (word-fetch-f)
                (keys)
                (swap-f (slides target-key))))))

;; make-listeners? :: SCServerState -> Integer -> PresState -> Bool
(defn need-listeners?
  "Return true if the server is running,
  we're past title slide, and we haven't
  made listeners yet. False otherwise."
  [sc-on? slide-index listeners-made?]
  (and sc-on? (>= slide-index 1) (not listeners-made?)))

(defn maybe-make-listeners [pred & args]
  (when (apply pred args)
    (make-listener :onset (fn [_] (inc-counter onset-counter)))
    true))

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
               :font
               "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  (let [nuslides (apply merge
                        (map (fn [[k v]] {k (assoc v :onsets 0)})
                             (get-slides)))
        scount   (count nuslides)]
    (println scount)
    {:draw false
     :listeners-made false
     :slides nuslides
     :slide-count scount
     :slide-index -1
     :mut-lower (int (/ scount 3))
     :mut-upper (int (* 8 (/ scount 10)))}))

;; speaker-click :: PresState -> PresState
(defn speaker-click [s e]
  (let [slindex   (s :slide-index)
        ctr       @onset-counter
        slides    (s :slides)
        slides    (assoc-in slides [slindex :onsets] ctr)
        scount    (s :slide-count)
        nu-index  (swap! slide-index (partial mod-inc scount))
        mut-lower (s :mut-lower)]
    
    (do
      (reset-num-atom onset-counter 0)
      (assoc s
             :draw true
             :slide-index nu-index
             :slides (if (and (<= (s :mut-lower) nu-index)
                              (<= nu-index (s :mut-upper)))
                       (apply assoc slides
                              (mutate-future-slide
                               (- nu-index mut-lower) 5
                               slides nu-index (inc nu-index)))
                       slides)
             :slide (-> nu-index (slides) (get :body) (speaker-tb))
             :listeners-made (to-bool
                              (maybe-make-listeners
                               need-listeners?
                               (sc-on?) nu-index (s :listeners-on)))))))

(defn speaker-draw [s]
  (draw-slide s))

;; Audience
(def audience-tb (text-box-fac))

(defn maybe-new-slide [old-slide slides idx]
  (let [maybe (not-empty (get slides idx nil))]
    (or (not-empty (filter #(< 0 (count %)) (maybe :important)))
        (get old-slide :text '("")))))

(defn aud-setup []
  (text-setup {:leading 10
               :size 30
               :font
               "Hurmit Medium Nerd Font Plus Octicons Plus Pomicons Mono"})
  {:last-slide-index @slide-index
   :draw false
   :bg [240]})

(defn aud-update [s]
  (if (= @slide-index (s :last-slide-index))
    s
    (let [slide   (s :slide)
          nuslide (maybe-new-slide slide slides @slide-index)
          walk-mul (* 0.1 @slide-index)
          bg   (s :bg)
          nubg (map #(constrained-walk 240 255 (* -1 walk-mul) walk-mul %) 
                    bg)]
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
    :draw draw-slide
    :features [:resizable]
    :middleware [m/fun-mode])

  (defapplet speaker
    :size [1000 700]
    :setup speaker-setup
    :draw draw-slide
    :mouse-clicked speaker-click
    ;;:features [:no-safe-fns]
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
    'app-started))

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

  (logos.sc/pp-node-tree)

  (sc-setup 0 0)

  onset-counter

  (pprint slides)
  )

(println "Ready!")
