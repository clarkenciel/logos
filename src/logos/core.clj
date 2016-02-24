;; TODO:
;; Transformations of slide texts can be mapped
;;   over the slides lazy seq

(ns logos.core
  (:use [logos.utils]
        [logos.slides]
        [logos.applet]
        [logos.q]
        [logos.sc])
  (:require [quil.middleware :as m]))



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
  {:draw false
   :listeners-made false})

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

;; speaker-click :: PresState -> PresState
(defn speaker-click [s e]
  (let [nu-index (swap! slide-index (partial mod-inc (count slides)))]
    (do
      (reset-num-atom onset-counter 0)
      (assoc s
             :draw true
             :slide (-> nu-index
                        (get-slide slides)
                        (get :body)
                        (speaker-tb))
             :listeners-made (->> (need-listeners? (sc-on?) nu-index s)
                                  (maybe-make-listeners)
                                  (to-bool))))))

(macroexpand '(-> nu-index
                        (get-slide slides)
                        (get :body)
                        (speaker-tb)))

(macroexpand '(->> (need-listeners? (sc-on?) nu-index s)
                                  (maybe-make-listeners)
                                  (to-bool)))

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
    (let [maybe-slide (get-slide @slide-index slides)
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
