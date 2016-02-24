(ns logos.sc ^{:doc "Supercollider audio analysis utilities"}
  (:use [overtone.api]
        [logos.utils]))

(immigrate-overtone-api)

;; ==================== OVERTONE

;; helper funcs
(defn sc-start []
  (boot-server))

(defn sc-stop []
  (kill-server))

(defn sc-restart []
  (when (server-connected?)
    (do
      (kill-server)
      (boot-server))))

(defn sc-on? []
  (if (= (server-status) :disconnected)
    false
    true))

(defn get-tap-val [ugen tap-name]
  (deref (get-in ugen [:taps tap-name])))

;; Some Synths
(defsynth pipe-in [src 0 out-bus 0]
  (let [input (sound-in [src (+ 1 src)])]
    (do
      ;;(poll (impulse:kr 20) (a2k input) "input: ")
      (out:ar out-bus input))))

(defsynth pipe-out [in-bus 0 tgt 0]
  (out [tgt (+ 1 tgt)] (in in-bus)))

(defsynth ja-det [in-bus 0 out-bus 0]
  (let [sig  (in in-bus)
        buf  (local-buf 512 2)
        ana  (fft buf sig :wintype HANN)
        det1  (< (spec-flatness ana) 0.0098)
        det2  (pv-jensen-andersen ana 0.5 0.8 0.2 0.8 0.15717724 0.025)]
    (do
      ;; (poll (impulse:kr 20) (a2k (* 1 det2)) "ja-det: ")
      (out out-bus (* 1 det2)))))

(defsynth onset-send [in-bus 0]
  (do
    (send-reply (a2k (in in-bus)) "/onset" 1)))

;; event handling

(defn event-vals [evt]
  (drop 2 (:args evt)))

(defn event-concat-fac [atm]
  (fn [e]
    (swap! atm #(concat % (event-vals e)))))

(defn event-adder-fac [atm]
  (fn [e]
    (swap! atm #(+ 1 %))))

;; Analysis set up
;; pipe audio through analysis and to output without modification
(defn sc-setup [input output]
  (defonce analysis (group "analysis"))
  (defonce ana-inputs (group "ana-inputs" :head analysis))
  (defonce ana-early (group "ana-early"  :after ana-inputs))
  (defonce ana-late (group "ana-late"   :after ana-early))

  (defonce in-bus  (audio-bus 2 "input"))
  (defonce router  (audio-bus 2 "router"))

  (def main-in        (pipe-in [:head ana-inputs] input in-bus))
  (def main-out       (pipe-out [:after main-in] in-bus output))
  (def onset-detector (ja-det [:head ana-early] in-bus router))
  (def onset-sender   (onset-send [:head ana-late] router)))

(defn clear-chain [group]
  (group-clear group))

;; Listener set up
;; make-listener :: KeyWord -> EventHandler -> KeyWord
(defn make-listener
  "Takes a keyword and handler function (must accept an event as argument)
  and generates a new event listener. Returns the keyword that can be used
  to refer to the event listener later"
  [name f]
  (do (on-event (str "/" (kw->str name)) f name)
      name))

(comment
  (restart-server)
  (pp-node-tree)
  (node-free onset-detector)
  (doseq [x (range 41 45)] (node-free x))
  onset-counter
)
