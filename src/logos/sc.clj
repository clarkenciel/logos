(ns logos.sc
  (:use overtone.live))

;; ==================== OVERTONE

;; helper funcs
(defn restart-server []
  (when (server-connected?)
    (do
      (kill-server)
      (boot-server))))

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
;;      (poll (impulse:kr 20) (a2k (* 1 det2)) "ja-det: ")
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
(def ana-group        (group "ana"))
(def inputs           (group "inputs"    :head ana-group))
(def analyzers        (group "ana-early" :after inputs))
(def analysis-senders (group "ana-late"  :after analyzers))

(def in-bus  (audio-bus 2 "input"))
(def router  (audio-bus 2 "router"))

(def main-in        (pipe-in [:head inputs] 3 in-bus))
(def main-out       (pipe-out [:after main-in] in-bus 0))
(def onset-detector (ja-det [:head analyzers] in-bus router))
(def onset-sender   (onset-send [:head analysis-senders] router))

;; Listener set up
(defonce onset-counter (atom 0))
(defonce pitch-record (atom []))

(on-event "/onset" (event-adder-fac onset-counter) ::onset-counter)
;;(on-event "/pitch" (event-concat-fac pitch-record) ::pitch-recorder)

(comment
  (restart-server)
  (pp-node-tree)
  (node-free onset-detector)
  (doseq [x (range 41 45)] (node-free x))
  onset-counter
)
