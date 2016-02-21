(ns logos.sc
  (:require [overtone.live :as ol])
  (:use overtone.live))

;; ==================== OVERTONE

;; helper funcs
(defn restart-server []
  (when (ol/server-connected?)
    (do
      (ol/kill-server)
      (ol/boot-server))))

(defn get-tap-val [ugen tap-name]
  (deref (get-in ugen [:taps tap-name])))

;; Some Synths
(ol/defsynth sine-tap
  [output 0 freq 220 amp 0.2 phase 0]
  ;; taps require control rate, but we're fine here
  (let [sig (* amp (ol/sin-osc:ar freq phase))
        _ (ol/tap "lf-sine-tap" 60 (ol/a2k sig))]
    (ol/out output sig)))

(ol/defsynth sine-del-tap
  [output 0 freq 220 amp 0.2 phase 0 del 0.5 dec 0.2 rate 1]
  (let [env (ol/linen:kr (ol/impulse:kr rate) 0.1 1 0.5)
        sig (* env amp (ol/sin-osc:ar freq phase))
        del (ol/comb-n sig 2 del dec)
        _   (ol/tap "sig" 60 (ol/a2k sig))
        _   (ol/tap "del" 60 (ol/a2k del))
        _   (ol/tap "env" 60 env)
        mix (ol/mix [sig del])]
    (ol/out output mix)))

(ol/defsynth ffter [input 0 tgt-buf 1]
  (ol/fft tgt-buf (ol/in:ar input)))

(ol/defsynth pipe-in [src 0 out-bus 0]
  (ol/out:ar out-bus (ol/sound-in [src (+ 1 src)])))

(ol/defsynth pipe-out [in-bus 0 tgt 0]
  (ol/out [tgt (+ 1 tgt)] (ol/in in-bus)))

(ol/defsynth ja-det [in-bus 0 out-bus 0]
  (let [sig  (ol/in:ar in-bus)
        buf  (ol/local-buf 512 2)
        ana  (ol/fft buf sig :wintype ol/HANN)
        det1  (< (ol/spec-flatness ana) 0.0098)
        det2  (ol/pv-jensen-andersen ana 0.5 0.8 0.2 0.8 0.15717724 0.025)]
    (ol/out out-bus (* det1 det2))))

(ol/defsynth sine [in-bus 0 out-bus 0]
  (ol/out out-bus (ol/limiter (* (ol/decay (ol/in in-bus) 0.1) (ol/sin-osc 440))
                              0.5)))

;; testing analysis
(comment (ol/pp-node-tree)
         (ol/node-free j)
         (ol/node-pause ana-group)

         (defonce ana-group (ol/group "ana"))
         (defonce ana-early (ol/group "ana-early" :head ana-group))
         (defonce ana-late  (ol/group "ana-late" :after ana-early))

         (def in-bus (ol/audio-bus 2 "input"))
         (def route1 (ol/audio-bus 2 "route1"))
         (def out-bus (ol/audio-bus 2 "output"))

         (def input (pipe [:head ana-early] 3 in-bus))
         (def j (ja-det [:after input] in-bus route1))
         (def snd (sine [:head ana-late] route1 out-bus))
         (def output (pipe-out [:after snd] out-bus)))

;; testing send-reply
(comment
  (defsynth sender [rate 1 lo -1 hi 1]
    (let [t (impulse:kr rate)
          v1 (t-rand:kr lo hi t)
          v2 (t-rand:kr lo hi t)
          v3 (t-rand:kr lo hi t)]
      (send-reply t "/rate" [v1 v2 v3])))

  (node-free s)
  (def s (sender))

  (defn average [vals]
    (/ (reduce + 0 vals) (count vals)))
  
  (defn std-dev [vals]
    (Math/sqrt (average vals)))

  (def a (atom {:name "a"
                :vals [0 0 0]
                :dev 0.0
                }))

  (defn mutate-a [a event]
    (let [nuvals (vec (map + (rest (rest (:args event)))
                           (:vals a)))
          nudev (std-dev nuvals)]
      (assoc a :vals nuvals :dev nudev)))

  (on-event "/rate"
            (fn [event]
              (swap! a mutate-a event))
            ::rate-receiver1)

  (on-event "/print"
            (fn [e] (println a))
            ::rate-printer1)

  (remove-event-handler ::rate-receiver1)
  (remove-event-handler ::rate-printer1)
  )
