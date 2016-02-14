(ns logos.sc
  (:require [overtone.live :as ol]))

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
(ol/defsynth sine-tap [output 0 freq 220 amp 0.2 phase 0]
  ;; taps require control rate, but we're fine here
  (let [sig (* amp (ol/sin-osc:ar freq phase))
        _ (ol/tap "lf-sine-tap" 60 (ol/a2k sig))]
    (ol/out output sig)))

(ol/defsynth sine-del-tap [output 0 freq 220 amp 0.2 phase 0 del 0.5 dec 0.2 rate 1]
  (let [env (ol/linen:kr (ol/impulse:kr rate) 0.1 1 0.5)
        sig (* env amp (ol/sin-osc:ar freq phase))
        del (ol/comb-n sig 2 del dec)
        _   (ol/tap "sig" 60 (ol/a2k sig))
        _   (ol/tap "del" 60 (ol/a2k del))
        _   (ol/tap "env" 60 env)
        mix (ol/mix [sig del])]
    (ol/out output mix)))

;; good reference:
;; https://github.com/overtone/shadertone/blob/master/src/shadertone/tone.clj#L81

(comment
  (defsynth bus-freqs->buf
    [in-bus 0 scope-buf 1 fft-buf-size WAVE-BUF-SIZE-2X rate 2]
    (let [phase     (- 1 (* rate (reciprocal fft-buf-size)))
          fft-buf   (local-buf fft-buf-size 1)
          ;; drop DC & nyquist samples
          n-samples (* 0.5 (- (buf-samples:ir fft-buf) 2))
          signal    (in in-bus 1)
          ;; found 0.5 window gave less periodic noise
          freqs     (fft fft-buf signal 0.5 HANN)
          ;; indexer = 2, 4, 6, ..., N-4, N-2
          indexer   (+ n-samples 2
                       (* (lf-saw (/ rate (buf-dur:ir fft-buf)) phase) ;; what are limits to this rate?
                          n-samples))
          indexer   (round indexer 2) ;; always point to the real sample
          ;; convert real,imag pairs to magnitude
          s0        (buf-rd 1 fft-buf indexer 1 1)
          s1        (buf-rd 1 fft-buf (+ 1 indexer) 1 1) ; kibit keep
          lin-mag   (sqrt (+ (* s0 s0) (* s1 s1)))]
      (record-buf lin-mag scope-buf)))
  )

(ol/defsynth ffter [input 0 tgt-buf 1]
  (ol/fft tgt-buf (ol/in:ar input)))

(ol/defsynth mic-in [out-bus 0]
  (ol/out:ar out-bus (ol/sound-in)))

(defonce fft-buf (ol/buffer 512))
(defonce in-bus (ol/audio-bus))
(defonce ana-group (ol/group "ana group"))
(defonce in-sig (mic-in [:head ana-group] in-bus))
(defonce in-fft (ffter [:after in-sig] in-bus fft-buf))

(defn fft-get []
  (ol/buffer-data fft-buf))

(defn fft-map [f]
  (map f (fft-get)))

(defn fft-reduce [op acc]
  (reduce op acc (fft-get)))
