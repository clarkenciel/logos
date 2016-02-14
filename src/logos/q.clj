(ns logos.q
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; ==================== QUIL

(defn draw-hist-rect
  ([x-start height width]
   (draw-hist-rect x-start height width 125))
  ([x-start height width color-info]
   (let [top    (- (q/height) height)]
     (apply q/fill color-info)
     (q/rect x-start top width height))))

(defn array->hist
  ([x-start data width]
   (array->hist x-start data width (fn [x] [125])))
  ([x-start data width color-f]
   (let [l (count data)
         w (/ width l)]
     (dotimes [i l]
       (let [d (aget data i)]
         (draw-hist-rect (+ x-start (* w i)) d w (color-f d)))))))

(defn vec->hist
  ([x-start data width]
   (vec->hist x-start data width (fn [x] [125])))
  ([x-start data width color-f]
   (let [l (count data)
         w (/ width l)]
     (doseq [[d i] (zipmap data (range l))]
       (draw-hist-rect (+ x-start (* w i)) d w (color-f d))))))

(defn vec->text [x-start data width]
  (let [l (count data)
        w (/ width l)
        h (q/height)]
    (doseq [[d i] (zipmap data (range l))]
      (q/text (str d) (+ x-start (* w i)) (- h d)))))
