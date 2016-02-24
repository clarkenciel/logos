(ns logos.slides ^{:doc "Creation of slides collection using the slides.org file
in logos/resources."}
  (:use [logos.utils])
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

;; all :: RegExp
(def slide-division-regexp #"\n\* ")
(def title-reg             #"([a-zA-Z0-9$-/:-?\{-~!\"^_`\[\] ]+).+\n")
(def body-reg              #"\n([a-zA-Z0-9$-/:-?\{-~!\"^_`\[\] \n\r]+)")
(def important-reg         #"\*([a-zA-Z0-9\s\.,\:;\\/\!\?\"\']+)\*")

;; get-title :: String -> String
(defn get-title [s]
  (second (re-find title-reg s)))

;; get-title :: String -> String
(defn get-body [s]
  (s/split (second (re-find body-reg s)) #"\n"))

;; get-important :: String -> String
(defn get-important [s]
  (map second (re-seq important-reg s)))

(defn only-alpha [s]
  (apply str (map second (re-seq #"(\w+ )" s))))


(defn count-word [word word-list]
  (reduce (fn [acc w]
            (if (= word w)
              (inc acc)
              acc))
          0 word-list))

(defn word-percentages [s]
  (let [word-list  (words (s/lower-case s))
        unique-wl  (set word-list)
        word-count (double (count word-list))]
    (zipmap unique-wl
            (map #(/ (count-word % word-list) word-count) unique-wl))))

;; make-slide :: Integer -> String -> Slide
(defn make-slide [num s]
  (let [body  (get-body s)
        title (get-title s)
        imp   (get-important s)
        bodys (apply str (interpose " " body))]
    {:id        num
     :title     title
     :body      body
     :important imp
     :percentages (word-percentages (only-alpha bodys))}))

;; most-prominent-word :: Slide -> String
(defn most-prominent-word [slide]
  (first (apply sorted-map (slide :percentages))))

(defn get-slide [n col]
  (first (drop n col)))

;; slides-fp :: FilePath
(def slides-fp "/home/danny/dev/clojure/logos/resources/slides.org")

;; slides-file :: FileHandle
(def slides-file-handle (io/file slides-fp))

;; slides :: String : slurp :: File -> String
(defn slides-file []
  (if (.exists slides-file-handle)
    (slurp slides-file-handle)
    ""))

;; slides-contents [String]
;; rest is used here because we always want a seq, even if it's empty
(defn slides-contents []
  (rest (s/split (slides-file) slide-division-regexp)))

;; slides :: [Slide]
(defn get-slides []
  (let [sc (slides-contents)]
    (map (fn [[n s]] (make-slide n s))
         (zipmap (range (count sc))
                 sc))))
