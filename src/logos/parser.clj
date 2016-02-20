(ns parser
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

;; slides-fp :: FilePath
(def slides-fp "/home/danny/dev/clojure/logos/resources/slides.org")

;; slides-file :: File :
(def slides-file-handle (io/file slides-fp))

;; slides :: String : slurp :: File -> String
(def slides-file (slurp slides-file-handle))

;; all :: RegExp
(def slide-division-regexp #"\n\* ")
(def id-reg                #"(\d+)")
(def title-reg             #"([a-z]+).+(\d+)")
(def body-reg              #"\n([a-zA-Z0-9$-/:-?\{-~!\"^_`\[\] ]+)")
(def important-reg         #".*\*(.+)\*")

;; slides-contents [String]
(def slides-contents (next (s/split slides-file slide-division-regexp)))

;; get-id :: String -> Integer
(defn read-id [s]
  (re-find id-reg s))
(defn get-id [s]
  (Integer/parseInt (second (read-id s))))

;; get-title :: String -> String
(defn get-title [s]
  (first (re-find title-reg s)))

;; get-title :: String -> String
(defn get-body [s]
  (second (re-find body-reg s)))

;; get-important :: String -> String
(defn get-important [s]
  (second (re-find important-reg s)))

;; make-slide :: String -> Slide
(defn make-slide [s]
  {:id        (get-id s)
   :title     (get-title s)
   :body      (get-body s)
   :important (get-important s)})

;; slides :: [Slide]
(def slides (map make-slide slides-contents))
slides-contents
(read-id (nth slides-contents 8))
slides
