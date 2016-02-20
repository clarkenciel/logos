(ns parser
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

;; all :: RegExp
(def slide-division-regexp #"\n\* ")
(def title-reg             #"([a-zA-Z0-9$-/:-?\{-~!\"^_`\[\] ]+).+\n")
(def body-reg              #"\n([a-zA-Z0-9$-/:-?\{-~!\"^_`\[\] \n\r]+)")
(def important-reg         #".*\*([a-zA-Z0-9$-/:-?\{-~!\"^_`\[\] \n\r]+)\*")

;; get-title :: String -> String
(defn get-title [s]
  (second (re-find title-reg s)))

;; get-title :: String -> String
(defn get-body [s]
  (s/split (second (re-find body-reg s)) #"\n"))

;; get-important :: String -> String
;; next is used here because we want to be able to check against nil
;; in later code, e.g. don't render special text if it isn't there
(defn get-important [s]
  (map #(s/replace % #"\n" " ") (next (re-find important-reg s))))

;; make-slide :: String -> Slide
(defn make-slide [num s]
  {:id        num
   :title     (get-title s)
   :body      (get-body s)
   :important (get-important s)})

;; slides-fp :: FilePath
(def slides-fp "/home/danny/dev/clojure/logos/resources/slides.org")

;; slides-file :: FileHandle
(def slides-file-handle (io/file slides-fp))

;; slides :: String : slurp :: File -> String
(def slides-file (if (.exists slides-file-handle)
                   (slurp slides-file-handle)
                   ""))

;; slides-contents [String]
;; rest is used here because we always want a seq, even if it's empty
(def slides-contents (rest (s/split slides-file slide-division-regexp)))

;; slides :: [Slide]
(def slides (map (fn [[n s]] (make-slide n s))
                 (zipmap (range (count slides-contents))
                         slides-contents)))
