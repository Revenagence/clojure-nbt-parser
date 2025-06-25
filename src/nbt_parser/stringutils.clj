(ns nbt-parser.stringutils
  (:import [java.nio.charset Charset]))


(defn decode-utf8 [byteinput]
  ;; Convert the UTF-8 byte array back to a string
  (String. (byte-array byteinput) (Charset/forName "UTF-8")))

;; This may have gotten a little too if-chainy
(defn constructName [bytes]
  (if (= (get bytes 0) 0) ;; TagEnd consists of only a 0 byte, and therefore has no name
    {:name "" :bytes bytes}
    (let [namelen (+ (bit-shift-left (get bytes 1) 8) (get bytes 2))]
      (print (str "NAME LEN BYTES " (get bytes 1) " " (get bytes 2)))
      (println namelen)
      (println (type (get bytes 2)))
;      (println (str (subvec bytes 0 50)))
;      (println (type bytes))
      ;; If the name string length bytes indicate a string length of 0,
      ;; we have no name, but still need to trim the length bytes
      (if (= namelen 0)
        {:name "" :bytes (subvec bytes 3)}
        {:name (decode-utf8 (subvec bytes 3 (+ 3 namelen)))
         :bytes (subvec bytes (+ 3 namelen))}))))


#_(defn constructNameBCK [bytes payload]
  (if payload ;; Payloads (tags in one of the list tags) have no names
    {:namelen 0 :name "" :bytes bytes}
    (if (= (get bytes 0) 0) ;; TagEnd consists of only a 0 byte, and therefore has no name
      {:namelen 0 :name "" :bytes bytes}
      (let [namelen (+ (bit-shift-left (get bytes 1) 8) (get bytes 2))]
        ;; If the name string length bytes indicate a string length of 0,
        ;; we have no name, but still need to trim the length bytes
        (if (= namelen 0)
          {:namelen 0 :name "" :bytes (subvec bytes 3)}
          {:namelen (+ 2 namelen)
           :name (apply str (subvec bytes 3 (+ 3 namelen))) ;; TODO: Do actual UTF-8 string processing
           :bytes (subvec bytes (+ 3 namelen))})))))
