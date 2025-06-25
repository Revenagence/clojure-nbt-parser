(ns nbt-parser.helpers
  (:require [clojure.string :refer [join]])
  (:import (java.nio ByteBuffer)
           (java.time LocalDateTime)))


(defn convertNumber [bytes numType]
  (let [buffer (ByteBuffer/wrap (byte-array bytes))]
    (case numType
      "short" (.getShort buffer)
      "int" (.getInt buffer)
      "long" (.getLong buffer)
      "float" (.getFloat buffer) ; Float and buffer output values seem to have a slight bit of error
      "double" (.getDouble buffer) ; compared to the actual values. Look into re-manual implementation?
      (throw (ex-info "Attempted to duplicate add counterTree key" {:key key})))))


(defn listtostr [l]
  (join l))


(defn getTimeStr []
  (.format (LocalDateTime/now) (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss")))


(defn setListCounter [parentStack counterMap amount]
  (let [key (listtostr parentStack)]
    (if (= (get counterMap key) nil)
      (assoc counterMap key amount)
      (throw (ex-info "Attempted to duplicate add counterTree key" {:key key})))))

(defn removeListCounter [parentStack counterMap]
  (let [key (listtostr parentStack)]
    (if (not= (get counterMap key) nil)
      (dissoc counterMap key)
      (throw (ex-info "Attempted to remove nonexistent counterTree key" {:key key})))))

(defn incListCounter [parentStack counterMap]
  (let [key (listtostr parentStack)]
    (if (not= (get counterMap key) nil)
      (assoc counterMap key (inc (get counterMap key)))
      (throw (ex-info "Attempted to increment nonexistent counterTree key" {:key key})))))

(defn decListCounter [parentStack counterMap]
  (let [key (listtostr parentStack)]
    (if (not= (get counterMap key) nil)
      (assoc counterMap key (dec (get counterMap key)))
      (throw (ex-info "Attempted to decrement nonexistent counterTree key" {:key key})))))

(defn getListCounter [parentStack counterMap]
  (let [key (listtostr parentStack)]
    (if (not= (get counterMap key) nil)
      (get counterMap key)
      -1))) ;; Give -1 so that we both differentate from contained keys and still pass > 0 checks

; (> (getListCounter parentStack counterMap) 0)


(defn checkInList
  "Are we in a list? Specialized check mainly in use for
   compound tags under a list in order to properly do the check
   despite the presence of the list index value in the parent stack"
  [parentStack counterMap requesterId]
  (let [leadingNumber (number? (peek parentStack))
        popLead (and (= requesterId 10) leadingNumber)]
    (println "            RequesterId " requesterId)
    (println "            Peek Stack " (peek parentStack))
    (println "            LeadingNumber " leadingNumber)
    (println "            PopLead " popLead)
    (println "            TrueCheck " (getListCounter (pop parentStack) counterMap))
    (println "            FalseCheck " (getListCounter parentStack counterMap))
    (if popLead
      (> (getListCounter (pop parentStack) counterMap) 0)
      (> (getListCounter parentStack counterMap) 0))))


(defn vectolist [vec]
  (into '() (reverse vec)))


(defn addToTree
  "Inserts a value into the data tag tree. Adds a key/value pair to a map
   if adding as a compound tag child, or conjs to a vector if adding as a list child"
  [key value tagTree parentStack fromList]
  (let [keylist (into [] (reverse parentStack))]
    (println (str "    AddToTree keylist " keylist))
    (println (str "    AddToTree KEY " key " -- VALUE " value))
    (println (str "    AddToTree KEYTYPE " (type key)))
    (println (str "    AddToTree VALTYPE " (type value)))
;    (println (str "    TagTree " tagTree))
    (println (str "    FromList " fromList))
    (if fromList
      (update-in tagTree keylist conj value)
      (update-in tagTree keylist assoc key value))))

;(update-in {:main {}} ["main"] assoc "testname" [])

;(update-in {"main" {"" {"7610511511611697103" [1 2 3]}}} ["main" "" "7610511511611697103"] assoc 66121116101971141149712111697103 [])


;; Minecraft NBT format stores all numbers as big-endian
(defn constructNumber
  "Construct a big-endian whole number from a variable length series of bytes"
  [bytes]
;    (println "-------------")
;    (println (str bytes))
;    (println (peek bytes))
;    (println (pop bytes))
;    (println (count bytes))
;    (println (- (count bytes) 1))
;    (println "-------------")
  (let [shiftAmount (- (count bytes) 1)]
    (if (> (count bytes) 1)
      (+ (bit-shift-left (peek bytes) (* 8 shiftAmount)) (constructNumber (pop bytes)))
      (peek bytes))))


#_(defn isParent
    "Is the given tag a parent capable of having child nodes? (List and Compound tags)"
    [tagId]
    (case tagId
      9 true
      10 true
      11 true
      12 true
      false))

#_(defn isPayload
    "Should the given tag have its children marked as payloads? (List tags)"
    [tagID]
    (case tagID
      9 true
      11 true
      12 true
      false))