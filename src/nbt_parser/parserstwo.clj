(ns nbt-parser.parserstwo
  (:import [java.nio ByteBuffer]
           [java.nio.charset Charset]))


(defn parseByteArrayTwo [^ByteBuffer bytes]
  (let [arrLength (.getInt bytes)
        byteArray (byte-array arrLength)]
    (.get bytes byteArray)
    byteArray))

(defn parseIntArrayTwo [^ByteBuffer bytes]
;  (let [arrLength (.getInt bytes)
;        byteArray (byte-array (* 4 arrLength))]
  (loop [length (.getInt bytes)
         acc []]
    (if (pos? length)
      (recur (dec length) (conj acc (.getInt bytes)))
      acc)))

(defn parseLongArrayTwo [^ByteBuffer bytes]
  (loop [length (.getInt bytes)
         acc []]
    (if (pos? length)
      (recur (dec length) (conj acc (.getLong bytes)))
      acc)))


(defn parseStringTwo [^ByteBuffer bytes]
  (let [arrLength (.getShort bytes) ;; UNSIGNED SHORT
        _ (assert (> arrLength 0))
        byteArray (byte-array arrLength)]
    (.get bytes byteArray)
    (String. byteArray (Charset/forName "UTF-8"))))

(declare parseCompound parseListTwo)

(defn callTagFunction [^ByteBuffer bytes tagId]
  (doto
   (case tagId
;     0 tags/parseTagEnd
     1 (.get bytes)
     2 (.getShort bytes)
     3 (.getInt bytes)
     4 (.getLong bytes)
     5 (.getFloat bytes)
     6 (.getDouble bytes)
     7 (parseByteArrayTwo bytes)
     8 (parseStringTwo bytes)
     9 (parseListTwo bytes)
     10 (parseCompound bytes {})
     11 (parseIntArrayTwo bytes)
     12 (parseLongArrayTwo bytes)
     (throw (ex-info "Attempted to parse invalid tagId" {:tagId tagId})))
    prn))

(defn parseListTwo [^ByteBuffer bytes]
  (let [payloadId (.get bytes)]
    (loop [length (.getInt bytes)
           acc []]
      (if (pos? length)
        (recur (dec length) (conj acc (callTagFunction bytes payloadId)))
        acc))))




(defn parseTagTwo [^ByteBuffer bytes tagId]
  (let [_ (assert (not= 0 tagId))
        nameLength (.getShort bytes)
        nameByteArray (byte-array nameLength)
        _ (.get bytes nameByteArray)
        name (String. nameByteArray (Charset/forName "UTF-8"))]

    [name (callTagFunction bytes tagId)]))


(defn parseCompound [^ByteBuffer bytes acc]
  (let [tagId (.get bytes)]
    (if (= 0 tagId) acc
        (recur bytes (conj acc (parseTagTwo bytes tagId)))) ;; Conj -> one k/v pair
    ))

(defn parseRoot [^ByteBuffer bytes]
  (let [tagId (.get bytes)
        nameLength (.getShort bytes)]

    (assert (= 10 tagId))
    (assert (= 0 nameLength))


    {"root" (parseCompound bytes {})}))