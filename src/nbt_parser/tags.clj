(ns nbt-parser.tags
  (:require [nbt-parser.helpers :refer [addToTree convertNumber setListCounter checkInList]]
            [nbt-parser.stringutils :refer [decode-utf8]]))


(def baseReturn {:bytes []
                 :parentStack '()
                 :tagTree {}
                 :counterMap {}
                 :listOverrideId nil})

(defn constructReturn
  ([bytes parentStack tagTree counterMap overrideId]
   (-> baseReturn
       (assoc :bytes bytes)
       (assoc :parentStack parentStack)
       (assoc :tagTree tagTree)
       (assoc :counterMap counterMap)
       (assoc :listOverrideId overrideId)))
  ([bytes parentStack tagTree]
   (constructReturn
    bytes
    parentStack
    tagTree
    {}
    nil)))


(defn parseTagEnd
  "Consists of only a single zero byte. Marks the end of a Compound Tag."
  [bytes parentStack tagTree counterMap name]
  (constructReturn
   (subvec bytes 1)
   (pop parentStack) ;; As we're exiting the compound tag, remove the current parent from the stack
   tagTree
   counterMap
   10)) ;; Set list tag id to compound, so if we're in a list of compounds the next one gets created

(defn parseTagByte
  "Represents a Signed, 8 bit number value.
   Payload consists of one byte representing the number."
  [bytes parentStack tagTree counterMap name]
  (constructReturn
   (subvec bytes 1) ;; Bytes tag is simple, just the single next byte
   parentStack
   (addToTree name (get bytes 0) tagTree parentStack (checkInList parentStack counterMap 1))
   counterMap
   1))

(defn parseTagShort
  "Represents a Signed, 16 bit number value.
   Payload consists of two bytes representing the number."
  [bytes parentStack tagTree counterMap name]
  (let [value (convertNumber (subvec bytes 0 2) "short")]
    (constructReturn
     (subvec bytes 2)
     parentStack
     (addToTree name value tagTree parentStack (checkInList parentStack counterMap 2))
     counterMap
     2)))

(defn parseTagInt
  "Represents a Signed, 32 bit number value.
   Payload consists of four bytes representing the number."
  [bytes parentStack tagTree counterMap name]
  (let [value (convertNumber (subvec bytes 0 4) "int")]
    (constructReturn
     (subvec bytes 4)
     parentStack
     (addToTree name value tagTree parentStack (checkInList parentStack counterMap 3))
     counterMap
     3)))

(defn parseTagLong
  "Represents a Signed, 64 bit number value.
   Payload consists of eight bytes representing the number."
  [bytes parentStack tagTree counterMap name]
  (let [value (convertNumber (subvec bytes 0 8) "long")]
    (constructReturn
     (subvec bytes 8)
     parentStack
     (addToTree name value tagTree parentStack (checkInList parentStack counterMap 4))
     counterMap
     4)))

(defn parseTagFloat
  "Represents a Signed, single-precision floating point value
   Payload consists of four bytes representing the number."
  [bytes parentStack tagTree counterMap name]
  (let [value (convertNumber (subvec bytes 0 4) "float")]
    (constructReturn
     (subvec bytes 4)
     parentStack
     (addToTree name value tagTree parentStack (checkInList parentStack counterMap 5))
     counterMap
     5)))

(defn parseTagDouble
  "Represents a Signed, double-precision floating point value
   Payload consists of eight bytes representing the number."
  [bytes parentStack tagTree counterMap name]
  (let [value (convertNumber (subvec bytes 0 8) "double")]
    (constructReturn
     (subvec bytes 8)
     parentStack
     (addToTree name value tagTree parentStack (checkInList parentStack counterMap 6))
     counterMap
     6)))

#_(defn parseTagByteArray
    "Represents an Tag containing a list of byte tags
   Payload consists of a 4 byte integer size `x`,
   and is then proceeded by `x` byte tag payloads"
    [bytes parentStack tagTree counterMap name]
    (let [payloadSize (constructNumber (subvec bytes 0 4))]
      (if (= payloadSize 0)
        (startNextTag ;; If the list is empty, move on to the next tag
         (subvec bytes 4)
         parentStack ;; No need to add the list here since it will never see any children
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         counterMap
         7)
        (parseTagByte ;; Trigger a tag byte call
         (subvec bytes 4)
         (conj parentStack name)
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         payloadSize
         ""))))

(defn parseTagByteArray
  "Represents an Tag containing a list of byte tags
   Payload consists of a 4 byte integer size `x`,
   and is then proceeded by `x` byte tag payloads"
  [bytes parentStack tagTree counterMap name]
  (let [payloadSize (convertNumber (subvec bytes 0 4) "int")
        newParentStack (conj parentStack name)]
    (constructReturn
     (subvec bytes 4)
     newParentStack
     (addToTree name (vector) tagTree parentStack (checkInList parentStack counterMap 7))
     (setListCounter newParentStack counterMap payloadSize)
     1)))

(defn parseTagString
  "Represents a Tag containing a series of bytes for a UTF-8 encoded string
   Payload consists of a 2 byte short size `x`
   and then `x` bytes representing the string"
  [bytes parentStack tagTree counterMap name]
  (let [strLength (+ (bit-shift-left (get bytes 0) 8) (get bytes 1))
        value (decode-utf8 (subvec bytes 2 (+ 2 strLength)))]
    (constructReturn
     (subvec bytes (+ 2 strLength))
     parentStack
     (addToTree name value tagTree parentStack (checkInList parentStack counterMap 8))
     counterMap
     8)))

#_(defn parseTagList
    "Represents an Tag containing a list of tags
   Payload consists of single byte representing which type of tag
   the list contains, a 4 byte integer size `x`,
   and is then proceeded by `x` payloads of the indicated type"
    [bytes parentStack tagTree counterMap name]
    (let [payloadSize (constructNumber (subvec bytes 1 5))
          payloadFunc (getTagFunc (get bytes 0))]
      (if (= payloadSize 0)
        (startNextTag ;; If the list is empty, move on to the next tag
         (subvec bytes 5)
         parentStack ;; No need to add the list here since it will never see any children
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         counterMap
         9)
        (payloadFunc
         (subvec bytes 5)
         (conj parentStack name)
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         payloadSize
         ""))))

(defn parseTagList
  "Represents an Tag containing a list of tags
   Payload consists of single byte representing which type of tag
   the list contains, a 4 byte integer size `x`,
   and is then proceeded by `x` payloads of the indicated type"
  [bytes parentStack tagTree counterMap name]
  (let [payloadSize (convertNumber (subvec bytes 1 5) "int")
        payloadId (get bytes 0)
        newParentStack (conj parentStack name)]
;    (println (type payloadSize))
;    (println (str "LIST TAG PAYLOADSIZE " payloadSize))
    (constructReturn
     (subvec bytes 5)
     newParentStack
     (addToTree name [] tagTree parentStack (checkInList parentStack counterMap 9))
     (setListCounter newParentStack counterMap payloadSize)
     payloadId)))

(defn parseTagCompound
  "Represents a compound tag that contains other tags underneath
   Payload consists of an unknown number of assorted other tags
   terminated by a 0 byte (End Tag)"
  [bytes parentStack tagTree counterMap name]
  (println (str "        ParentStack " parentStack))
  (println (str "        CounterMap " counterMap))
  (println (str "        Calculation " (checkInList parentStack counterMap 10)))
  (constructReturn
   bytes ;; No byte removal required, compound tag contains no payload specific to itself (excluding id+name)
   (conj parentStack name)
   (addToTree name '{} tagTree parentStack (checkInList parentStack counterMap 10))
   counterMap
   10))

#_(defn parseTagIntArray
    "Represents an Tag containing a list of int tags
   Payload consists of a 4 byte integer size `x`,
   and is then proceeded by `x` int tag payloads"
    [bytes parentStack tagTree counterMap name]
    (let [payloadSize (constructNumber (subvec bytes 0 4))]
      (if (= payloadSize 0)
        (startNextTag ;; If the list is empty, move on to the next tag
         (subvec bytes 4)
         parentStack ;; No need to add the list here since it will never see any children
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         counterMap
         11)
        (parseTagInt ;; Trigger a tag int call
         (subvec bytes 4)
         (conj parentStack name)
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         payloadSize
         ""))))

(defn parseTagIntArray
  "Represents an Tag containing a list of int tags
   Payload consists of a 4 byte integer size `x`,
   and is then proceeded by `x` int tag payloads"
  [bytes parentStack tagTree counterMap name]
  (let [payloadSize (convertNumber (subvec bytes 0 4) "int")
        newParentStack (conj parentStack name)]
    (constructReturn
     (subvec bytes 4)
     newParentStack
     (addToTree name (vector) tagTree parentStack (checkInList parentStack counterMap 11))
     (setListCounter newParentStack counterMap payloadSize)
     3)))

#_(defn parseTagLongArray
    "Represents an Tag containing a list of long tags
   Payload consists of a 4 byte integer size `x`,
   and is then proceeded by `x` long tag payloads"
    [bytes parentStack tagTree counterMap name]
    (let [payloadSize (constructNumber (subvec bytes 0 4))]
      (if (= payloadSize 0)
        (startNextTag ;; If the list is empty, move on to the next tag
         (subvec bytes 4)
         parentStack ;; No need to add the list here since it will never see any children
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         counterMap
         12)
        (parseTagLong ;; Trigger a tag long call
         (subvec bytes 4)
         (conj parentStack name)
         (addToTree name (vector) tagTree parentStack (not= counterMap 0))
         payloadSize
         ""))))

(defn parseTagLongArray
  "Represents an Tag containing a list of long tags
   Payload consists of a 4 byte integer size `x`,
   and is then proceeded by `x` long tag payloads"
  [bytes parentStack tagTree counterMap name]
  (let [payloadSize (convertNumber (subvec bytes 0 4) "int")
        newParentStack (conj parentStack name)]
    (constructReturn
     (subvec bytes 4)
     newParentStack
     (addToTree name (vector) tagTree parentStack (checkInList parentStack counterMap 12))
     (setListCounter newParentStack counterMap payloadSize)
     4)))

