(ns nbt-parser.parsers
  (:require [nbt-parser.helpers :refer [removeListCounter decListCounter getListCounter]]
            [nbt-parser.tags :as tags]
            [nbt-parser.stringutils :refer [constructName]]))

(defn getTagFunc [tagId]
  (case tagId
    0 tags/parseTagEnd
    1 tags/parseTagByte
    2 tags/parseTagShort
    3 tags/parseTagInt
    4 tags/parseTagLong
    5 tags/parseTagFloat
    6 tags/parseTagDouble
    7 tags/parseTagByteArray
    8 tags/parseTagString
    9 tags/parseTagList
    10 tags/parseTagCompound
    11 tags/parseTagIntArray
    12 tags/parseTagLongArray
    (throw (ex-info "Attempted to parse invalid tagId" {:tagId tagId}))))


;; [bytes parentStack tagTree remainingTagCount name]


(defn parseListTagPayloads
  "Parse a child tag of a List Tag"
  [bytes parentStack tagTree counterMap tagId]
  (if (> (getListCounter parentStack counterMap) 0)
    ;; We have more list items to operate on, do so
    (do
      (println)
      (println "===== RUNNING LIST PARSE_TAG FUNC =====")
      (println (str "Running for " tagId))
      (println (str "Before ListCounter " counterMap))
      (println (str "ListParse parentStack " parentStack))
      (println (str "SearchVector " (reverse (into [] parentStack))))
;      (println (str "TagTree " tagTree))
      (println (str "SearchResult " (get-in tagTree (reverse (into [] parentStack)))))
      (println (str "Next ListSize " (count (get-in tagTree (reverse (into [] parentStack))))))
      (let [currentListSize (count (get-in tagTree (reverse (into [] parentStack))))
            ;; If compound tag, add index to parent stack, but don't use in creating
            ;; the compound tag itself.
            ;; NOTE: Does this also need to be done for lists of lists, however unlikely those are?
            newStack (if (= tagId 10) (conj parentStack currentListSize) parentStack)
            tagFuncReturnMap ((getTagFunc tagId) bytes parentStack tagTree counterMap "")
            ;; Leave old stack here as the counter needs to be associated with the list
            ;; and not the underlying compound tag
            decrementedMap (decListCounter parentStack counterMap)]
        (println (str "DEC COUNT " (getListCounter parentStack decrementedMap)))
        (println)
        (-> tagFuncReturnMap
            (assoc :parentStack (if (= tagId 10) newStack (get tagFuncReturnMap :parentStack)))
            (assoc :counterMap decrementedMap))))
    ;; The list's items have ended, perform cleanup and move on
    (do
      (println "===== RUNNING LIST PARSE_TAG FUNC (Cleanup) =====")
      (-> tags/baseReturn
          (assoc :bytes bytes)
          (assoc :tagTree tagTree)
          (assoc :listOverrideId tagId)
          ;; Remove counter tracker for this list
          (assoc :counterMap (removeListCounter parentStack counterMap))
          ;; Remove this item from the parent stack
          (assoc :parentStack (pop parentStack))))))


;; [bytes parentStack tagTree remainingTagCount name]

(defn parseTag ;;TODO GET NAME
  "Parse a child tag of a Compound Tag (or the root compound tag)"
  [bytes parentStack tagTree counterMap]
  (let [tagId (get bytes 0)
        nameMap (constructName bytes)]
    (println)
    (println "===== RUNNING MAIN PARSE_TAG FUNC =====")
    (println (str "Running for " tagId))
    (println (str "ParentStack " parentStack))
      ;(println (str bytes))
    (println)
    ((getTagFunc tagId) (get nameMap :bytes) parentStack tagTree counterMap (get nameMap :name))))


(defn startNextTag
  "Wrapper that determines whether to call the list or nonlist parse tag function"
  ([bytes initialName] ;; First call args
   (startNextTag bytes (conj '() initialName) {initialName {}} {} nil))
  ([bytes parentStack tagTree counterMap previousTagId]
   (if (empty? bytes)
     tagTree
     (let [tagFuncReturnMap (if (>= (getListCounter parentStack counterMap) 0)
                              (parseListTagPayloads
                               bytes
                               parentStack
                               tagTree
                               counterMap
                               previousTagId)
                              (parseTag
                               bytes
                               parentStack
                               tagTree
                               counterMap))]
       (println "----- RETURN OBJECT -----")
       (println "ParentStack: " (get tagFuncReturnMap :parentStack))
       (println "TagTree: " (get tagFuncReturnMap :tagTree))
       (println "CounterMap: " (get tagFuncReturnMap :counterMap))
       (println "ListOverrideID: " (get tagFuncReturnMap :listOverrideId))
;       (println "Byte snippet: " (subvec (get tagFuncReturnMap :bytes) 0 20))
       (println "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
       (println)
       (startNextTag
        (get tagFuncReturnMap :bytes)
        (get tagFuncReturnMap :parentStack)
        (get tagFuncReturnMap :tagTree)
        (get tagFuncReturnMap :counterMap)
        (get tagFuncReturnMap :listOverrideId))))))
