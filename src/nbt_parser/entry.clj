(ns nbt-parser.entry
  (:require [nbt-parser.parsers :refer [startNextTag]]
            [nbt-parser.decomp :refer [loadCompressedNbtFile getFileBytes]]
            [nbt-parser.helpers :refer [getTimeStr]]
            [cheshire.core :refer [generate-string]]))

;; "D:\\testplayerdatadecomp.dat"

;; TEST TAG CONTENTS

;; Opening Compound tag bytes: 10 0 0 (id and no name for `teststrnameremoved`)
;;   List Tag: 9 0 7 76 105 115 116 116 97 103 3 0 0 0 3 0 0 0 1 0 0 0 2 0 0 0 3
;;     id                9
;;     name length       0 7
;;     name              76 105 115 116 116 97 103 (Listtag)
;;     payload tag id    3
;;     number of items   0 0 0 3
;;     contained items   0 0 0 1 0 0 0 2 0 0 0 3
;;   ByteList Tag: 7 0 12 66 121 116 101 97 114 114 97 121 116 97 103 0 0 0 15 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
;;     id                7
;;     name length       0 12
;;     name              66 121 116 101 97 114 114 97 121 116 97 103 (Bytearraytag)
;;     number of items   0 0 0 15
;;     contained items   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
;;   Double Tag: 6 0 9 68 111 117 98 108 101 116 97 103 63 213 85 85 85 85 85 85
;;     id                6
;;     name length       0 9
;;     name              68 111 117 98 108 101 116 97 103 (Doubletag)
;;     payload           63 213 85 85 85 85 85 85 (0.333333333333333314829616256247390992939472198486328125)
;;   End tag (Compound tag terminator): 0
(def teststr [10 0 11 67 111 109 112 111 117 110 100 116 97 103 9 0 7 76 105 115 116 116 97 103 3 0 0 0 3 0 0 0 1 0 0 0 2 0 0 0 3 7 0 12 66 121 116 101 97 114 114 97 121 116 97 103 0 0 0 15 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 6 0 9 68 111 117 98 108 101 116 97 103 63 213 85 85 85 85 85 85 0])
(def teststrnameremoved [10 0 0 9 0 7 76 105 115 116 116 97 103 3 0 0 0 3 0 0 0 1 0 0 0 2 0 0 0 3 7 0 12 66 121 116 101 97 114 114 97 121 116 97 103 0 0 0 15 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 6 0 9 68 111 117 98 108 101 116 97 103 63 213 85 85 85 85 85 85 0])

(def testfile (into [] (getFileBytes "D:\\testplayerdatadecomp.dat")))
(def testfilecomp (into [] (loadCompressedNbtFile "D:\\testplayerdata.dat")))
#_(startNextTag teststrnameremoved "main")
#_(startNextTag testfile "main")


#_(defn parseTagBCK [bytes payload parentStack tagTree remainingList]
    (if (empty? bytes)
      (tagTree)
      (let [tagId (get bytes 0)
            nameMap (constructName bytes payload)]
        ((getTagFunc tagId)
         nameMap :bytes
         (> 0 remainingList)
         (if (isParent tagId) (conj parentStack nameMap :name) parentStack)
         tagTree
         (if (not= remainingList 0) (dec remainingList) 0)))))



#_(defn runFile [filename]
    (parseTag (getFileBytes filename) '("main") {"main" {}}))
;; Consider replacing "main" with extension-removed filename

#_(runFile "D:\\testplayerdatadecomp.dat")



#_(getFileBytes "D:\\testplayerdatadecomp.dat")
#_(count (getFileBytes "D:\\testplayerdatadecomp.dat"))


"Hello entry file"

(defn runSample [opts] (startNextTag testfilecomp "main"))

(spit (str "D:\\clojureout"(getTimeStr)".json") (generate-string (runSample {}) {:pretty true}))
