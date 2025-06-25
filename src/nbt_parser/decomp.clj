(ns nbt-parser.decomp
  (:require [clojure.java.io :refer [input-stream copy] :as io]
            [clj-compress.core :refer [decompress-data]])
  (:import (java.io ByteArrayOutputStream)))




;; Retrieved from https://gist.github.com/mikeananev/b2026b712ecb73012e680805c56af45f
;; Does not work
(defn gunzip
  "decompress data.
    input: gzipped data  which can be opened by io/input-stream.
    output: something which can be copied to by io/copy (e.g. filename ...)."
  [input output & opts]
  (with-open [input (-> input io/input-stream java.util.zip.GZIPInputStream.)]
    (apply io/copy input output opts)))

;; Retrieved from https://stackoverflow.com/questions/26790881/clojure-file-to-byte-array
(defn getFileBytes [path]
  (with-open [in (input-stream path)
              out (ByteArrayOutputStream.)]
    (copy in out)
    (.toByteArray out)))

(defn loadCompressedNbtFile [inputPath]
  (let [stream (ByteArrayOutputStream.)]
    (decompress-data inputPath stream "gz")
    (.toByteArray stream)
    )
  )

;(loadCompressedNbtFile "D:\\testplayerdata.dat")

;(gunzip2 "D:\\testplayerdata.dat" "D:\\testoutput.dat")

