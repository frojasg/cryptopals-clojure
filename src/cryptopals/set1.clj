(ns cryptopals.set1
  (:gen-class))

(require '[clojure.data.codec.base64 :as b64])
(use '[clojure.string :only (join split upper-case)])

(defn bytes-to-hex-str
    "Convert a seq of bytes into a hex encoded string."
    [bytes]
    (apply str (for [b bytes] (format "%02x" b))))

(defn hex-str-to-bytes
  "Convert a hex encoded string into a seq of bytes"
  [hex]
  (let [i (BigInteger. hex 16)]
    (.toByteArray i)))

(defn convert-hex-to-base64 [hex]
  "Challenge 1: Convert hex to base64"
  (let [bytes (hex-str-to-bytes hex)]
  (String. (b64/encode bytes))))

(defn fixed-xor [input1 input2]
  "Challenge 2: Fixed XOR"
  (let [a (hex-str-to-bytes input1)
       b (hex-str-to-bytes input2)
       xored (map bit-xor a b)]
    (bytes-to-hex-str xored)))

; based on this
; http://practicalcryptography.com/cryptanalysis/text-characterisation/quadgrams/
; yikes! this is very ugly and i'm pretty sure I still have the imperative mindset but it
; works for now.
(defn ngram [file]
  (let [content (slurp file)
        lines (split content #"\n")
        mem (into {} (map (fn [line] (split line #"\s")) lines))
        [k, _] (first mem)
        l (count k)
        n (reduce + (map (fn [[_, v]] (BigInteger. v)) mem))
        calculated (into {} (for [[k,v] mem] [k, (Math/log10 (/ (BigInteger. v) n))]))
        floor (Math/log10 (/ 0.01 n))]
    (fn [text] (let [text (upper-case text)
                     mon (map (fn [i] (subs text i (+ i l))) (range (- (+ (count text) 1) l)))
                     mon (map (fn [t] (get calculated t floor)) mon)]
                     (reduce + mon)))))

(def english-monogram (ngram "resources/english_monograms.txt"))
(def english-quadgrams (ngram "resources/english_quadgrams.txt"))

