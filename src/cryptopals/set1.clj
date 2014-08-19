(ns cryptopals.set1
  (:gen-class))

(require '[clojure.data.codec.base64 :as b64])

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

