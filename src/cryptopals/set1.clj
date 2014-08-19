(ns cryptopals.set1
  (:gen-class))

(require '[clojure.data.codec.base64 :as b64])

(defn convert-hex-to-base64 [s]
  (let [hex (new java.math.BigInteger  s 16)
        binary-hex (.toByteArray hex)]
  (String. (b64/encode binary-hex))))

