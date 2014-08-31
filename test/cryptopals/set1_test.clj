(ns cryptopals.set1-test
  (:require [clojure.test :refer :all]
            [cryptopals.set1 :refer :all]))

(deftest hex-to-base-64
  (testing "Convert hex to base64"
    (is (= (convert-hex-to-base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"))))

(deftest fixed-xor-test
  (testing "fixed xor"
    (is (= (fixed-xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") "746865206b696420646f6e277420706c6179"))))


(deftest ngram-generate-function
  (testing "ngram is generating a function"
    (is (function? (ngram "resources/english_monograms.txt")))))

(deftest english-monogram-test
  (testing "function to see if a word is english"
    (is (> (english-monogram "Hello") (english-monogram "QWERQ")))))

(deftest english-quadgrams-test
  (testing "let try with english quadram"
    (is (> (english-quadgrams "ATTACK THE EAST WALL OF THE CASTLE AT DAWN") (english-quadgrams "FYYFHP YMJ JFXY BFQQ TK YMJ HFXYQJ FY IFBS")))))

(deftest hex-str-single-byte-xor-cipher-test
  (testing "Single-byte XOR cipher"
    (is (= (hex-str-single-byte-xor-cipher "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") "ola"))))
