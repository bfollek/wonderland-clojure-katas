(ns alphabet-cipher.coder
  "wonderland alphabet-cipher kata."
  (:require [clojure.string :as str]
            [rabbithole.core :as rh]))

(def ^:private index-of-a
  "Numeric value of 'a' char."
  (int \a))

(defn- alpha-to-index
  "Get the alphabetic index of a char c: a = 0, b = 1, c = 2...z = 25"
  [c]
  (- (int c) index-of-a))

(defn- index-to-alpha
  "Get the char from the alphabetic index i: 0 = a, 1 = b, 2 = c...25 = z"
  [i]
  (char (+ i index-of-a)))

(defn- code-char
  "Encode or decode a message char. f should be + to encode, - to decode."
  [f key-char message-char]
  {:pre [(some #(= f %) [+ -])]} ; Enforce either + or -.
  (-> (alpha-to-index message-char)
      (f (alpha-to-index key-char))
      (mod 26)
      index-to-alpha))

(defn- code
  "Encode or decode a message. See code-char for more info."
  [f key-word message]
  (let [coded-chars
        (map #(code-char f %1 %2) (cycle (str/lower-case key-word))
             (str/lower-case message))]
    (str/join coded-chars)))

(defn encode
  "Encode a message."
  [key-word message]
  (code + key-word message))

(defn decode
  "Decode a message encoded by encode."
  [key-word message]
  (code - key-word message))

(defn decipher
  "Find the keyword from an encrypted message and its plaintext."
  [encrypted plaintext]
  (rh/find-cycle (decode plaintext encrypted)))
