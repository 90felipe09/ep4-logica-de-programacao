(ns ep4.recognizer (:gen-class))

; Ref declaration and related functions
(def recognized-strings (ref {}))
(def unrecognized-strings (ref #{}))

(defn add-recognized-string
  "sents the recognized strings ref adding a non-terminal to the
   set of non-terminals associated with the given string"
  [input-string non-terminal-list]
  (dosync
    (alter recognized-strings
      (fn [state]
        (if (state input-string)
          (assoc state input-string (into (state input-string) non-terminal-list))
          (assoc state input-string (into #{} non-terminal-list)))))))

(defn initialize-recognized-strings
  [production-rules]
  (loop [rule (first production-rules)
         other-rules (rest production-rules)]
    (add-recognized-string (second rule) [(first rule)])
    (when (not-empty other-rules)
      (recur (first other-rules) (rest other-rules)))))

(defn add-unrecognized-string
  "sets the unrecognized strings ref adding the input-string to the
   set of known unrecognized string"
  [input-string]
  (dosync
    (alter unrecognized-strings
      (fn [state] (conj state input-string)))))

(defn return-unrecognized
  "same as add-unrecognized-string but returns nil"
  [input-string]
  (add-unrecognized-string input-string)
  nil)

; Other utils
(defn and-print
  "Returns the supplied value after printing it. May accept a message first."
  ([val]
  (println val)
  val)

  ([msg val]
  (println (str msg val))
  val))
  

(defn carthesian-product
  "Returns the carthesian product of two collections as a list."
  [& colls]
  (if (empty? colls)
    '(())
    (for [cart (apply carthesian-product (rest colls))
          item (first colls)]
      (cons item cart))))

(defn decompose-string
  "Generates a list of all possible head/tail divisions of a string"
  [string]
  (for [index (range 1 (count string))]
    [(subs string 0 index) (subs string index)]))

; Regression functions
(declare regress-decomposition)

(defn regress-string
  "Returns, if found, the non-terminals that produce the given string,
   obtained by checking the set of accepted strings. Otherwise, returns
   nil."
  [string]
  (if (<= (count string) 1)
    (@recognized-strings string)
    (when (not (contains? @unrecognized-strings string))
      (if-let [regression (not-empty
                            (reduce into
                              (map #(apply regress-decomposition %)
                                (decompose-string string))))]
        regression
        (return-unrecognized string)))))

(defn regress-decomposition
  "Returns, if found, the possible originating non-terminals of the given
   head and tail substrings while updating the related agents. Otherwise,
   returns nil."
  [string-head string-tail]
  (if-let [regressed-head (regress-string string-head)]
    (if-let [regressed-tail (regress-string string-tail)]
      (if-let [regressed-combinations (not-empty
                                        (filter #(not= nil %)
                                          (map #(@recognized-strings (apply str %))
                                            (carthesian-product regressed-head regressed-tail))))]
        (do
          (add-recognized-string
            (str string-head string-tail)
            (reduce into regressed-combinations))
          (@recognized-strings (str string-head string-tail)))
        nil)
      (return-unrecognized string-tail))
    (return-unrecognized string-head)))