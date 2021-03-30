(ns ep4.recognizer (:gen-class))
  
(defn regress-string
  "Returns, if found, the non-terminal that produces the given string by
   checking the extended set of regression rules. Otherwise, returns nil."
  [string regression-rules]
  (reduce
   (fn [acc, regression-rule]
      (when
        (= (first regression-rule) string)
        (reduced (second regression-rule))))
   nil
   regression-rules))

(defn decompose-string
  "Generates a list of all possible head/tail divisions of a string"
  [string]
  (for [index (range 1 (count string))]
    [(subs string 0 index) (subs string index)]))

(defn regress-decomposition
  "Returns, if found, the non-terminal that produces the given head/tail
   substrings by checking the extended set of regression rules. Otherwise,
   returns nil."
  [string-head string-tail regression-rules]
  (if-let [regressed-head (regress-string string-head regression-rules)]
    (if-let [regressed-tail (regress-string string-tail regression-rules)]
      (str regressed-head regressed-tail))))

(defn recognize
  "Returns true if the provided string is a production of the
    provided Chomsky-normalized grammar, returns false otherwise."
  ([string grammar]
    (let [recognized-strings #{}
          unrecognized-strings #{}]
      )))