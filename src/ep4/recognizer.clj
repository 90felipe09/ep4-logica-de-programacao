(ns ep4.recognizer (:gen-class))
  
(defn regress-string
  "Returns, if found, the non-terminal that produces the given string,
   obtained by checking the extended set of regression rules. Otherwise,
   returns nil."
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
  "Returns, if found, the possible combinations of regressions of the
   given head and tail substrings. checking the extended set of
   regression rules. Otherwise, returns nil."
  [string-head string-tail regression-rules]
  (let [regressed-head (regress-string string-head regression-rules)
        regressed-tail (regress-string string-tail regression-rules)
        combinations [(when (and regressed-head regressed-tail)
                        (str regressed-head regressed-tail))
                      (when regressed-head
                        (str regressed-head string-tail))
                      (when regressed-tail
                        (str string-head regressed-tail))]
        valid-combinations (filter #(not= nil %) combinations)]
    (when (> 0 (count valid-combinations)) valid-combinations)))

(defn recognize
  "Returns true if the provided string is a production of the
   provided Chomsky-normalized grammar, returns false otherwise."
  ([string grammar]
    (let [recognized-strings #{}
          unrecognized-strings #{}]
      )))