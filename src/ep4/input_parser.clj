(ns ep4.input_parser (:gen-class))
(require '[clojure.data.json :as json])
;; From EP2
(defn parse-input
  "Given a grammar in json format along with a given word W to assert belonging
  property, returns a hash-map with two keys in which the first is the grammar,
  that is a list that the elements are:
  N: A set of non terminal symbols,
  T: A set of terminal symbols,
  P: A set of valid derivation operations,
  S: The element from N that is the language initiator.
  and the word that is the word to assert"
  [raw-input]
  (let
    [ [N T P S W] (vals (json/read-str raw-input)) ]
    {:grammar [ 
      (into #{} N)
      (into #{} T)
      (reduce (
        fn [acc rule] (
          conj acc { (first rule) (last rule) } 
          ) 
        ) #{} P
      )
      S 
    ]
    :word W}
  )
)

(defn get-productions
  "Given a parsed grammar, returns it's production set"
  [parsed-grammar]
  (parsed-grammar 2)
)

