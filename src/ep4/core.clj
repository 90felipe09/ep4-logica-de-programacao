(ns ep4.core (:gen-class))
(require '[ep4.transform-to-chomsky :as ttc])
(require '[ep4.input-parser :as parser])
(require '[ep4.recognizer :as rec])

(defn -main
  "Given a path of a machine,..."
  [grammar-path]
  (let [grammar (reduce into (ttc/to-chomsky grammar-path))
        input-struct (parser/parse-input (slurp grammar-path))
        word (:word input-struct)]
    (rec/initialize-recognized-strings grammar)
    (if (rec/regress-string (:word input-struct))
      (println (str "A cadeia " word " pertence aa linguagem."))
      (println (str "A cadeia " word " nao pertence aa linguagem.")))))
