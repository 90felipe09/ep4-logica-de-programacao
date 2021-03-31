(ns ep4.core (:gen-class))
(require '[ep4.transform_to_chomsky :as ttc])

(defn -main
  "Given a path of a machine,..."
  [grammar-path]
  (println (ttc/to-chomsky grammar-path))
)
