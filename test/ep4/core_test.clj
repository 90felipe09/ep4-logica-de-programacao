(ns ep4.core-test (:gen-class))
(require '[clojure.test :refer :all])
(require '[ep4.input_parser :as ip])
(require '[ep4.transform_to_chomsky :as ttc])

(deftest parse-input-correctly
  (testing "parse-input-correctly")
  (let
    [raw-input (slurp "resources/1NaoConsecutivo.json")
     input (ip/parse-input raw-input) ]
    (println "1NaoConsecutivo:")
    (println "raw: \n" raw-input)
    (println "parsed: \n" input)
  )
)

(deftest get-production-set
  (testing "get-production-set")
  (let
    [raw-input (slurp "resources/1NaoConsecutivo.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input)) ]
    (println "1NaoConsecutivo:")
    (println "parsed: \n" input)
    (println "production-set: \n" production-set)
  )
)

(deftest remove-null-productions
  (testing "remove-null-productions")
  (let
    [raw-input (slurp "resources/emptyProductions.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input)) 
     clean-set (ttc/remove-null-productions production-set)]
    (println "emptyProductions.json:")
    (println "production-set: \n" production-set)
    (println "clean-set: \n" clean-set)
    (println "test key access: \n" ((first clean-set) "S"))
  )
)