(ns ep4.core-test (:gen-class))
(require '[clojure.test :refer :all])
(require '[ep4.input_parser :as ip])
(require '[ep4.transform_to_chomsky :as ttc])

(deftest parse-input-correctly
  (testing "parse-input-correctly")
  (let
    [raw-input (slurp "resources/1NaoConsecutivo.json")
     input (ip/parse-input raw-input) ]
    (println "############### parse-input-correctly ################")
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
    (println "############### get-production-set ################")
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
    (println "############### remove-null-productions ################")
    (println "emptyProductions.json:")
    (println "production-set: \n" production-set)
    (println "clean-set: \n" clean-set)
    (println "test key access: \n" ((first clean-set) "S"))
  )
)

(deftest unit-production-assertion
  (testing "unit-production-assertion")
  (let
    [raw-input (slurp "resources/unitProductions.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     assertion (ttc/is-unit-production? (first production-set) non-terminals-set)]
    (println "############### unit-production-assertion ################")
    (println "unitProductions.json:")
    (println "production-set: \n" production-set)
    (println "production-rule: \n" (first production-set))
    (println "non-terminal-set: \n" non-terminals-set)
    (println "is unit: \n" assertion)
    (is (= true assertion))
    (println "production-rule: \n" (last production-set))
    (println "is unit: \n" (ttc/is-unit-production? (last production-set) non-terminals-set))
    (is (= false (ttc/is-unit-production? (last production-set) non-terminals-set)))
  )
)

(deftest redundant-production-assertion
  (testing "redundant-production-assertion")
  (let
    [raw-input (slurp "resources/redundantProductions.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input)) 
     assertion (ttc/is-redundant-production? (first production-set))]
    (println "############### redundant-production-assertion ################")
    (println "redundantProductions.json:")
    (println "production-set: \n" production-set)
    (println "production-rule: \n" (first production-set))
    (println "is redundant: \n" assertion)
    (is (= assertion false))
    (println "production-rule: \n" (last production-set))
    (println "is redundant: \n" (ttc/is-redundant-production? (last production-set)))
    (is (= (ttc/is-redundant-production? (last production-set)) true))
  )
)

(deftest remove-redundant-production
  (testing "remove-redundant-production")
  (let
    [raw-input (slurp "resources/redundantProductions.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input)) 
     clean-set (ttc/remove-redundant-productions production-set)]
    (println "############### remove-redundant-production ################")
    (println "redundantProductions.json:")
    (println "production-set: \n" production-set)
    (println "clean-set: \n" clean-set)
  )
)