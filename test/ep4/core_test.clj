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

(deftest substitution-procedure
  (testing "substitution-procedure")
  (let
    [unit-rule {"A" "B"}
     rule {"S" "aA"}
     new-rule (ttc/substitute unit-rule rule) ]
    (println "############### substitution-procedure ################")
    (println "unit-rule: \n" unit-rule)
    (println "rule: \n" rule)
    (println "new-rule: \n" new-rule)
  )
)

(deftest remove-unit-productions
  (testing "remove-unit-productions")
  (let
    [raw-input (slurp "resources/unitProductions.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     clean-set (ttc/remove-unit-productions production-set non-terminals-set)]
    (println "############### remove-unit-productions ################")
    (println "unitProductions.json:")
    (println "production-set: \n" production-set)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "clean-set: \n" clean-set)
  )
)

(deftest word-only-terminals-assert
  (testing "word-only-terminals-assert")
  (let
    [raw-input (slurp "resources/unitProductions.json")
     input (ip/parse-input raw-input)
     word "aabbb"
     other-word "aabBb"
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     assertion (ttc/is-only-terminals? word non-terminals-set)]
    (println "############### word-only-terminals-assert ################")
    (println "word: \n" word)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "assertion: \n" assertion)
    (println "other word: \n" other-word)
    (println "assertion: \n" (ttc/is-only-terminals? other-word non-terminals-set))
  )
)

(deftest create-set-of-variables-with-only-terminals
  (testing "create-set-of-variables-with-only-terminals")
  (let
    [raw-input (slurp "resources/unitProductions.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     variables-set (ttc/create-set-of-variables-that-derives-to-terminals production-set non-terminals-set)]
    (println "############### create-set-of-variables-with-only-terminals ################")
    (println "production-set: \n" production-set)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "variables-set: \n" variables-set)
  )
)

(deftest has-terminals-and-variables-assert
  (testing "has-terminals-and-variables-assert")
  (let
    [raw-input (slurp "resources/unitProductions.json")
     input (ip/parse-input raw-input)
     word "ab"
     other-word "aB"
     another-word "AB"
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     assertion1 (ttc/has-terminals-and-variables? word non-terminals-set)
     assertion2 (ttc/has-terminals-and-variables? other-word non-terminals-set)
     assertion3 (ttc/has-terminals-and-variables? another-word non-terminals-set)]
    (println "############### has-terminals-and-variables-assert ################")
    (println "word: \n" word)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "assertion: \n" assertion1)
    (println "other word: \n" other-word)
    (println "assertion: \n" assertion2)
    (println "other word: \n" another-word)
    (println "assertion: \n" assertion3)
  )
)