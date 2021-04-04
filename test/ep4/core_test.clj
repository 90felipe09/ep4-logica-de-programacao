(ns ep4.core-test (:gen-class))
(require '[clojure.test :refer :all])
(require '[ep4.input-parser :as ip])
(require '[ep4.transform-to-chomsky :as ttc])

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

(deftest word-has-symbol-assert
  (testing "word-has-symbol-assert")
  (let
    [raw-input (slurp "resources/unitProductions.json")
     input (ip/parse-input raw-input)
     word "aabB"
     other "aabb"
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     assert1 (ttc/has-one-of-the-vars-in-word? word non-terminals-set)
     assert2 (ttc/has-one-of-the-vars-in-word? other non-terminals-set)]
    (println "############### word-has-symbol-assert ################")
    (println "word: \n" word)
    (println "other: \n" other)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "assert1: \n" assert1)
    (println "assert2: \n" assert2)
  )
)


(deftest create-set-of-potential-variables
  (testing "create-set-of-potential-variables")
  (let
    [raw-input (slurp "resources/chomskyExample.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     variables-set (ttc/create-set-of-variables-that-derives-to-terminals production-set non-terminals-set)
     potential-variables (ttc/create-set-of-potential-variables  production-set variables-set non-terminals-set)]
    (println "############### create-set-of-potential-variables ################")
    (println "production-set: \n" production-set)
    (println "variables-set: \n" variables-set)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "potential-variables: \n" potential-variables)
  )
)

(deftest remove-derivations-that-doesnt-generates-given-symbols
  (testing "remove-derivations-that-doesnt-generates-given-symbols")
  (let
    [raw-input (slurp "resources/chomskyExample.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     variables-set (ttc/create-set-of-variables-that-derives-to-terminals production-set non-terminals-set)
     potential-variables (ttc/create-set-of-potential-variables  production-set variables-set non-terminals-set)
     clean-set (ttc/remove-derivations-that-doesnt-generates-given-symbols production-set potential-variables non-terminals-set)]
    (println "############### remove-derivations-that-doesnt-generates-given-symbols ################")
    (println "production-set: \n" production-set)
    (println "potential-variables: \n" potential-variables)
    (println "clean-set: \n" clean-set)
  )
)

(deftest create-S-reachable-variables-set
  (testing "create-S-reachable-variables-set")
  (let
    [raw-input (slurp "resources/chomskyExample.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     initial (ip/get-initial (:grammar input)) 
     reachable-set (ttc/create-S-reachable-variables-set production-set initial non-terminals-set)]
    (println "############### create-S-reachable-variables-set ################")
    (println "production-set: \n" production-set)
    (println "initial: \n" initial)
    (println "reachable-set: \n" reachable-set)
  )
)

(deftest remove-useless-derivations
  (testing "remove-useless-derivations")
  (let
    [raw-input (slurp "resources/chomskyExample.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     initial (ip/get-initial (:grammar input)) 
     pre-chomsky (ttc/pre-chomsky production-set non-terminals-set initial)]
    (println "############### remove-useless-derivations ################")
    (println "production-set: \n" production-set)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "initial: \n" initial)
    (println "pre-chomsky: \n" pre-chomsky)
  )
)

(deftest terminal-has-producer
  (testing "terminal-has-producer")
  (let
    [raw-input (slurp "resources/chomskyExample.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     terminal "a" 
     assert (ttc/terminal-has-producer? terminal production-set)]
    (println "############### terminal-has-producer ################")
    (println "production-set: \n" production-set)
    (println "terminal: \n" terminal)
    (println "assert: \n" assert)
  )
)

(deftest identify-terminal-symbols
  (testing "identify-terminal-symbols")
  (let
    [raw-input (slurp "resources/chomskyExample.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     initial (ip/get-initial (:grammar input)) 
     pre-chomsky (ttc/pre-chomsky production-set non-terminals-set initial)
     terminal-symbols (ttc/identify-terminal-symbols pre-chomsky non-terminals-set)
     ]
    (println "############### identify-terminal-symbols ################")
    (println "production-set: \n" production-set)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "initial: \n" initial)
    (println "pre-chomsky: \n" pre-chomsky)
    (println "terminal symbols: \n" terminal-symbols)
  )
)

(deftest add-terminal-productions
  (testing "add-terminal-productions")
  (let
    [raw-input (slurp "resources/chomsky.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     initial (ip/get-initial (:grammar input)) 
     pre-chomsky (ttc/pre-chomsky production-set non-terminals-set initial)
     terminal-symbols (ttc/identify-terminal-symbols pre-chomsky non-terminals-set)
     with-terminals (ttc/create-terminal-productions pre-chomsky terminal-symbols)
     ]
    (println "############### add-terminal-productions ################")
    (println "production-set: \n" production-set)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "initial: \n" initial)
    (println "pre-chomsky: \n" pre-chomsky)
    (println "terminal symbols: \n" terminal-symbols)
    (println "with terminal: \n" with-terminals)
  )
)

(deftest to-chomsky-form
  (testing "to-chomsky-form")
  (let
    [raw-input (slurp "resources/chomsky.json")
     input (ip/parse-input raw-input)
     production-set (ip/get-productions (:grammar input))
     non-terminals-set (ip/get-non-terminals (:grammar input)) 
     initial (ip/get-initial (:grammar input)) 
     pre-chomsky (ttc/pre-chomsky production-set non-terminals-set initial)
     terminal-symbols (ttc/identify-terminal-symbols pre-chomsky non-terminals-set)
     with-terminals (ttc/create-terminal-productions pre-chomsky terminal-symbols)
     only-variables (ttc/get-set-without-terminals-on-right-side with-terminals non-terminals-set)
     chomsky-form (ttc/minimize-to-size-2 only-variables)
     ]
    (println "############### to-chomsky-form ################")
    (println "production-set: \n" production-set)
    (println "non-terminals-set: \n" non-terminals-set)
    (println "initial: \n" initial)
    (println "pre-chomsky: \n" pre-chomsky)
    (println "terminal symbols: \n" terminal-symbols)
    (println "with terminal: \n" with-terminals)
    (println "only-variables: \n" only-variables)
    (println "chomsky-form: \n" chomsky-form)
  )
)