(ns ep4.core-test (:gen-class))
(require '[clojure.test :refer :all])
(require '[ep4.input_parser :as ip])

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

;; (deftest run-dfa-test
;;   (testing "check successful dfa simulation"
;;   (let [machine (fsmParser/parse-automata "resources/machines/alternate-acceptor.dfa")
;;         tape (slurp "resources/tapes/alternate-tape.tap")]
;;     (is (= true (dfa/run-dfa machine tape))))))