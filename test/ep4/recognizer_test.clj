(ns ep4.recognizer-test (:gen-class))
(require '[ep4.recognizer :as recognizer])
(require '[clojure.test :refer :all])

(deftest regress-string-test
  (testing "regress-string")
  (let
    [production-rules '(["S" "aS"]
                        ["A" "a"]
                        ["S" "a"]
                        ["S" "AS"]
                        ["S" "SA"])
     regressible-string "aaaa"
     unregressible-string "aaSaa"
     unknown-string "aS"]
    (recognizer/initialize-recognized-strings production-rules)
    (is (= #{"S"} (recognizer/regress-string regressible-string)))
    (is (= nil (recognizer/regress-string unregressible-string)))
    (println @recognizer/recognized-strings)
    (println @recognizer/unrecognized-strings)))

(deftest decompose-string-test
  (testing "decompose-string")
  (let [string "abcd"
        decomposition '(["a" "bcd"]
                        ["ab" "cd"]
                        ["abc" "d"])]
    (is (= decomposition (recognizer/decompose-string string)))))

(deftest regress-decomposition-test
  (testing "regress-decomposition")
  (let [head-string "a"
        tail-string "aS"
        bad-string "b"
        production-rules '(["S" "AS"]
                           ["S" "SA"]
                           ["S" "a"]
                           ["A" "a"]
                           ["S" "S"])]
    (recognizer/initialize-recognized-strings production-rules)
    (is (= #{"S"} (recognizer/regress-decomposition head-string tail-string)))
    (is (= nil (recognizer/regress-decomposition bad-string tail-string)))
    (is (= nil (recognizer/regress-decomposition head-string bad-string)))
    (is (= nil (recognizer/regress-decomposition bad-string bad-string)))
    (println @recognizer/recognized-strings)
    (println @recognizer/unrecognized-strings)
    ))