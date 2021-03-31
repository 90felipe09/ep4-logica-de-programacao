(ns ep4.recognizer-test (:gen-class))
(require '[ep4.recognizer :as recognizer])
(require '[clojure.test :refer :all])

(deftest regress-string-test
  (testing "regress-string")
  (let
    [regression-rules '(["aS" "S"] ["a" "S"])
     regressible-string "aS"
     unregressible-string "b"]
    (is (= "S" (recognizer/regress-string regressible-string regression-rules)))
    (is (= nil (recognizer/regress-string unregressible-string regression-rules)))))

(deftest decompose-string-test
  (testing "decompose-string")
  (let [string "abcd"
        decomposition '(["a" "bcd"]
                        ["ab" "cd"]
                        ["abc" "d"])]
    (is (= decomposition (recognizer/decompose-string string)))))

(deftest regress-decompostion-test
  (testing "regress-decomposition")
  (let [head-string "aa"
        tail-string "aS"
        bad-string "b"
        regression-rules '(["aS" "S"]
                           ["aa" "S"])]
    ;TODO: uma parte do algoritmo precisa mudar pra testar as regressões aS, SS e Sa e considerar a possibilidade de ambiguidade (tando aS quanto Sa quanto SS, por exemplo)
    (is (= ["SS" "Sa" "aS"] (recognizer/regress-decomposition head-string tail-string regression-rules)))
    (is (= ["bS"] (recognizer/regress-decomposition bad-string tail-string regression-rules)))
    (is (= ["Sb"] (recognizer/regress-decomposition head-string bad-string regression-rules)))
    (is (= nil (recognizer/regress-decomposition bad-string bad-string regression-rules)))))