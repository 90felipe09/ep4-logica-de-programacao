(ns ep4.transform_to_chomsky (:gen-class))

(defn remove-null-productions
    "Outputs a set of productions by removing null productions from input set"
    [input-production-set]
    (reduce (
        fn [acc rule] (
            if (not (= (first (vals rule)) "eps"))
                (conj acc rule)
            acc
            ) 
        ) #{} input-production-set
    )
)

;; (conj acc rule)