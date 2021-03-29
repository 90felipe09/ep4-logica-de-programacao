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

(defn is-unit-production?
    "verifies if a rule is a unit production by comparing to non terminal symbols of a grammar"
    [rule non-terminal-set]
    (contains? non-terminal-set (first (vals rule)) )
)

(defn is-redundant-production?
    "verifies if a rule produces the same variable"
    [rule]
    (= (first (keys rule)) (first (vals rule)))
)

(defn remove-redundant-productions
    "given a production rules set, removes all redundant productions"
    [production-set]
    (reduce (
        fn [acc rule] (
            if (not (is-redundant-production? rule))
                (conj acc rule)
            acc
            ) 
        ) #{} production-set
    )    
)

(defn substitute
    "given a unit rule and another rule, returns the rule with the transitive value of the unit rule"
    [unit-rule rule-to-substitute]
    (let [  key-unit (first (keys unit-rule))
            val-unit (first (vals unit-rule))
            key-rule (first (keys rule-to-substitute))
            val-rule (first (vals rule-to-substitute)) 
            new-val  (clojure.string/replace val-rule key-unit val-unit)]

        {key-rule new-val}        
    )    
)

(defn get-substitution-set
    "given a rule and a production set, returns a set of rules upon substitution operation"
    [rule-sub production-set]
    (reduce (
        fn [acc rule] (
            if (not (= rule-sub rule))
                (conj acc (substitute rule-sub rule))
            acc)
        ) #{} production-set
    )   
)

(defn remove-unit-productions
    "Given a production rules set and a non terminals set, returns production rules without unit productions"
    [production-set non-terminal-set]
    (remove-redundant-productions 
        (reduce (
            fn [acc rule] (
                if (is-unit-production? rule non-terminal-set)
                    (concat acc (get-substitution-set rule production-set))
                    (conj acc rule)  
                ) 
            ) #{} production-set
        ) 
    ) 
)    

(defn is-only-terminals?
    "Given a string and a set of non-terminal symbols, asserts if it's made of only terminals"
    [input-chain non-terminals-set]
    (loop [ char (str (first input-chain))
            rest-chain (drop 1 input-chain)]
        (if (empty? char)
            true
            (do 
                (if (contains? non-terminals-set char)
                    false
                    (recur (str (first rest-chain)) (drop 1 rest-chain))
                )
            )
        )
    )
)

(defn create-set-of-variables-that-derives-to-terminals
    "Given a production rules set and a non-terminals set, returns a set of variables that derives to an only terminals symbols"
    [production-set non-terminals-set]
    (reduce (
        fn [acc rule] (
            if (is-only-terminals? (first (vals rule)) non-terminals-set)
                (conj acc (first (keys rule)))
            acc)
        ) #{} production-set
    )
)

(defn has-terminals-and-variables?
    "Given a string and a set of non-terminal symbols, asserts if it's made of both non-terminals and terminals"
    [input-chain non-terminals-set]
    (loop [ char (str (first input-chain))
            rest-chain (drop 1 input-chain)
            has-non-terminal (contains? non-terminals-set char)
            has-terminal (not has-non-terminal)]
        (if (empty? char)
            false
            (do 
                (if (and has-terminal has-non-terminal)
                    true
                    (do
                        (if has-terminal
                            (recur (str (first rest-chain)) (drop 1 rest-chain) (contains? non-terminals-set (str (first rest-chain))) true)
                            (recur (str (first rest-chain)) (drop 1 rest-chain) true (not (contains? non-terminals-set (str (first rest-chain)))))
                        )
                    )
                )
            )
        )
    )
)

(defn create-set-of-variables-that-derives-to-terminals-and-non-terminals
    "Given a production rules set and a non-terminals set, returns a set of variables that derives to a word of terminals and non terminals"
    [production-set non-terminals-set]
    (reduce (
        fn [acc rule] (
            if (has-terminals-and-variables? (first (vals rule)) non-terminals-set)
                (conj acc (first (keys rule)))
            acc)
        ) #{} production-set
    )
)

;; (defn remove-useless-productions
;;     "Given a production rules set and a non terminals set, removes useless productions"
;;     [productions-rules non-terminals-set]

;; )
