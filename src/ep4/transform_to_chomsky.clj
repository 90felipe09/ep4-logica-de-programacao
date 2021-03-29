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
            if (is-redundant-production? rule)
                acc
                (conj acc rule)
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
    [rule-sub production-set non-terminals]
    (reduce (
        fn [acc rule] (
            if (and (not (= rule-sub rule)) (not (is-unit-production? rule non-terminals)))
                (conj acc (substitute rule-sub rule))
                acc
            )
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
                    (concat acc (get-substitution-set rule production-set non-terminal-set))
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

(defn has-one-of-the-vars-in-word?
    "Given a string and a set of symbols, asserts if there's a occurrence of this string in there"
    [input-chain symbols-set]
    (loop [ char (str (first input-chain))
            rest-chain (drop 1 input-chain)
            has-symbol (contains? symbols-set char)]
        (if (empty? char)
            false
            (do
                (if has-symbol
                    true
                    (recur (str (first rest-chain)) (drop 1 rest-chain) (contains? symbols-set (str (first rest-chain))))
                )
            )
        )
    )
)

(defn create-set-of-potential-variables
    "Given a set of symbols, a set of non terminals and a set of production rules, returns a set of only variables that derives to terminals
    or variables present on the given set"
    [productions-set variables-set non-terminal-set]
    (reduce (
        fn [acc rule] (
            if (has-one-of-the-vars-in-word? (first (vals rule)) variables-set)
                (conj acc (first (keys rule)))
            acc
            ) 
        ) variables-set productions-set
    )   
)

(defn remove-derivations-that-doesnt-generates-given-symbols
    "Given a productions set and a set of symbols, returns another production set without productions that doesn't generates
    derivations that is not mentioned in the given symbols set"
    [productions-set symbols-set non-terminals]
    (reduce (
        fn [acc rule] (
            if (or (has-one-of-the-vars-in-word? (first (vals rule)) symbols-set) (is-only-terminals? (first (vals rule) ) non-terminals) )
                (conj acc rule)
                acc
        )) #{} productions-set
    )   
)

(defn create-S-reachable-variables-set
    "Given a productions set and an initial symbol, returns a set of variables reachable from S"
    [productions-set initial-symbol non-terminal-symbols]
    (reduce (
        fn [acc rule] (
            if (= (first (keys rule)) initial-symbol)
                (do
                    (loop [ char (str (first (vals rule)))
                            rest-chain (drop 1 (first (vals rule)))
                            is-var (contains? non-terminal-symbols char)]
                        (if (empty? char)
                            acc
                            (do
                                (if is-var
                                    (conj acc char)
                                    (recur (str (first rest-chain)) (drop 1 rest-chain) (contains? non-terminal-symbols (str (first rest-chain))))
                                )
                            )
                        )
                    )
                )
                acc
        )) #{} productions-set
    )   
)

(defn remove-unreachable-derivations
    "remove all derivations which variable does not belongs to the reachable set"
    [prod-set reachable-set]
    (reduce (
        fn [acc rule] (
            if (contains? reachable-set (first (keys rule)))
                (conj acc rule)
                acc
        )) #{} prod-set
    )   
)

(defn remove-useless-productions
    "Given a production rules set, a non terminals set and an initial symbol, returns a production rules set free of useless productions"
    [prod-set non-terminals initial]
    (let [
        vars-to-terminals (create-set-of-variables-that-derives-to-terminals prod-set non-terminals)
        potential-vars (create-set-of-potential-variables prod-set vars-to-terminals non-terminals)
        no-useless-derivations (remove-derivations-that-doesnt-generates-given-symbols prod-set potential-vars non-terminals)
        s-reachable-variables (create-S-reachable-variables-set prod-set initial non-terminals)
        no-unreachable-derivations (remove-unreachable-derivations no-useless-derivations s-reachable-variables)
    ]
    no-unreachable-derivations)
)

(defn pre-chomsky
    "given a production set, non terminal symbols set and a initial symbol, returns a production set in pre chomsky state"
    [prod-set non-terminals, initial]
    (let [  null-free (remove-null-productions prod-set)
            unit-free (remove-unit-productions null-free non-terminals)
            useless-free (remove-useless-productions unit-free non-terminals initial)
    ] 
    useless-free)
)

(defn terminal-has-producer?
    "Given a terminal symbol and production rules set, asserts if there's direct derivation"
    [terminal prod-set]
    (loop
        [rule (first prod-set)
         rest-rules (drop 1 prod-set)]
        (if (= nil rule)
            false
            (do
                (if (= (first (vals rule)) terminal)
                    true
                    (recur (first rest-rules) (drop 1 rest-rules))
                )
            )
        )    
    )
)

(defn identify-terminal-symbols
    "Given a production rules set, returns a set of terminal symbols"
    [prod-set non-terminals]
    (reduce (
            fn [acc rule] (
                loop [  char (str (first (first (vals rule))))
                        rest (drop 1 (first (vals rule)))
                        acumulator acc]
                    (if (empty? char)
                        acumulator
                        (if (contains? non-terminals char)
                            (recur (str (first rest)) (drop 1 rest) acumulator)
                            (recur (str (first rest)) (drop 1 rest) (conj acumulator char))
                        )
                    )
            )
        ) #{} prod-set
    )
)