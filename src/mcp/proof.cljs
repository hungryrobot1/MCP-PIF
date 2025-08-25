(ns mcp.proof
  "Enhanced proof system with better inference rules and pattern matching"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [cljs.reader :refer [read-string]]))

;; Formula parsing and normalization
(defn normalize-formula
  "Normalize formula representation for consistent matching"
  [f]
  (cond
    ;; Handle string that looks like a vector "[:implies A B]"
    (and (string? f) (str/starts-with? f "["))
    (let [parsed (read-string f)]
      (normalize-formula parsed))

    ;; Simple string proposition
    (string? f)
    (cond
      (str/starts-with? f ":") (keyword (subs f 1))
      :else (keyword f))

    (keyword? f) f

    ;; Vector formula
    (and (vector? f) (>= (count f) 2))
    (let [op (first f)]
      (case op
        :implies (if (= (count f) 3)
                  [:implies (normalize-formula (second f))
                            (normalize-formula (nth f 2))]
                  f)
        :and [:and (normalize-formula (second f))
                   (normalize-formula (nth f 2))]
        :or [:or (normalize-formula (second f))
                 (normalize-formula (nth f 2))]
        :not [:not (normalize-formula (second f))]
        ;; Handle symbols that should be keywords
        (if (symbol? op)
          (normalize-formula (vec (cons (keyword (name op)) (rest f))))
          f)))

    ;; Symbol that should be a keyword
    (symbol? f) (keyword (name f))

    :else f))

(defn parse-formula
  "Parse and normalize a logical formula"
  [f]
  (normalize-formula f))

;; Pattern matching for formulas
(defn match-formula
  "Check if two formulas match, handling variable binding"
  [pattern formula bindings]
  (cond
    ;; Variable binding
    (and (keyword? pattern) (str/starts-with? (name pattern) "?"))
    (if-let [bound (get bindings pattern)]
      (when (= bound formula) bindings)
      (assoc bindings pattern formula))

    ;; Exact match
    (= pattern formula) bindings

    ;; Structural match
    (and (vector? pattern) (vector? formula)
         (= (count pattern) (count formula)))
    (reduce (fn [b [p f]]
              (when b (match-formula p f b)))
            bindings
            (map vector pattern formula))

    :else nil))

;; Enhanced inference rules
(defn apply-modus-ponens
  "Apply modus ponens: From A and A→B, derive B"
  [formulas]
  (let [implications (filter #(and (vector? %)
                                   (= (first %) :implies))
                             formulas)]
    (set
     (for [impl implications
           :let [[_ antecedent consequent] impl]
           :when (contains? formulas antecedent)]
       consequent))))

(defn apply-modus-tollens
  "Apply modus tollens: From ¬B and A→B, derive ¬A"
  [formulas]
  (let [implications (filter #(and (vector? %)
                                   (= (first %) :implies))
                             formulas)
        negations (filter #(and (vector? %)
                               (= (first %) :not))
                         formulas)]
    (set
     (for [impl implications
           neg negations
           :let [[_ antecedent consequent] impl
                 [_ negated] neg]
           :when (= negated consequent)]
       [:not antecedent]))))

(defn apply-and-rules
  "Apply conjunction introduction and elimination"
  [formulas]
  (let [conjunctions (filter #(and (vector? %)
                                   (= (first %) :and))
                             formulas)]
    (set/union
     ;; And elimination
     (set (mapcat (fn [[_ left right]] [left right]) conjunctions))
     ;; And introduction (limit to prevent explosion)
     (set (take 10
                (for [f1 formulas f2 formulas
                      :when (and (not= f1 f2)
                                (not (and (vector? f1) (= (first f1) :and)))
                                (not (and (vector? f2) (= (first f2) :and))))]
                  [:and f1 f2]))))))

(defn apply-or_rules
  "Apply disjunction introduction"
  [formulas]
  ;; Limit or-introduction to prevent formula explosion
  (set (take 5
             (for [f formulas
                   other formulas
                   :when (not= f other)]
               [:or f other]))))

(defn apply-double-negation
  "Apply double negation elimination"
  [formulas]
  (set
   (for [f formulas
         :when (and (vector? f)
                   (= (first f) :not)
                   (vector? (second f))
                   (= (first (second f)) :not))]
     (second (second f)))))

(defn apply-hypothetical-syllogism
  "From A→B and B→C, derive A→C"
  [formulas]
  (let [implications (filter #(and (vector? %)
                                   (= (first %) :implies))
                             formulas)]
    (set
     (for [impl1 implications
           impl2 implications
           :let [[_ a b] impl1
                 [_ b2 c] impl2]
           :when (= b b2)]
       [:implies a c]))))

(defn apply-disjunctive-syllogism
  "From A∨B and ¬A, derive B"
  [formulas]
  (let [disjunctions (filter #(and (vector? %)
                                   (= (first %) :or))
                             formulas)
        negations (filter #(and (vector? %)
                               (= (first %) :not))
                         formulas)]
    (set
     (for [disj disjunctions
           neg negations
           :let [[_ left right] disj
                 [_ negated] neg]]
       (cond
         (= negated left) right
         (= negated right) left
         :else nil)))))

(defn derive-formulas
  "Apply all inference rules to derive new formulas"
  [formulas max-iterations]
  (loop [known (set formulas)
         iteration 0]
    (if (>= iteration max-iterations)
      known
      (let [new-formulas (set/union
                         known
                         (apply-modus-ponens known)
                         (apply-modus-tollens known)
                         (apply-and-rules known)
                         (apply-double-negation known)
                         (apply-hypothetical-syllogism known)
                         (apply-disjunctive-syllogism known))]
        (if (= new-formulas known)
          known
          (recur new-formulas (inc iteration)))))))

;; Natural deduction proof search
(defn prove-natural-deduction
  "Attempt to prove goal using natural deduction"
  [{:keys [premises goal max-depth]
    :or {max-depth 10}}]
  (let [derived (derive-formulas premises max-depth)]
    (if (contains? derived goal)
      {:success true
       :method :natural-deduction
       :derived derived}
      {:success false
       :derived derived})))

;; Proof by contradiction
(defn prove-by-contradiction
  "Attempt proof by assuming negation of goal and deriving contradiction"
  [{:keys [premises goal]}]
  (let [neg-goal [:not goal]
        extended (conj premises neg-goal)
        derived (derive-formulas extended 10)]
    ;; Look for P and ¬P
    (if (some (fn [f]
                (or (and (contains? derived f)
                        (contains? derived [:not f]))
                    (and (vector? f)
                        (= (first f) :not)
                        (contains? derived (second f)))))
              derived)
      {:success true
       :method :contradiction}
      {:success false})))

;; Resolution-based theorem proving
(defn to-cnf
  "Convert formula to Conjunctive Normal Form"
  [formula]
  ;; Simplified CNF conversion
  (cond
    (keyword? formula) #{#{formula}}

    (and (vector? formula) (= (first formula) :not))
    #{#{[:not (second formula)]}}

    (and (vector? formula) (= (first formula) :and))
    (set/union (to-cnf (second formula))
              (to-cnf (nth formula 2)))

    (and (vector? formula) (= (first formula) :or))
    ;; Distribute OR over AND
    (let [left-cnf (to-cnf (second formula))
          right-cnf (to-cnf (nth formula 2))]
      (set (for [l left-cnf r right-cnf]
             (set/union l r))))

    (and (vector? formula) (= (first formula) :implies))
    ;; A→B becomes ¬A∨B
    (to-cnf [:or [:not (second formula)] (nth formula 2)])

    :else #{#{formula}}))

(defn resolve-clauses
  "Apply resolution rule to two clauses"
  [clause1 clause2]
  (for [lit1 clause1
        lit2 clause2
        :when (or (and (vector? lit1)
                      (= (first lit1) :not)
                      (= (second lit1) lit2))
                 (and (vector? lit2)
                      (= (first lit2) :not)
                      (= (second lit2) lit1)))]
    (set/union (disj clause1 lit1) (disj clause2 lit2))))

(defn prove-by-resolution
  "Prove using resolution and refutation"
  [{:keys [premises goal]}]
  (let [;; Convert premises and negated goal to CNF
        premise-cnf (reduce set/union #{} (map to-cnf premises))
        neg-goal-cnf (to-cnf [:not goal])
        initial-clauses (set/union premise-cnf neg-goal-cnf)]
    (loop [clauses initial-clauses
           iterations 0]
      (if (> iterations 100)
        {:success false :reason :timeout}
        (if (contains? clauses #{})  ; Empty clause means contradiction
          {:success true :method :resolution}
          (let [new-clauses (set (for [c1 clauses c2 clauses
                                       :when (not= c1 c2)
                                       resolved (resolve-clauses c1 c2)]
                                   resolved))]
            (if (empty? (set/difference new-clauses clauses))
              {:success false :reason :saturated}
              (recur (set/union clauses new-clauses)
                     (inc iterations)))))))))

;; Main proof interface
(defn prove
  "Main proof tool with multiple strategies"
  [{:keys [premises goal method]
    :or {method "auto"}} _state]
  (let [parsed-premises (map parse-formula premises)
        parsed-goal (parse-formula goal)]
    (case method
      "natural-deduction"
      (let [result (prove-natural-deduction
                   {:premises parsed-premises
                    :goal parsed-goal})]
        (if (:success result)
          "Proof found!"
          (str "Could not prove goal. Derived: "
               (pr-str (:derived result)))))

      "contradiction"
      (let [result (prove-by-contradiction
                   {:premises parsed-premises
                    :goal parsed-goal})]
        (if (:success result)
          "Proof by contradiction succeeded!"
          "No contradiction found"))

      "resolution"
      (let [result (prove-by-resolution
                   {:premises parsed-premises
                    :goal parsed-goal})]
        (if (:success result)
          "Proof by resolution succeeded!"
          (str "Resolution failed: " (:reason result))))

      ;; Auto mode - try all methods
      "auto"
      (let [methods [prove-natural-deduction
                    prove-by-contradiction
                    prove-by-resolution]]
        (if-let [success (first
                         (filter :success
                                (map #(% {:premises parsed-premises
                                         :goal parsed-goal})
                                     methods)))]
          (str "Proof found using " (name (:method success)) "!")
          "Could not prove using any method"))

      "Could not prove goal")))

;; Proof verification for checking user-provided proofs
(defn verify-proof-step
  "Verify a single proof step"
  [step known-formulas]
  (let [{:keys [formula rule justification]} step]
    (case rule
      :premise (contains? (set justification) formula)
      :modus-ponens (let [[p impl] justification]
                     (and (contains? known-formulas p)
                          (contains? known-formulas impl)
                          (= impl [:implies p formula])))
      :and-intro (let [[left right] justification]
                  (and (contains? known-formulas left)
                       (contains? known-formulas right)
                       (= formula [:and left right])))
      :and-elim (let [[conj] justification]
                 (and (contains? known-formulas conj)
                      (or (= formula (second conj))
                          (= formula (nth conj 2)))))
      false)))

(defn verify-proof
  "Verify a complete formal proof"
  [{:keys [premises conclusion steps]}]
  (loop [known (set premises)
         remaining steps]
    (if (empty? remaining)
      (if (contains? known conclusion)
        {:valid true :message "Proof is valid"}
        {:valid false :message "Conclusion not established"})
      (let [step (first remaining)]
        (if (verify-proof-step step known)
          (recur (conj known (:formula step))
                 (rest remaining))
          {:valid false
           :message (str "Invalid step: " (:formula step))})))))

;; Export main function
(defn proof-tool
  "Main entry point for proof checking"
  [params state]
  (prove params state))
