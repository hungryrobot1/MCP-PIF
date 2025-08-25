(ns mcp.proof
  "Simple proof system for formal reasoning"
  (:require [clojure.set :as set]))

;; Natural Deduction System for Propositional Logic

(defn parse-formula
  "Parse a logical formula from string or structured data"
  [f]
  (cond
    (keyword? f) f  ; Atomic proposition
    (string? f) (keyword f)
    (and (vector? f) (= (count f) 3))
    (let [[op left right] f]
      (case op
        :and [:and (parse-formula left) (parse-formula right)]
        :or [:or (parse-formula left) (parse-formula right)]
        :implies [:implies (parse-formula left) (parse-formula right)]
        :not [:not (parse-formula left)]
        f))
    :else f))

(defn apply-rule
  "Apply an inference rule to derive new formulas"
  [rule premises]
  (case rule
    ;; Modus Ponens: From A and A→B, derive B
    :modus-ponens
    (let [[p1 p2] premises]
      (when (and (vector? p2) (= (first p2) :implies))
        (let [[_ antecedent consequent] p2]
          (when (= p1 antecedent)
            consequent))))
    
    ;; And Introduction: From A and B, derive A∧B
    :and-intro
    (let [[p1 p2] premises]
      [:and p1 p2])
    
    ;; And Elimination
    :and-elim-left
    (let [[p] premises]
      (when (and (vector? p) (= (first p) :and))
        (second p)))
    
    :and-elim-right
    (let [[p] premises]
      (when (and (vector? p) (= (first p) :and))
        (nth p 2)))
    
    ;; Or Introduction
    :or-intro-left
    (let [[p other] premises]
      [:or p other])
    
    :or-intro-right
    (let [[other p] premises]
      [:or other p])
    
    ;; Double Negation
    :double-neg-elim
    (let [[p] premises]
      (when (and (vector? p) 
                 (= (first p) :not)
                 (vector? (second p))
                 (= (first (second p)) :not))
        (second (second p))))
    
    nil))

(defn prove-step
  "Attempt to prove a goal using available premises and rules"
  [{:keys [premises goal rules]}]
  (let [all-formulas (atom (set premises))]
    ;; Try each rule with each combination of premises
    (doseq [rule rules]
      (let [premise-count (case rule
                            (:modus-ponens :and-intro 
                             :or-intro-left :or-intro-right) 2
                            1)]
        (doseq [selected-premises (if (= premise-count 1)
                                    (map vector @all-formulas)
                                    (for [p1 @all-formulas
                                          p2 @all-formulas
                                          :when (not= p1 p2)]
                                      [p1 p2]))]
          (when-let [derived (apply-rule rule selected-premises)]
            (swap! all-formulas conj derived)))))
    
    ;; Check if goal was derived
    (if (contains? @all-formulas goal)
      {:success true
       :derived @all-formulas
       :proof-found true}
      {:success false
       :derived @all-formulas
       :message "Could not derive goal from premises"})))

;; Proof by Contradiction
(defn prove-by-contradiction
  "Attempt proof by assuming negation of goal"
  [{:keys [premises goal]}]
  (let [neg-goal [:not goal]
        extended-premises (conj premises neg-goal)
        ;; Look for contradiction (P and ¬P)
        contradictions (for [p extended-premises
                            :when (or (and (vector? p) 
                                         (= (first p) :not)
                                         (contains? (set extended-premises) (second p)))
                                    (contains? (set extended-premises) [:not p]))]
                        [p [:not p]])]
    (if (seq contradictions)
      {:success true
       :method :contradiction
       :contradiction (first contradictions)}
      {:success false
       :message "No contradiction found"})))

;; Sequent Calculus style rules
(defn sequent-prove
  "Prove using sequent calculus rules"
  [{:keys [antecedents consequent]}]
  ;; Simple implementation of sequent calculus
  (cond
    ;; Axiom: if consequent is in antecedents
    (contains? (set antecedents) consequent)
    {:success true :rule :axiom}
    
    ;; If consequent is an implication A→B
    (and (vector? consequent) (= (first consequent) :implies))
    (let [[_ a b] consequent]
      (sequent-prove {:antecedents (conj antecedents a)
                     :consequent b}))
    
    ;; If an antecedent is a conjunction
    (some #(and (vector? %) (= (first %) :and)) antecedents)
    (let [conj (first (filter #(and (vector? %) (= (first %) :and)) antecedents))
          [_ left right] conj
          new-antecedents (-> (set antecedents)
                             (disj conj)
                             (conj left right)
                             vec)]
      (sequent-prove {:antecedents new-antecedents
                     :consequent consequent}))
    
    :else
    {:success false :message "Cannot prove sequent"}))

;; Proof verification
(defn verify-proof
  "Verify a proof is valid"
  [{:keys [premises conclusion steps]}]
  (let [known (atom (set premises))]
    (doseq [{:keys [formula rule from]} steps]
      (let [valid-step? 
            (case rule
              :premise (contains? (set premises) formula)
              :modus-ponens (and (contains? @known (first from))
                               (contains? @known (second from))
                               (= formula (apply-rule :modus-ponens from)))
              :and-intro (and (contains? @known (first from))
                            (contains? @known (second from))
                            (= formula [:and (first from) (second from)]))
              false)]
        (if valid-step?
          (swap! known conj formula)
          (throw (js/Error. (str "Invalid step: " formula))))))
    
    (if (contains? @known conclusion)
      {:valid true :message "Proof is valid"}
      {:valid false :message "Conclusion not established"})))

(defn proof-tool
  "Main proof checking tool"
  [{:keys [premises goal method]} _state]
  (let [parsed-premises (map parse-formula premises)
        parsed-goal (parse-formula goal)]
    (case (or method "auto")
      "natural-deduction"
      (let [result (prove-step {:premises parsed-premises
                                :goal parsed-goal
                                :rules [:modus-ponens :and-intro 
                                       :and-elim-left :and-elim-right]})]
        (if (:success result)
          (str "Proof found! Goal derived from premises.")
          (str "Could not prove goal. Derived: " (pr-str (:derived result)))))
      
      "contradiction"
      (let [result (prove-by-contradiction {:premises parsed-premises
                                           :goal parsed-goal})]
        (if (:success result)
          (str "Proof by contradiction succeeded! Found: " (pr-str (:contradiction result)))
          "No contradiction found"))
      
      "sequent"
      (let [result (sequent-prove {:antecedents parsed-premises
                                   :consequent parsed-goal})]
        (if (:success result)
          (str "Sequent proof found using rule: " (:rule result))
          "Cannot prove sequent"))
      
      ;; Auto mode - try all methods
      (let [results [(prove-step {:premises parsed-premises
                                  :goal parsed-goal
                                  :rules [:modus-ponens :and-intro]})
                    (prove-by-contradiction {:premises parsed-premises
                                            :goal parsed-goal})
                    (sequent-prove {:antecedents parsed-premises
                                   :consequent parsed-goal})]]
        (if-let [success (first (filter :success results))]
          "Proof found!"
          "Could not find proof using any method")))))