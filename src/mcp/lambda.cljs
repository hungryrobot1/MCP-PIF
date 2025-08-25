(ns mcp.lambda
  "Pure lambda calculus evaluator for formal reasoning"
  (:require [clojure.walk :as walk]
            [cljs.reader :refer [read-string]]))

(defn free-vars
  "Get free variables in a lambda term"
  [expr]
  (cond
    (symbol? expr) #{expr}
    (and (vector? expr) (= (first expr) 'λ))
    (let [[_ param body] expr]
      (disj (free-vars body) param))
    (vector? expr)
    (apply clojure.set/union (map free-vars expr))
    :else #{}))

(defn substitute
  "Capture-avoiding substitution: [x := s]t"
  [expr var replacement]
  (cond
    ;; Variable
    (symbol? expr)
    (if (= expr var) replacement expr)
    
    ;; Abstraction
    (and (vector? expr) (= (first expr) 'λ))
    (let [[_ param body] expr]
      (cond
        (= param var) expr  ; Shadowing
        (contains? (free-vars replacement) param)
        ;; Alpha conversion needed
        (let [new-param (gensym param)]
          ['λ new-param (substitute (substitute body param new-param) var replacement)])
        :else
        ['λ param (substitute body var replacement)]))
    
    ;; Application
    (vector? expr)
    (mapv #(substitute % var replacement) expr)
    
    :else expr))

(defn beta-reduce
  "Perform one beta reduction step"
  [expr]
  (cond
    ;; Direct beta redex: ((λx.M) N)
    (and (vector? expr)
         (= (count expr) 2)
         (vector? (first expr))
         (= (first (first expr)) 'λ))
    (let [[[_ param body] arg] expr]
      (substitute body param arg))
    
    ;; Try to reduce inside application
    (and (vector? expr) (= (count expr) 2))
    (let [[func arg] expr
          func' (beta-reduce func)]
      (if (not= func func')
        [func' arg]
        [func (beta-reduce arg)]))
    
    ;; Try to reduce inside abstraction
    (and (vector? expr) (= (first expr) 'λ))
    (let [[_ param body] expr
          body' (beta-reduce body)]
      (if (not= body body')
        ['λ param body']
        expr))
    
    :else expr))

(defn normalize
  "Reduce to normal form (if possible)"
  [expr & {:keys [max-steps] :or {max-steps 1000}}]
  (loop [current expr
         steps 0]
    (if (>= steps max-steps)
      {:result current :status :timeout :steps steps}
      (let [next (beta-reduce current)]
        (if (= next current)
          {:result current :status :normal-form :steps steps}
          (recur next (inc steps)))))))

;; Church encodings
(def church-zero ['λ 'f ['λ 'x 'x]])
(def church-succ ['λ 'n ['λ 'f ['λ 'x ['f [['n 'f] 'x]]]]])
(def church-true ['λ 'x ['λ 'y 'x]])
(def church-false ['λ 'x ['λ 'y 'y]])

;; Combinators
(def I ['λ 'x 'x])  ; Identity
(def K ['λ 'x ['λ 'y 'x]])  ; Constant
(def S ['λ 'f ['λ 'g ['λ 'x [['f 'x] ['g 'x]]]]])  ; Substitution

;; Y combinator (for recursion)
(def Y ['λ 'f [['λ 'x ['f ['x 'x]]] 
               ['λ 'x ['f ['x 'x]]]]])

(defn parse-lambda-string
  "Parse a string representation of lambda calculus"
  [s]
  ;; Simple parser for expressions like: "(λx. x x) (λy. y)"
  ;; This is a placeholder - you'd want a proper parser
  (try
    (read-string (-> s
                     (clojure.string/replace #"λ" "λ ")
                     (clojure.string/replace #"\." " ")
                     (clojure.string/replace #"([^()\s]+)" "'$1")))
    (catch js/Error e
      nil)))

(defn evaluate-lambda-tool
  "Evaluate a lambda calculus expression"
  [{:keys [expression max-steps]} _state]
  (if-let [parsed (or (parse-lambda-string expression)
                      (try (read-string expression) (catch js/Error _ nil)))]
    (let [result (normalize parsed :max-steps (or max-steps 100))]
      (str "Result: " (pr-str (:result result)) 
           "\nStatus: " (:status result)
           "\nSteps: " (:steps result)))
    "Error: Invalid lambda expression"))