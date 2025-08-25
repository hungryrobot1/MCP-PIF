(
  ns mcp.types
  "Simple Hindley-Milner type inference for lambda calculus"
  (:require [clojure.walk :as walk]
            [cljs.reader :refer [read-string]]))

(defn fresh-type-var
  "Generate a fresh type variable"
  []
  (keyword (str "t" (random-uuid))))

(defn occurs?
  "Check if type variable occurs in type (for occurs check)"
  [tvar type]
  (cond
    (= tvar type) true
    (vector? type) (some #(occurs? tvar %) type)
    :else false))

(defn substitute-type
  "Substitute type variable with type in expression"
  [expr tvar replacement]
  (walk/postwalk
   (fn [x]
     (if (= x tvar)
       replacement
       x))
   expr))

(defn unify
  "Unify two types, returning substitution map or nil"
  [t1 t2]
  (cond
    (= t1 t2) {}

    (keyword? t1)
    (if (occurs? t1 t2)
      nil
      {t1 t2})

    (keyword? t2)
    (if (occurs? t2 t1)
      nil
      {t2 t1})

    (and (vector? t1) (vector? t2)
         (= (first t1) (first t2) '->)
         (= (count t1) (count t2) 3))
    (let [[_ a1 r1] t1
          [_ a2 r2] t2
          s1 (unify a1 a2)]
      (when s1
        (let [r1' (reduce-kv substitute-type r1 s1)
              r2' (reduce-kv substitute-type r2 s1)
              s2 (unify r1' r2')]
          (when s2
            (merge s1 s2)))))

    :else nil))

(defn apply-substitution
  "Apply substitution to type"
  [subst type]
  (reduce-kv substitute-type type subst))

(defn infer-type
  "Infer type of lambda calculus expression"
  ([expr] (infer-type expr {}))
  ([expr env]
   (cond
     ;; Variable
     (symbol? expr)
     (if-let [type (get env expr)]
       [type {}]
       (let [tvar (fresh-type-var)]
         [tvar {}]))

     ;; Abstraction: λx.e
     (and (vector? expr) (= (first expr) 'λ))
     (let [[_ param body] expr
           param-type (fresh-type-var)
           [body-type body-subst] (infer-type body (assoc env param param-type))
           result-type ['-> (apply-substitution body-subst param-type) body-type]]
       [result-type body-subst])

     ;; Application: (f x)
     (and (vector? expr) (= (count expr) 2))
     (let [[func arg] expr
           [func-type func-subst] (infer-type func env)
           env' (reduce-kv (fn [e k v] (assoc e k (apply-substitution func-subst v))) {} env)
           [arg-type arg-subst] (infer-type arg env')
           result-type (fresh-type-var)
           func-type' (apply-substitution arg-subst func-type)
           unified (unify func-type' ['-> arg-type result-type])]
       (if unified
         [(apply-substitution unified result-type)
          (merge func-subst arg-subst unified)]
         [nil nil]))

     :else [nil nil])))

(defn type->string
  "Convert type to readable string"
  [type]
  (cond
    (keyword? type) (name type)
    (and (vector? type) (= (first type) '->))
    (let [[_ arg res] type]
      (str "(" (type->string arg) " → " (type->string res) ")"))
    :else (str type)))

(defn check-type
  "Type check a lambda expression and return human-readable result"
  [expr]
  (let [[type subst] (infer-type expr)]
    (if type
      {:success true
       :type (type->string type)
       :raw-type type}
      {:success false
       :message "Type inference failed"})))

;; Example type signatures for common functions
(def type-signatures
  {'id '(-> :a :a)  ; Identity function
   'const '(-> :a (-> :b :a))  ; Constant function
   'compose '(-> (-> :b :c) (-> (-> :a :b) (-> :a :c)))  ; Function composition
   'apply '(-> (-> :a :b) (-> :a :b))})  ; Function application

(defn validate-tool-type
  "Validate that a tool's implementation matches expected type"
  [code expected-type]
  (try
    (let [parsed (read-string code)
          [inferred _] (infer-type parsed)]
      (if inferred
        (let [unified (unify inferred expected-type)]
          {:valid (some? unified)
           :inferred (type->string inferred)
           :expected (type->string expected-type)})
        {:valid false
         :message "Could not infer type"}))
    (catch js/Error e
      {:valid false
       :message (.-message e)})))
