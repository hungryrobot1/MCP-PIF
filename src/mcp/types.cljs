(ns mcp.types
  "Improved Hindley-Milner type inference with readable type variables"
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [mcp.lambda :as lambda]))

;; Global counter for generating readable type variables
(def ^:private type-var-counter (atom 0))

(defn reset-type-vars!
  "Reset type variable counter for fresh inference"
  []
  (reset! type-var-counter 0))

(defn fresh-type-var
  "Generate a fresh, readable type variable (a, b, c, ...)"
  []
  (let [n @type-var-counter
        letters "abcdefghijklmnopqrstuvwxyz"
        var-name (if (< n 26)
                   (str (nth letters n))
                   (str (nth letters (mod n 26)) (inc (quot n 26))))]
    (swap! type-var-counter inc)
    (keyword var-name)))


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

(defn generalize
  "Generalize type by making free type variables polymorphic"
  [type env]
  (let [env-vars (set (mapcat #(filter keyword? (flatten %)) (vals env)))
        free-vars (filter keyword? (flatten type))
        gen-vars (remove env-vars free-vars)]
    (if (empty? gen-vars)
      type
      [:forall gen-vars type])))

(defn instantiate
  "Instantiate a polymorphic type with fresh type variables"
  [type]
  (if (and (vector? type) (= (first type) :forall))
    (let [[_ vars body] type
          subst (zipmap vars (repeatedly (count vars) fresh-type-var))]
      (apply-substitution subst body))
    type))

(defn infer-type
  "Infer type of lambda calculus expression with let-polymorphism"
  ([expr]
   (reset-type-vars!)
   (infer-type expr {}))
  ([expr env]
   (cond
     ;; Variable
     (symbol? expr)
     (if-let [type (get env expr)]
       [(instantiate type) {}]
       (let [tvar (fresh-type-var)]
         [tvar {}]))

     ;; Number literal
     (number? expr)
     [:int {}]

     ;; String literal
     (string? expr)
     [:string {}]

     ;; Abstraction: λx.e or ['λ x body]
     (and (vector? expr) (= (first expr) 'λ) (= (count expr) 3))
     (let [[_ param body] expr
           param-type (fresh-type-var)
           [body-type body-subst] (infer-type body (assoc env param param-type))
           result-type ['-> (apply-substitution body-subst param-type) body-type]]
       [result-type body-subst])

     ;; Let binding: ['let x e1 e2]
     (and (vector? expr) (= (first expr) 'let) (= (count expr) 4))
     (let [[_ var e1 e2] expr
           [t1 s1] (infer-type e1 env)
           env' (reduce-kv (fn [e k v] (assoc e k (apply-substitution s1 v))) {} env)
           gen-type (generalize t1 env')
           [t2 s2] (infer-type e2 (assoc env' var gen-type))]
       [t2 (merge s1 s2)])

     ;; Application: [f x]
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

     ;; Built-in operators
     (and (vector? expr) (contains? #{'+ '- '* '/ '= '< '>} (first expr)))
     (let [op (first expr)
           args (rest expr)]
       (case op
         (+ - * /) (if (every? #(number? %) args)
                    [:int {}]
                    (let [t (fresh-type-var)]
                      [['-> t ['-> t t]] {}]))
         (= < >) [['-> :int ['-> :int :bool]] {}]
         [nil nil]))

     :else [nil nil])))

(defn type->string
  "Convert type to readable string"
  [type]
  (cond
    (keyword? type) (name type)
    (and (vector? type) (= (first type) '->))
    (let [[_ arg res] type
          arg-str (if (and (vector? arg) (= (first arg) '->))
                   (str "(" (type->string arg) ")")
                   (type->string arg))]
      (str arg-str " → " (type->string res)))
    (and (vector? type) (= (first type) :forall))
    (let [[_ vars body] type]
      (str "∀" (clojure.string/join "," (map name vars)) ". " (type->string body)))
    :else (str type)))

(defn check-type
  "Type check a lambda expression and return human-readable result"
  [expr]
  (try
    (reset-type-vars!)
    (let [parsed (lambda/parse-lambda-string expr)
          [type subst] (infer-type parsed)]
      (if type
        {:success true
         :type (type->string type)
         :raw-type type}
        {:success false
         :message "Type inference failed"}))
    (catch js/Error e
      {:success false
       :message (str "Error: " (.-message e))})))

;; Common type signatures for reference
(def type-signatures
  {'id ['-> :a :a]  ; Identity function
   'const ['-> :a ['-> :b :a]]  ; Constant function
   'compose ['-> ['-> :b :c] ['-> ['-> :a :b] ['-> :a :c]]]  ; Function composition
   'flip ['-> ['-> :a ['-> :b :c]] ['-> :b ['-> :a :c]]]  ; Flip arguments
   'apply ['-> ['-> :a :b] ['-> :a :b]]  ; Function application
   'fix ['-> ['-> :a :a] :a]  ; Fixed point (Y combinator)
   'church-zero ['-> ['-> :a :a] ['-> :a :a]]  ; Church zero
   'church-succ ['-> ['-> ['-> :a :a] ['-> :a :a]]
                  ['-> ['-> :a :a] ['-> :a :a]]]})  ; Church successor
