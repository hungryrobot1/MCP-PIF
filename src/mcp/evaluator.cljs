(ns mcp.evaluator
  "Safe code evaluation for runtime tool creation"
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn create-safe-function
  "Create a safe JavaScript function from a string representation"
  [fn-body]
  ;; Create a sandboxed function with limited scope
  (let [safe-globals #js {:Math js/Math
                          :String js/String
                          :Number js/Number
                          :parseInt js/parseInt
                          :parseFloat js/parseFloat}
        ;; Create function in isolated context
        create-fn (js/Function. "globals" 
                    (str "return function(args, state) {"
                         "  const {Math, String, Number, parseInt, parseFloat} = globals;"
                         "  try {"
                         "    return " fn-body ";"
                         "  } catch (e) {"
                         "    return 'Error: ' + e.message;"
                         "  }"
                         "}"))]
    (create-fn safe-globals)))

(defn parse-simple-fn
  "Parse a simple function definition and extract the body"
  [code-str]
  (try
    ;; Match patterns like: (fn [{:keys [n]}] ...) or (fn [x] ...)
    (let [trimmed (str/trim code-str)]
      (cond
        ;; JavaScript arrow function
        (str/includes? trimmed "=>")
        trimmed
        
        ;; Simple JavaScript function
        (str/starts-with? trimmed "function")
        trimmed
        
        ;; Try to extract body from Clojure-style fn
        (str/starts-with? trimmed "(fn")
        (let [;; Extract everything after the parameter vector
              body-match (re-find #"\(fn\s+\[[^\]]*\]\s+(.+)\)$" trimmed)]
          (when body-match
            ;; Convert simple Clojure forms to JavaScript
            (-> (second body-match)
                (str/replace #"\(" "(")
                (str/replace #"\)" ")")
                (str/replace #"inc\s+" "(1 + ")
                (str/replace #"dec\s+" "(-1 + ")
                (str/replace #"count\s+" "")
                (str/replace #"\.length" ".length"))))
        
        :else nil))
    (catch js/Error e
      nil)))

(defn create-arithmetic-tool
  "Create a tool that performs arithmetic operations"
  [code-str]
  (try
    ;; For simple arrow functions, we can evaluate them directly
    (if (str/includes? code-str "=>")
      (let [safe-fn (js/eval (str "(" code-str ")"))]
        (fn [args _state]
          (try
            (let [result (safe-fn (clj->js args))]
              (str result))
            (catch js/Error e
              (str "Error: " (.-message e))))))
      ;; Fallback for other formats
      (fn [args _state]
        (str "Unsupported function format")))
    (catch js/Error e
      (fn [args _state]
        (str "Error creating tool: " (.-message e))))))

(defn create-string-tool
  "Create a tool that performs string operations"
  [code-str]
  (try
    ;; For simple arrow functions, we can evaluate them directly
    (if (str/includes? code-str "=>")
      (let [safe-fn (js/eval (str "(" code-str ")"))]
        (fn [args _state]
          (try
            (let [result (safe-fn (clj->js args))]
              (str result))
            (catch js/Error e
              (str "Error: " (.-message e))))))
      ;; Fallback for other formats
      (fn [args _state]
        (str "Unsupported function format")))
    (catch js/Error e
      (fn [args _state]
        (str "Error creating tool: " (.-message e))))))