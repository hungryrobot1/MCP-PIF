(ns mcp.tools
  "Base tools for the MCP server with robust parsing"
  (:require [mcp.journal :as journal]
            [mcp.meta :as meta]
            [mcp.lambda :as lambda]
            [mcp.types :as types]
            [mcp.proof :as proof]
            [cljs.reader :refer [read-string]]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn memory-store
  "Store a key-value pair in memory"
  [{:keys [key value]} server-state-atom]
  (swap! server-state-atom assoc-in [:memory key] value)
  (journal/record! {:type :memory-store :key key :value value})
  (str "Stored: " key))

(defn memory-retrieve
  "Retrieve a value from memory"
  [{:keys [key]} server-state-atom]
  (let [value (get-in @server-state-atom [:memory key])]
    (journal/record! {:type :memory-retrieve :key key :found (some? value)})
    (if value
      (str value)
      (str "No value found for key: " key))))

(defn journal-recent
  "Get recent journal entries"
  [{:keys [limit]} server-state-atom]
  (let [entries (journal/recent-entries (or limit 10))]
    (if (empty? entries)
      ""
      (str "Recent journal entries:\n"
           (str/join "\n"
                    (map #(str (:timestamp %) " - " (:type %)
                              (when-let [data (:data %)]
                                (str ": " data)))
                         entries))))))

(defn server-info
  "Get comprehensive server information including all tools and system state"
  [_ server-state-atom]
  (let [state @server-state-atom
        tools (:tools state)
        runtime-tools (filter #(:runtime (second %)) tools)
        built-in-tools (remove #(:runtime (second %)) tools)
        memory-count (count (:memory state))
        tool-list (map (fn [[tool-name tool]]
                        (str "  - " (name tool-name) ": " (:description tool)
                             (when (:runtime tool) " [RUNTIME]")))
                      tools)]
    (str "=== MCP-PIF-CLJS Server Info ===\n"
         "Version: 0.2.0\n"
         "\n"
         "Tools (" (count tools) " total, " (count runtime-tools) " runtime):\n"
         (str/join "\n" tool-list) "\n"
         "\n"
         "Active Namespaces: " (str/join ", " (:namespaces state)) "\n"
         "Features: " (str/join ", " (map name (:capabilities state))) "\n"
         "Memory: " memory-count " items stored\n"
         "Journal entries: active")))

(defn meta-evolve
  "Tool wrapper for meta/evolve functionality"
  [{:keys [type name description code tool-type schema] :as params} server-state-atom]
  (let [result (meta/handle-evolve params server-state-atom)]
    (if (:success result)
      (:message result)
      (str "Evolution failed: " (:message result)))))


(defn lambda-eval-handler
  "Lambda evaluation handler with robust parsing"
  [{:keys [expression max-steps] :as args} _state]
  (try
    ;; Use lambda namespace's own parser for consistency
    (lambda/evaluate-lambda-tool {:expression expression
                                  :max-steps max-steps}
                                 nil)
    (catch js/Error e
      (str "Error: " (.-message e)))))

(defn type-check-handler
  "Type checking handler with robust parsing"
  [{:keys [expression]} _]
  (try
    (let [parsed (lambda/parse-lambda-string expression)]
      ;; Reset type variables and call type inference
      (types/reset-type-vars!)
      (let [[type subst] (types/infer-type parsed)]
        (if type
          (str "Type: " (types/type->string type))
          "Type inference failed: Could not infer type")))
    (catch js/Error e
      (str "Error: " (.-message e)))))

(defn parse-proof-formula
  "Parse a proof formula correctly"
  [f]
  (cond
    ;; Already a keyword
    (keyword? f) f

    ;; String that should be a keyword
    (and (string? f) (not (str/starts-with? f "[")))
    (keyword f)

    ;; String representing a vector formula like "[:implies A B]"
    (and (string? f) (str/starts-with? f "["))
    (let [parsed (read-string f)]
      (if (vector? parsed)
        (walk/postwalk (fn [x]
                        (cond
                          (keyword? x) x
                          (and (symbol? x)
                               (contains? #{:implies :and :or :not} (keyword (name x))))
                          (keyword (name x))
                          (symbol? x) (keyword (name x))
                          :else x))
                      parsed)
        parsed))

    ;; Already a vector
    (vector? f)
    (walk/postwalk (fn [x]
                    (cond
                      (keyword? x) x
                      (and (symbol? x)
                           (contains? #{:implies :and :or :not} (keyword (name x))))
                      (keyword (name x))
                      (symbol? x) (keyword (name x))
                      :else x))
                  f)

    :else f))

(defn prove-handler
  "Proof handler with fixed formula parsing"
  [{:keys [premises goal method]} state]
  (try
    (let [parsed-premises (map parse-proof-formula premises)
          parsed-goal (parse-proof-formula goal)]
      ;; Call the proof system directly
      (proof/prove {:premises parsed-premises
                    :goal parsed-goal
                    :method (or method "auto")}
                   state))
    (catch js/Error e
      (str "Error: " (.-message e)))))

(defn execute-tool-handler
  "Execute any tool by name, including dynamically created tools"
  [{:keys [tool-name arguments]} server-state-atom]
  (let [tool (get-in @server-state-atom [:tools (keyword tool-name)])]
    (if tool
      (try
        ((:handler tool) arguments server-state-atom)
        (catch js/Error e
          (str "Error executing tool: " (.-message e))))
      (str "Tool not found: " tool-name))))

(defn base-tools
  "Return the base set of tools with robust handlers"
  []
  {:memory-store {:description "Store a key-value pair in memory"
                  :schema {:type "object"
                          :properties {:key {:type "string"}
                                      :value {:type "string"}}
                          :required ["key" "value"]}
                  :handler memory-store}

   :memory-retrieve {:description "Retrieve a value from memory"
                    :schema {:type "object"
                            :properties {:key {:type "string"}}
                            :required ["key"]}
                    :handler memory-retrieve}

   :journal-recent {:description "Get recent journal entries"
                   :schema {:type "object"
                           :properties {:limit {:type "number"
                                              :default 10}}}
                   :handler journal-recent}

   :server-info {:description "Get comprehensive server information including all tools, namespaces, and system state"
                 :schema {:type "object"}
                 :handler server-info}

   :meta-evolve {:description "Create new tools at runtime. USE WITH CAUTION: This can modify server capabilities."
                 :schema {:type "object"
                         :properties {:type {:type "string"
                                            :enum ["add-tool"]
                                            :description "Type of evolution (currently only add-tool)"}
                                     :name {:type "string"
                                           :description "Name of the new tool"}
                                     :description {:type "string"
                                                  :description "Description of what the tool does"}
                                     :code {:type "string"
                                           :description "JavaScript arrow function code, e.g. '(args) => args.x + args.y'"}
                                     :tool-type {:type "string"
                                                :enum ["arithmetic" "string" "lambda"]
                                                :description "Type of tool being created"}
                                     :schema {:type "object"
                                             :description "JSON Schema for the tool's parameters"}}
                         :required ["type" "name" "description" "code"]}
                 :handler meta-evolve}

   :lambda-eval {:description "Evaluate lambda calculus expressions with beta reduction"
                 :schema {:type "object"
                         :properties {:expression {:type "string"
                                                  :description "Lambda calculus expression, e.g. \"[[λ x x] 42]\""}
                                     :max-steps {:type "number"
                                               :default 100
                                               :description "Maximum reduction steps"}}
                         :required ["expression"]}
                 :handler lambda-eval-handler}

   :type-check {:description "Perform Hindley-Milner type inference on a lambda expression"
                :schema {:type "object"
                        :properties {:expression {:type "string"
                                                 :description "Expression to type check, e.g. \"[λ x x]\""}}
                        :required ["expression"]}
                :handler type-check-handler}

   :prove {:description "Prove logical formulas using natural deduction, contradiction, or sequent calculus"
           :schema {:type "object"
                   :properties {:premises {:type "array"
                                          :items {:type "string"}
                                          :description "List of premises, e.g. [\"A\", \"[:implies A B]\"]"}
                               :goal {:type "string"
                                     :description "Goal formula to prove, e.g. \"B\""}
                               :method {:type "string"
                                       :enum ["auto" "natural-deduction" "contradiction" "sequent" "resolution"]
                                       :default "auto"
                                       :description "Proof method to use"}}
                   :required ["premises" "goal"]}
           :handler prove-handler}

   :execute-tool {:description "Execute any tool by name, including dynamically created tools. Use this to call tools created with meta-evolve."
                  :schema {:type "object"
                          :properties {:tool-name {:type "string"
                                                  :description "Name of the tool to execute"}
                                      :arguments {:type "object"
                                                 :description "Arguments to pass to the tool"}}
                          :required ["tool-name" "arguments"]}
                  :handler execute-tool-handler}})
