(ns mcp.meta
  "Meta-programming capabilities for self-modification"
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [mcp.journal :as journal]
            [mcp.evaluator :as evaluator]))

(defn safe-eval
  "Safely evaluate ClojureScript code in a sandboxed context"
  [code-str]
  ;; In a real implementation, this would use a proper sandboxing mechanism
  ;; For now, we'll just parse and validate the code
  (try
    {:success true
     :parsed (reader/read-string code-str)
     :message "Code parsed successfully (evaluation not yet implemented)"}
    (catch js/Error e
      {:success false
       :error (.-message e)})))

(defn handle-inspect
  "Inspect the current state and structure of the server"
  [{:keys [namespace function]} server-state]
  (cond
    namespace
    {:type "namespace"
     :data {:available (contains? (set (:namespaces server-state)) (symbol namespace))
            :namespaces (:namespaces server-state)}}
    
    function
    {:type "function"
     :data {:message "Function inspection not yet implemented"}}
    
    :else
    {:type "overview"
     :data {:namespaces (:namespaces server-state)
            :tools (keys (:tools server-state))
            :capabilities (:capabilities server-state)}}))

(defn validate-evolution
  "Validate that a proposed evolution is safe"
  [evolution]
  (let [{:keys [type code namespace]} evolution
        type-kw (keyword type)]
    (cond
      (not (#{:add-tool :modify-tool :add-namespace} type-kw))
      {:valid false :reason "Unknown evolution type"}
      
      (and (= type-kw :add-namespace) 
           (contains? #{"cljs.core" "mcp.core" "mcp.protocol"} namespace))
      {:valid false :reason "Cannot modify core namespaces"}
      
      (and code (str/includes? code "js/eval"))
      {:valid false :reason "Direct JavaScript evaluation not allowed"}
      
      (and code (str/includes? code "process.exit"))
      {:valid false :reason "Process control not allowed"}
      
      :else
      {:valid true})))

(defn create-tool-handler
  "Safely create a tool handler function from code string"
  [code-str tool-type]
  (try
    (case tool-type
      :arithmetic (evaluator/create-arithmetic-tool code-str)
      :string (evaluator/create-string-tool code-str)
      ;; Default: try both
      (or (evaluator/create-arithmetic-tool code-str)
          (evaluator/create-string-tool code-str)))
    (catch js/Error e
      nil)))

(defn add-runtime-tool!
  "Add a new tool at runtime"
  [{:keys [name description code schema tool-type]} server-state-atom]
  (let [handler (create-tool-handler code tool-type)]
    (if handler
      (do
        (swap! server-state-atom 
               assoc-in [:tools (keyword name)]
               {:description description
                :schema (or schema {:type "object"})
                :handler handler
                :runtime true
                :tool-type tool-type
                :code code
                :created-at (js/Date.now)})
        {:success true
         :message (str "Tool '" name "' added successfully")})
      {:success false
       :message "Failed to create tool handler"})))

(defn handle-evolve
  "Handle a request to evolve/modify the server"
  [{:keys [type code namespace approval-token name description schema] :as evolution} server-state-atom]
  (journal/record! {:type :evolution-request :data evolution})
  
  (let [validation (validate-evolution evolution)]
    (if (:valid validation)
      (case (keyword type)
        :add-tool
        (let [result (add-runtime-tool! evolution server-state-atom)]
          (journal/record! {:type (if (:success result) 
                                   :tool-added 
                                   :tool-add-failed)
                           :data (assoc evolution :result result)})
          result)
        
        ;; Default case - not yet implemented
        (do
          (journal/record! {:type :evolution-approved :data evolution})
          {:success true
           :message (str "Evolution type '" type "' validated but not yet implemented")}))
      (do
        (journal/record! {:type :evolution-rejected 
                         :data (merge evolution validation)})
        {:success false
         :message (:reason validation)}))))