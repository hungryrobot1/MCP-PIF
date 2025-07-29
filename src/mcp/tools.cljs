(ns mcp.tools
  "Base tools for the MCP server"
  (:require [mcp.journal :as journal]
            [mcp.meta :as meta]))

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
    (str "Recent journal entries:\n"
         (clojure.string/join "\n" 
                              (map #(str (:timestamp %) " - " (:type %) 
                                        (when-let [data (:data %)]
                                          (str ": " data)))
                                   entries)))))

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
         (clojure.string/join "\n" tool-list) "\n"
         "\n"
         "Active Namespaces: " (clojure.string/join ", " (:namespaces state)) "\n"
         "Features: " (clojure.string/join ", " (map name (:capabilities state))) "\n"
         "Memory: " memory-count " items stored\n"
         "Journal entries: active")))

(defn meta-evolve
  "Tool wrapper for meta/evolve functionality"
  [{:keys [type name description code tool-type schema] :as params} server-state-atom]
  (let [result (meta/handle-evolve params server-state-atom)]
    (if (:success result)
      (:message result)
      (str "Evolution failed: " (:message result)))))

(defn base-tools
  "Return the base set of tools"
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
                                                :enum ["arithmetic" "string"]
                                                :description "Type of tool being created"}
                                     :schema {:type "object"
                                             :description "JSON Schema for the tool's parameters"}}
                         :required ["type" "name" "description" "code"]}
                 :handler meta-evolve}
   
   :execute-tool {:description "Execute any tool by name, including dynamically created tools. Use this to call tools created with meta-evolve."
                  :schema {:type "object"
                          :properties {:tool-name {:type "string"
                                                  :description "Name of the tool to execute"}
                                      :arguments {:type "object"
                                                 :description "Arguments to pass to the tool"}}
                          :required ["tool-name" "arguments"]}
                  :handler (fn [{:keys [tool-name arguments]} server-state-atom]
                            (let [tool (get-in @server-state-atom [:tools (keyword tool-name)])]
                              (if tool
                                (try
                                  ((:handler tool) arguments server-state-atom)
                                  (catch js/Error e
                                    (str "Error executing tool: " (.-message e))))
                                (str "Tool not found: " tool-name))))}})