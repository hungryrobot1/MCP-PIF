(ns mcp.protocol
  "MCP protocol implementation"
  (:require [cljs.core.async :as async :refer [go <! >! chan]]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(defonce server-channel (chan))
(defonce connection (atom nil))

(defn json->clj [json-str]
  (-> json-str
      js/JSON.parse
      (js->clj :keywordize-keys true)))

(defn clj->json [data]
  (-> data
      clj->js
      js/JSON.stringify))

(defn send-response! [response]
  (let [json-response (clj->json response)]
    (.write js/process.stdout (str json-response "\n"))))

(defn error-response 
  ([code message] 
   {:jsonrpc "2.0"
    :error {:code code
            :message message}})
  ([code message data]
   {:jsonrpc "2.0"
    :error {:code code
            :message message
            :data data}}))

(defn success-response [id result]
  {:jsonrpc "2.0"
   :id id
   :result result})

(defn handle-initialize [request server-state]
  (success-response (:id request)
                    {:protocolVersion "2024-11-05"
                     :capabilities {:tools {:listChanged true}
                                   :prompts {}
                                   :experimental {:metaProgramming true
                                                 :selfModification true}}
                     :serverInfo {:name "mcp-pif-cljs"
                                 :version "0.2.0"}}))

(defn handle-list-tools [server-state]
  {:tools (vec (for [[tool-name tool] (:tools server-state)]
                 {:name (name tool-name)
                  :description (:description tool)
                  :inputSchema (:schema tool)}))})

(defn handle-tool-call [params server-state-atom]
  (let [tool-name (keyword (:name params))
        tool (get-in @server-state-atom [:tools tool-name])
        arguments (:arguments params)]
    (if tool
      (try
        (let [result ((:handler tool) arguments server-state-atom)]
          {:content [{:type "text"
                     :text (str result)}]})
        (catch js/Error e
          {:error {:message "Tool execution failed"
                  :details (.-message e)}}))
      {:error {:message "Unknown tool"
              :details (str "Tool not found: " tool-name)}})))

(defn process-input [input handler]
  (try
    (let [request (json->clj input)
          request-id (:id request)
          response (handler request)]
      ;; Only send response if there is one (handler returns non-nil)
      (when response
        (send-response! (if request-id 
                         (assoc response :id request-id)
                         response))))
    (catch js/Error e
      (js/console.error "[Protocol] Error processing input:" e)
      ;; Only send error response if request had an id
      (let [request-id (try 
                        (:id (json->clj input))
                        (catch js/Error _ nil))]
        (when request-id
          (send-response! (assoc (error-response -32603 "Failed to process request" (.-message e))
                                :id request-id))))))

(defn start-server! [handler]
  (js/console.error "[Protocol] Starting JSON-RPC server on stdio...")
  
  ;; Set up stdin reader
  (.setEncoding js/process.stdin "utf8")
  
  ;; Resume stdin (necessary in Node.js)
  (.resume js/process.stdin)
  
  (let [buffer (atom "")]
    (.on js/process.stdin "data"
         (fn [chunk]
           (swap! buffer str chunk)
           ;; Process complete lines
           (let [current-buffer @buffer
                 lines (str/split current-buffer #"\n")]
             ;; Check if buffer ends with newline
             (if (str/ends-with? current-buffer "\n")
               (do
                 ;; Process the complete line(s) and clear buffer
                 (reset! buffer "")
                 (let [line (str/trim current-buffer)]
                   (when-not (str/blank? line)
                     (process-input line handler))))
               ;; Incomplete message, keep waiting
               (js/console.log "[Protocol] Waiting for newline, buffer size:" (count current-buffer)))))))
  
  ;; Handle process termination
  (.on js/process.stdin "end" #(js/process.exit 0)))

(defn stop-server! []
  (js/console.log "[Protocol] Stopping server..."))