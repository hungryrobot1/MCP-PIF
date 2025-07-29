(ns mcp.core
  "Core MCP server implementation with self-modifying capabilities"
  (:require [cljs.core.async :as async :refer [go go-loop <! >! chan]]
            [mcp.protocol :as protocol]
            [mcp.meta :as meta]
            [mcp.tools :as tools]
            [mcp.journal :as journal]))

(defonce server-state (atom {:running false
                             :capabilities []
                             :tools {}
                             :namespaces []
                             :journal []}))

(defn log [& args]
  ;; Use stderr for logging to avoid interfering with JSON-RPC on stdout
  (apply js/console.error "[MCP]" args))

(defn init!
  "Initialize the MCP server"
  []
  (log "Initializing MCP server...")
  (reset! server-state {:running false
                        :capabilities [:tools :prompts :meta-programming]
                        :tools (tools/base-tools)
                        :namespaces ['mcp.core 'mcp.tools 'mcp.meta]
                        :journal []})
  (journal/init!)
  (log "MCP server initialized"))

(defn handle-request
  "Main request handler"
  [{:keys [method params id] :as request}]
  (log "Handling request:" method)
  ;; If no id, this is a notification and we shouldn't respond
  (when id
    (case method
      "initialize" (protocol/handle-initialize request @server-state)
      "tools/list" (protocol/success-response id 
                                             (protocol/handle-list-tools @server-state))
      "tools/call" (protocol/success-response id
                                            (protocol/handle-tool-call params server-state))
      "prompts/list" (protocol/success-response id {:prompts []})
      "meta/inspect" (protocol/success-response id
                                              (meta/handle-inspect params @server-state))
      (assoc (protocol/error-response -32601 "Method not found") :id id))))

(defn start!
  "Start the MCP server"
  []
  (when-not (:running @server-state)
    (log "Starting MCP server...")
    (swap! server-state assoc :running true)
    (protocol/start-server! handle-request)
    (log "MCP server started")))

(defn stop!
  "Stop the MCP server"
  []
  (when (:running @server-state)
    (log "Stopping MCP server...")
    (swap! server-state assoc :running false)
    (protocol/stop-server!)
    (log "MCP server stopped")))

(defn reload!
  "Hot-reload handler for development"
  []
  (log "Reloading...")
  (when (:running @server-state)
    (stop!)
    (start!)))

(defn main
  "Main entry point"
  [& args]
  (log "MCP Server (ClojureScript) v0.2.0")
  (log "Self-modifying capabilities: ACTIVE")
  (init!)
  (start!)
  
  ;; Keep process alive
  (js/process.on "SIGINT" (fn []
                            (log "Shutting down...")
                            (stop!)
                            (js/process.exit 0))))