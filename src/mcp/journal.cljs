(ns mcp.journal
  "Journaling system for tracking all server activities"
  (:require [datascript.core :as d]
            [cljs.reader :as reader]))

(defonce journal-db (atom nil))

(def journal-schema
  {:entry/timestamp {:db/index true}
   :entry/type {:db/index true}
   :entry/data {}
   :entry/metadata {}})

(defn init!
  "Initialize the journal database"
  []
  (reset! journal-db (d/create-conn journal-schema)))

(defn record!
  "Record an entry in the journal"
  [{:keys [type data metadata] :as entry}]
  (let [timestamp (js/Date.now)
        full-entry {:entry/timestamp timestamp
                   :entry/type type
                   :entry/data (pr-str data)
                   :entry/metadata (or metadata {})}]
    (d/transact! @journal-db [full-entry])
    timestamp))

(defn recent-entries
  "Get the most recent journal entries"
  [limit]
  (let [db @journal-db
        entries (d/q '[:find ?e ?timestamp ?type ?data
                      :where
                      [?e :entry/timestamp ?timestamp]
                      [?e :entry/type ?type]
                      [?e :entry/data ?data]]
                    db)]
    (->> entries
         (sort-by second >)
         (take limit)
         (map (fn [[e timestamp type data]]
                {:timestamp (js/Date. timestamp)
                 :type type
                 :data (try (reader/read-string data)
                           (catch js/Error _ data))})))))

(defn entries-by-type
  "Get journal entries of a specific type"
  [entry-type]
  (let [db @journal-db]
    (d/q '[:find ?e ?timestamp ?data
          :in $ ?type
          :where
          [?e :entry/type ?type]
          [?e :entry/timestamp ?timestamp]
          [?e :entry/data ?data]]
        db entry-type)))

(defn rollback-to
  "Rollback the journal to a specific timestamp"
  [timestamp]
  ;; This would be implemented based on the specific rollback strategy
  ;; For now, it's a placeholder
  (record! {:type :rollback-attempted
           :data {:target-timestamp timestamp}}))