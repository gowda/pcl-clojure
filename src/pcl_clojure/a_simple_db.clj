(ns pcl-clojure.a-simple-db
  "Implementation of `A Simple Database` from `Practical Common Lisp` text.
   More detailed documentation of the logical progression of the final
   result can be found in `Practical Common Lisp` book or at the online
   version at http://www.gigamonkeys.com/book/practical-a-simple-database.html")

(def ^{:doc "The ultimate in-memory data store."}
  db (atom []))

(defn add-record
  "Add a record to the database. Schema for individual record is self-contained,
   as every record is a map."
  [record]
  (swap! db conj record))

(defn add-records
  "Add multiple records at once."
  [records]
  (doseq [record records]
    (add-record record)))

(defn save-db
  "Persist the database to a file."
  [filename]
  (spit filename (pr-str @db)))

(defn load-db
  "Load the database from a file."
  [filename]
  (try
    (swap! db (fn [old new] new) (read-string (slurp filename)))
    (catch Exception e
      (do (println (.getMessage e))
          (println "Database storage " filename " seems to be corrupt.")
          (println "Continuing with an empty database.")))))

(defn- matches?
  "Returns true if a criteria is matched for a map."
  [criteria m]
  (some (fn [[k v]] (= v (k m))) criteria))

(defn select
  "Returns a list of maps which match the criteria for select."
  [criteria]
  (if (empty? criteria)
    @db
    (let [criteria-keys (into #{} (keys criteria))]
      (into [] (filter (fn [m]
                         (matches? criteria m))
                       @db)))))

(defn update
  "Updates the record which matches the criteria with values provided. If values
   happen to contain keys which are not already present, they are simply added."
  [criteria values]
  (let [criteria-keys (into #{} (keys criteria))
        new-db (into []
                     (map (fn [m]
                            (if (matches? criteria m)
                              (conj m values)
                              m)) @db))]
    (swap! db (fn [old new] new) new-db)))


(defn delete
  "Delete the record which matches the criteria."
  [criteria]
  (let [new-db (into []
                     (filter (fn [m]
                               (not (matches? criteria m)))
                             @db))]
    (swap! db (fn [old new] new) new-db)))
