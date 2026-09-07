(ns jj.bettong.capability.edn
  "Reading and changing values inside EDN files."
  (:require [jj.bettong.edn :as edn-files]
            [jj.bettong.impl.options :as options]))

(defn edn
  "Read and update values inside EDN files - deps.edn, config files, anything.
   Cheaper and safer than having the model rewrite a whole file to change one
   value. Options:
     :root      confine every path to this directory (default: unrestricted)
     :writable? add set_edn and delete_edn (default false)

   Paths into the data are given as a JSON array of EDN-read elements, so
   \":server\" is the keyword :server and \"0\" is a vector index."
  ([] (edn {}))
  ([{:keys [root writable?] :as opts}]
   (options/reject-read-only! opts "edn")
   (let [key-path {:type "array"
                   :items {:type "string"}
                   ;; These examples must be written as valid JSON - comma-separated - or
                   ;; the model copies the syntax it is shown and emits unparseable arguments.
                   :description (str "JSON array of keys into the data, each element read as EDN. "
                                     "[\":server\", \":port\"] addresses {:server {:port ...}}; "
                                     "[\":flags\", \"0\"] addresses the first item of a vector.")}
         read-tools
         [{:name "read_edn"
           :description "Read an EDN file, or one value inside it."
           :parameters {:path {:type "string" :description "Path of the .edn file." :required true}
                        :key_path (assoc key-path :description
                                         (str (:description key-path) " Omit it to read the whole file."))}
           :handler #(edn-files/read-edn opts %)}]
         write-tools
         [{:name "set_edn"
           :description (str "Set one value inside an EDN file, leaving the rest of the file alone. "
                             "Missing intermediate maps are created.")
           :parameters {:path {:type "string" :description "Path of the .edn file." :required true}
                        :key_path (assoc key-path :required true)
                        :value {:type "string"
                                :description (str "The new value, written as EDN: 8080, \"localhost\", :prod, "
                                     ;; commas are whitespace in EDN, so these examples are
                                     ;; valid in both languages and cannot be miscopied
                                     "[1, 2], {:a 1}.")
                                :required true}}
           :handler #(edn-files/set-edn opts %)}
          {:name "delete_edn"
           :description "Remove a key from an EDN file."
           :parameters {:path {:type "string" :description "Path of the .edn file." :required true}
                        :key_path (assoc key-path :required true)}
           :handler #(edn-files/delete-edn opts %)}]]
     {:id :edn
      :instructions (cond-> "EDN files have their own tool: read_edn reads one value out of one."
                      writable? (str " set_edn changes a single value in place, and delete_edn removes one - "
                                     "prefer them over write_file for .edn files, which would rewrite the whole thing.")
                      root (str (format " Every path is confined to %s." root))
                      (not writable?) (str " You cannot change EDN files."))
      :tools (cond-> read-tools
               writable? (into write-tools))})))
