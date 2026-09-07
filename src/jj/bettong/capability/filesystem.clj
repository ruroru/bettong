(ns jj.bettong.capability.filesystem
  "Reading files, and writing them only when asked."
  (:require [jj.bettong.impl.files :as files]
            [jj.bettong.impl.options :as options]))

(defn filesystem
  "Read files, and write them only if asked. Options:
     :root      confine every path to this directory (default: unrestricted)
     :writable? add write_file and delete_file (default false - reading is the
                default because it cannot damage anything)"
  ([] (filesystem {}))
  ([{:keys [root writable?] :as opts}]
   (options/reject-read-only! opts "filesystem")
   (let [read-tools
         [{:name "read_file"
           :description "Read and return the contents of a file."
           :parameters {:path {:type "string" :description "Path of the file to read." :required true}}
           :handler #(files/read-file opts %)}
          {:name "list_dir"
           :description "List the entries of a directory. Directories are suffixed with '/'."
           :parameters {:path {:type "string" :description "Directory to list. Defaults to '.'."}}
           :handler #(files/list-dir opts %)}]
         write-tools
         [{:name "write_file"
           :description "Create or overwrite a file with the given content. Parent directories are created automatically."
           :parameters {:path {:type "string" :description "Path of the file to write." :required true}
                        :content {:type "string" :description "The full contents to write." :required true}}
           :handler #(files/write-file opts %)}
          {:name "delete_file"
           :description "Delete a single file."
           :parameters {:path {:type "string" :description "Path of the file to delete." :required true}}
           :handler #(files/delete-file opts %)}]]
     {:id :filesystem
      :instructions (cond-> "You can read files with the file tools; use them rather than a shell for file work."
                      writable? (str " You can also create and delete them.")
                      root (str (format " Every path is confined to %s." root))
                      (not writable?) (str " You cannot create, change or delete files."))
      :tools (cond-> read-tools
               writable? (into write-tools))})))
