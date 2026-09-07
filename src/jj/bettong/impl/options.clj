(ns jj.bettong.impl.options
  "Option checks shared by the capabilities.")

(defn reject-read-only!
  "`:read-only?` was how writing used to be turned off. Reading is the default
   now, so a stray `:read-only? false` would quietly do the opposite of what it
   says. Better to say so."
  [opts capability]
  (when (contains? opts :read-only?)
    (throw (ex-info (format "%s no longer takes :read-only? - reading is the default. Pass :writable? true to allow writes."
                            capability)
                    {:capability capability :option :read-only?}))))
