(defproject org.clojars.jj/bettong "0.1.0"
  :description "A small agent library: give an agent capabilities you define, and it uses them."
  :url "https://github.com/ruroru/bettong"
  :license {:name "Eclipse Public License 2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [org.clojars.jj/potoroo "0.1.0"]
                 [org.clojure/data.json "2.5.2"]]
  :deploy-repositories [["clojars" {:url      "https://repo.clojars.org"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false
                                    }]]

  :plugins [[org.clojars.jj/bump "1.0.4"]
            [lein-cloverage "1.2.4"]
            [org.clojars.jj/strict-check "1.1.0"]
            [org.clojars.jj/lein-git-tag "1.0.1"]
            [org.clojars.jj/bump-md "1.1.0"]
            ]

  :main ^:skip-aot jj.bettong.cli
  :target-path "target/%s"
  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :all (constantly true)}
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojars.jj/ring-http-exchange "1.4.9"]]}})
