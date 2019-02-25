{:user {:plugins [[cider/cider-nrepl "0.14.0"]
                  [lein-ancient "0.6.8"]
                  [lein-cljfmt  "0.6.4"]
                  [lein-codox "0.10.3"]
                  [lein-try "0.4.3"]
                  [lein-virgil "0.1.0"]
                  [refactor-nrepl "2.4.0"]
                  [jonase/eastwood "0.2.3"]]
        :codox {:output-path "codox"}
        :dependencies [[org.clojure/tools.trace "0.7.9"]
                       [slamhound "1.5.5"]
                       [cljfmt "0.6.1"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :cljfmt  {:indents  {facts  [[:inner 0]]
                             fact  [[:inner 0]]
                             fact-group [[:inner 0]]}}}}
