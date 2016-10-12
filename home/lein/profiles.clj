{:user {:plugins [[cider/cider-nrepl "0.10.0"]
                  [lein-ancient "0.6.8"]
                  [lein-cljfmt  "0.5.1"]
                  [lein-try "0.4.3"]]
        :dependencies [[org.clojure/tools.trace "0.7.9"]
                       [slamhound "1.5.5"]
                       [cljfmt "0.5.1"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :cljfmt  {:indents  {facts  [[:inner 0]]
                             fact  [[:inner 0]]
                             fact-group [[:inner 0]]}}}}
