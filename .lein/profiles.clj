{:user {:plugins [[lein-ancient "0.6.7"]
                  [lein-ring "0.9.3"]
                  [lein-midje "3.1.3"]
                  [com.2tothe8th/lein-expect "0.1.0"]
                  [lein-pprint "1.1.2"]
                  [lein-kibit "0.0.8"]
                  #_[cider/cider-nrepl "0.8.2"]
                  [codox "0.8.12"]
                  [lein-gorilla "0.3.4"]
                  [mvxcvi/whidbey "1.0.0"]]
        :aliases {"omni" ["do" ["clean"]
                          ["with-profile" "production" "deps" ":tree"]
                          ["ancient"] ["v"] ["kibit"] ["midje"]]}
        :signing {:gpg-key "5F5664F2"}
        :jvm-opts ["-Dapple.awt.UIElement=true"]
        :whidbey {:escape-types '#{datomic.db.DB datomic.btset.BTSet}
                  :color-scheme {:boolean [:bold :magenta],
                                 :character [:magenta],
                                 :class-delimiter [:magenta],
                                 :class-name [:bold :magenta],
                                 :delimiter [:bold :cyan],
                                 :function-symbol nil,
                                 :keyword [:bold :blue],
                                 :number [:bold :magenta],
                                 :string [:magenta],
                                 :tag [:bold :magenta]}}
        :gorilla-options {:keymap {"command:evaluator:evaluate" "command+e"}}}
 :repl {:dependencies [[criterium "0.4.3"]
                       [com.2tothe8th/example "0.5.0+repl"]]}
 :auth {:repository-auth
        {#"s3(p)?:\/\/rk-maven" {:username :env/AWS_ACCESS_KEY_ID
                                 :passphrase :env/AWS_SECRET_ACCESS_KEY}}}}
