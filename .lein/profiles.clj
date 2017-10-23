{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-midje "3.2.1"]
                  [lein-kibit "0.1.5"]
                  [lein-pprint "1.1.2"]
                  [codox "0.10.3"]
                  [cider/cider-nrepl "0.15.0"]
                  [mvxcvi/whidbey "1.3.1"]]
        :whidbey {:escape-types '#{datomic.db.DB datomic.btset.BTSet}
                  :color-scheme {:boolean [:bold :magenta],
                                 :character [:magenta],
                                 :class-delimiter [:magenta],
                                 :class-name [:bold :magenta],
                                 :delimiter [:bold :white],
                                 :function-symbol nil,
                                 :keyword [:bold :blue],
                                 :number [:bold :magenta],
                                 :string [:magenta],
                                 :tag [:bold :magenta]}}}
 :repl {:dependencies [[criterium "0.4.4"]
                       [com.2tothe8th/example "0.5.0+repl"]]}}
