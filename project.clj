(defproject jnalien "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.java.dev.jna/jna "3.4.0"]]
  :source-paths      ["src/clj"]
  :test-paths ["test/clj"]
  :profiles {:dev {:jvm-opts ["-Djna.library.path=test/cpp/build"]}})
