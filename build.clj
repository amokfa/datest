(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.github.YOUR-GITHUB-NAME/clojure-test-lib)
(def version "1.0.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean _)
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   version
                :basis     basis
                :src-dirs  ["src"]
                :scm {:url "https://github.com/amokfa/datest"
                      :tag "v1.0.0"
                      :connection "scm:git:git://github.com/amokfa/datest.git"
                      :developerConnection "scm:git:ssh://git@github.com/amokfa/datest.git"}})
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))