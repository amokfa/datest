(ns t
  (:require [clojure.pprint :refer [pprint]]))

(defmacro m [& args]
  `(list ~@args))