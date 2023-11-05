(ns datest.core
  (:require [clojure.data :refer [diff]]))

(def VecType #?(:clj  clojure.lang.PersistentVector
                :cljs cljs.core/PersistentVector))

(defn get-exception-filter []
  #?(:clj  (-> *ns*
               ns-name
               name
               (clojure.string/split #"\.")
               first)
     :cljs nil))

(defmacro dbg [form]
  (let [result-s (symbol "result")
        repr (str form)]
    `(let [~result-s ~form]
       (println (str ~repr " : " ~result-s))
       ~result-s)))

(defmacro testing-inner-node [name & subs]
  (doseq [sub subs]
    (assert (and (seqable? sub)
                 (= 'testing (first sub)))))
  `(assoc (sorted-map)
     ~name
     (apply merge ~(vec subs))))

(defmacro testing-leaf-node [name & rst]
  (let [[params & body] rst
        args (->> params
                  (map (fn [p] `[~p '~p]))
                  (into {}))
        test-fn `(fn [~args]
                   ~@body)]
    (assoc (sorted-map)
      name `(with-meta ~test-fn
                       {:code        (quote ~test-fn)
                        :params      '~params
                        :module-name (get-exception-filter)}))))

(defmacro testing [name & rst]
  (if (instance? VecType (first rst))
    `(testing-leaf-node ~name ~@rst)
    `(testing-inner-node ~name ~@rst)))

(defn return-comparison [expected actual]
  (if (= expected actual)
    {:result :OK}
    {:result   :ERR
     :expected expected
     :actual   actual
     :diff     (diff expected actual)}))

(defn update-exception [ex module-name]
  #?(:clj
     (let [old-stack (.getStackTrace ex)
           new-stack (into-array StackTraceElement (filter #(clojure.string/includes? (.getClassName %) module-name) (seq old-stack)))]
       (doto ex
         (.setStackTrace new-stack)))
     :cljs ex))

(defn run-test [ts]
  (->> ts
       (map (fn [[name body]]
              [name (let [md (meta body)]
                      (if md
                        (let [{params :params code :code module-name :module-name} md
                              param_bindings (->> params
                                                  (map (fn [p] [p (atom :datest/UNINITIALIZED)]))
                                                  (into {}))]
                          (assoc
                            (try
                              (body param_bindings)
                              (catch #?(:clj  Throwable
                                        :cljs :default) e
                                {:result    :EXCEPTION
                                 :exception (update-exception e module-name)}))
                            ;:context code
                            :state (->> param_bindings
                                        (map (fn [[k v]] [k @v]))
                                        (into {}))))
                        (run-test body)))]))
       (into (sorted-map))))

(defn flatten-result [res]
  (apply concat (for [[name body] res]
                  (if (contains? body :state)
                    [(assoc body :path (seq [name]))]
                    (map #(update % :path conj name) (flatten-result body))))))

(defn treefy-result [res]
  (let [chs (group-by #(first (:path %)) res)]
    (reduce
      (fn [r [name children]]
        (if (nil? name)
          (dissoc (first children) :path)
          (assoc r name (treefy-result (map #(update % :path rest) children)))))
      (sorted-map)
      chs)))

(defn get-failed [res]
  (->> res
       flatten-result
       (filter #(not= (:result %)
                      :OK))
       treefy-result))

(defn filter-tests-by-result [res st]
  (->> res
       flatten-result
       (filter #(= st (:result %)))
       treefy-result))

(defn summarize-result [res]
  (let [flat (flatten-result res)
        grouped (group-by :result flat)]
    {:total_tests (count flat)
     :passed      (count (grouped :OK))
     :failed      (count (grouped :ERR))
     :errors      (count (grouped :EXCEPTION))}))

(defn combine-tests [ts]
  (reduce #(merge-with merge %1 %2) ts))