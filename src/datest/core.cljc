(ns datest.core
  (:require [clojure.data :refer [diff]]))

(def VecType #?(:clj  clojure.lang.PersistentVector
                :cljs cljs.core/PersistentVector))

(def ^:dynamic EXCEPTION_FILTER nil)

(defn get-exception-filter []
  (if EXCEPTION_FILTER
    EXCEPTION_FILTER
    #?(:clj  (-> *ns*
                 ns-name
                 name
                 (clojure.string/split #"\.")
                 first
                 (clojure.string/replace #"-" "_")
                 clojure.core/re-pattern)
       :cljs nil)))

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
                       {:code             (quote ~test-fn)
                        :params           '~params
                        :exception-filter (get-exception-filter)}))))

(defmacro testing [name & rst]
  (assert (not= name :state))
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

(defn update-exception [ex exception-filter]
  #?(:clj
     (let [old-stack (.getStackTrace ex)
           new-stack (into-array StackTraceElement (filter #(clojure.core/re-find exception-filter (.getClassName %))
                                                           (seq old-stack)))]
       (doto ex
         (.setStackTrace new-stack)))
     :cljs ex))

(defn run-test [ts]
  (let [md (meta ts)]
    (if md
      (let [body ts
            {params :params code :code exception-filter :exception-filter} md
            param_bindings (->> params
                                (map (fn [p] [p (atom :datest/UNINITIALIZED)]))
                                (into {}))]
        (assoc
          (try
            (body param_bindings)
            (catch #?(:clj  Throwable
                      :cljs :default) e
              {:result    :EXCEPTION
               :exception (update-exception e exception-filter)}))
          ;:context code
          :state (->> param_bindings
                      (map (fn [[k v]] [k @v]))
                      (into {}))))
      (->> ts
           (map (fn [[name body]]
                  [name (run-test body)]))
           (into (sorted-map))))))

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