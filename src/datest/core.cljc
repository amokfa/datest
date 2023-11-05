(ns datest.core
  (:require [clojure.data :refer [diff]]))

(defn grouper [b]
  (group-by #(if (and (seqable? %)
                      (symbol? (first %))
                      (= "testing" (name (first %))))
               :testing :main) b))

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
                 (= 'testing2 (first sub)))))
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

(defmacro testing2 [name & rst]
  (if (instance? VecType (first rst))
    `(testing-leaf-node ~name ~@rst)
    `(testing-inner-node ~name ~@rst)))

; name !== :main
; params == [symbol]
(defmacro testing [name & rst]
  (let [[params & body] (if (instance? VecType (first rst))
                          rst
                          (cons [] rst))
        {subs :testing fns :main} (grouper body)
        res (mapv
              (fn [s]
                (let [n (second s)]
                  `[~n (~s ~n)]))
              subs)
        args (->> params
                  (map (fn [p] `[~p '~p]))
                  (into {}))
        fn `(fn [~args]
              ~@fns)]
    {name (into (sorted-map)
                (if fns
                  (conj res [:main `(with-meta ~fn
                                               {:code        (quote ~fn)
                                                :params      '~params
                                                :module-name (get-exception-filter)})])
                  res))}))

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
  (into (sorted-map)
        (for [[name body] ts]
          [name
           (if (= name :main)
             (let [{params :params code :code module-name :module-name} (meta body)
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
             (run-test body))])))

(defn flatten-result [res]
  (apply concat (for [[name body] res]
                  (if (= :main name)
                    [(assoc body :path '(:main))]
                    (map #(update % :path conj name) (flatten-result body))))))

(defn treefy-result [res]
  (let [chs (group-by #(first (:path %)) res)]
    (reduce
      (fn [r [name body]]
        (assoc r name (if (= name :main)
                        (dissoc (first body) :path)
                        (treefy-result (map #(update % :path rest) body)))))
      {}
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