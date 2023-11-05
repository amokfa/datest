(ns usage
  (:require [datest.core :refer [run-test get-failed summarize-result combine-tests flatten-result testing] :refer-macros [testing]]))

; 'testing' returns a test spec that can be passed to 'run-test'
(def t (testing :a
                (testing :b []
                         {:result :OK})
                (testing :c []
                         {:result  :ERR
                          :message "failing test"})
                (testing :d []
                         (throw (new Error))
                         {:result  :OK
                          :message "ignored"})))

(def res (run-test t))
; returns:
#_{:a {:b {:result :OK, :state {}},
       :c {:result :ERR, :message "failing test", :state {}},
       :d {:result :EXCEPTION,
           :exception error,
           :state {}}}}

(def failed (get-failed res))
#_{:a {:c {:result :ERR, :message "failing test", :state {}},
       :d {:result :EXCEPTION,
           :exception error,
           :state {}}}}

(def lst (flatten-result failed))
; returns a sequence of three failed results

(def fst (first lst))
; first failed result
#_{:result :ERR,
   :message "failing test",
   :state {},
   :path (:a :c)}

; The body of a test case must return a map with `:result` as a key, it's value must be `:OK` if the test was successful
; This map may contain other data as well

; test suits can be combined:

(defn int-tests []
  (testing :ints
           (testing :adding_1 []
                    (if (= (+ 5 1) 6)
                      {:result :OK}
                      {:result :ERR}))
           (testing :adding_2 []
                    (if (= (+ 5 2) 6)
                      {:result :OK}
                      {:result  :ERR
                       :message "values not equal"}))
           (testing :failing_test []
                    (throw (new Error)))))

(defn string-tests []
  (testing :strings
           (testing :concating_asdf []
                    (if (= (str "hello" "asdf") "helloasdf")
                      {:result :OK}
                      {:result :ERR}))))

(defn all-tests []
  (combine-tests [(int-tests)
                  (string-tests)]))

(def res2 (run-test (all-tests)))
#_{:ints {:adding_1 {:result :OK, :state {}},
          :adding_2 {:result :ERR, :message "values not equal", :state {}},
          :failing_test {:result :EXCEPTION,
                         :exception error,
                         :state {}}},
   :strings {:concating_asdf {:result :OK, :state {}}}}

; Leaf nodes in a test suite may have a vector of symbols which define the state of that test case.
; This symbols will resolve to atoms in the context of test suite. Their values will be returned in
; the `:state` key of result

(def t3 (testing :test_with_state [a b c]
                 (reset! a 0)
                 (reset! b 1)
                 (reset! c (/ @b @a))
                 {:result :OK}))

(def res3 (run-test t3))
#_{:test_with_state {:result :EXCEPTION,
                     :exception {:cause "Divide by zero"},
                     :state {a 0, b 1, c :datest/UNINITIALIZED}}}

(enable-console-print!)
; summary
(defn summarize [r]
  (println (summarize-result r)))

(summarize res)
#_{:total_tests 4, :passed 1, :failed 2, :errors 1}
(summarize res2)
(summarize res3)