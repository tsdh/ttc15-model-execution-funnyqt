(ns ttc15-model-execution-funnyqt.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [funnyqt.query :refer [the forall?]]
            [funnyqt.emf :refer :all]
            [funnyqt.visualization :as viz]
            [funnyqt.utils :as u]
            [ttc15-model-execution-funnyqt.core :refer :all]
            [ttc15-model-execution-funnyqt.ad :as a]))


(defn check-total-execution-order [trace & names]
  (let [real-names (mapv a/name (a/->executedNodes trace))]
    (is (= real-names names))))

(defn check-partial-execution-order [trace & names]
  (let [exp-names (vec names)
        real-names (mapv a/name (a/->executedNodes trace))
        order-indexes (int-array (count exp-names))
        first-order-idx (fn [n]
                          (loop [i 0, rn real-names]
                            (if (= n (first rn))
                              i
                              (recur (inc i) (rest rn)))))]
    (dotimes [i (alength order-indexes)]
      (aset-int order-indexes i (first-order-idx (nth exp-names i))))
    (dotimes [i (dec (alength order-indexes))]
      (is (< (aget order-indexes i) (aget order-indexes (inc i)))))))

(defn check-unexecuted [trace & names]
  (let [unex-names (set names)
        real-names (mapv a/name (a/->executedNodes trace))
        too-many (filter unex-names real-names)]
    (is (empty? too-many))))

(defn variable-value [ad var-name]
  (a/value (a/->currentValue (the #(= var-name (a/name %)) (a/all-Variables ad)))))

(defn no-offers-and-tokens-left-over [ad]
  (is (empty? (a/all-Offers ad)))
  (is (empty? (a/all-activitydiagram$Tokens ad))))

(defmacro make-test [name file & assertions]
  (let [get-name (fn get-name [file]
                   (if (coll? file)
                     (str/join ", " (map get-name file))
                     (.getName (io/file file))))]
    `(deftest ~name
       (println "Model" ~(clojure.core/name name))
       (dotimes [i# 2]
         (println "  Run" (inc i#))
         (let [~'ad ~(if (coll? file)
                       `(doto (new-resource-set)
                          ~@(for [f file]
                              `(get-resource ~f true)))
                       `(load-resource ~file))
               ~'trace (u/timing "    Executing activity diagram:\n      file(s)   => %s\n      exec time => %T"
                                 (execute-activity-diagram ~'ad)
                                 ~(get-name file))]
           (no-offers-and-tokens-left-over ~'ad)
           ~@assertions)))))

(make-test test1 "test/model/test1.xmi"
           (check-total-execution-order trace "initialNode1" "action1" "finalNode1"))

(make-test test2 "test/model/test2.xmi"
           (check-partial-execution-order
            trace "initialNode2" "forkNode1" "action2" "joinNode1" "finalNode2")
           (check-partial-execution-order
            trace "initialNode2" "forkNode1" "action3" "joinNode1" "finalNode2"))

(make-test test3 "test/model/test3.xmi"
           (check-total-execution-order
            trace "initialNode3" "decisionNode1" "action4" "mergeNode1" "finalNode3")
           (check-unexecuted trace "action5"))

(make-test test4 "test/model/test4.xmi"
           (check-total-execution-order
            trace "initialNode4" "action6" "action7" "action8" "action9" "finalNode4" )
           (is (= 3 (variable-value ad "var3")))
           (is (= 1 (variable-value ad "var4")))
           (is (= 2 (variable-value ad "var5")))
           (is (variable-value ad "var6"))
           (is (not (variable-value ad "var7")))
           (is (variable-value ad "var8")))

(make-test test5 ["test/model/test5.xmi" "test/model/test5_input.xmi"]
           (check-total-execution-order
            trace "initialNode5" "action10" "finalNode5")
           (is (= 10 (variable-value ad "var9")))
           (is (= 5 (variable-value ad "var10")))
           (is (= 15 (variable-value ad "var11"))))

(make-test test6-true ["test/model/test6.xmi" "test/model/test6_true_input.xmi"]
           (check-partial-execution-order
            trace "initialNode6" "register" "decisionInternal" "getWelcomePackage"
            "forkGetWelcomePackage" "joinManagerInterview" "managerInterview" "managerReport"
            "mergeAuthorizePayment" "authorizePayment" "finalNode6")
           (check-partial-execution-order
            trace "forkGetWelcomePackage" "assignToProject" "joinManagerInterview")
           (check-partial-execution-order
            trace "forkGetWelcomePackage" "addToWebsite" "joinManagerInterview")
           (check-unexecuted trace "assignToProjectExternal"))

(make-test test6-false ["test/model/test6.xmi" "test/model/test6_false_input.xmi"]
           (check-total-execution-order
            trace "initialNode6" "register" "decisionInternal" "assignToProjectExternal"
            "mergeAuthorizePayment", "authorizePayment", "finalNode6")
           (check-unexecuted
            trace "getWelcomePackage" "forkGetWelcomePackage" "assignToProject"
            "addToWebsite" "joinManagerInterview" "managerInterview" "managerReport"))


(make-test test-performance-variant-1 "test/model/testperformance_variant1.xmi"
           (is (= 1002 (count (a/->executedNodes trace)))))

(make-test test-performance-variant-2 "test/model/testperformance_variant2.xmi"
           (is (= 1004 (count (a/->executedNodes trace)))))


(make-test test-performance-variant-3-1 "test/model/testperformance_variant3_1.xmi"
           (is (= 1004 (count (a/->executedNodes trace))))
           (doseq [lv (remove #(= "one" (a/name %))
                              (a/->locals (first (a/all-Activities ad))))]
             (is (= 10 (-> lv a/->currentValue a/value)))))

(make-test test-performance-variant-3-2
           ["test/model/testperformance_variant3_2.xmi" "test/model/testperformance_variant3_2_input.xmi"]
           (is (= 1001 (count (a/->executedNodes trace))))
           (is (= 141 (variable-value ad "loop"))))
