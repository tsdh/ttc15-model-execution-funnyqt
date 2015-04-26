(ns ttc15-model-execution-funnyqt.core
  (:require [funnyqt.emf :refer :all]
            [funnyqt.query :refer [forall? exists? the]]
            [funnyqt.polyfns :refer :all]
            [funnyqt.utils :refer [doseq+ mapc]]))

;;* Load metamodel and generate accessors

(load-ecore-resource "activitydiagram.ecore")
(generate-ecore-model-functions "activitydiagram.ecore" ttc15-model-execution-funnyqt.ad a)

;;* Solution

(defn init-variables [activity input]
  (doseq+ [lv (a/->locals activity)]
    (when-let [init-value (a/->initialValue lv)]
      (a/->set-currentValue! lv init-value)))
  (doseq+ [iv (and input (a/->inputValues input))]
    (when-let [val (a/->value iv)]
      (a/->set-currentValue! (a/->variable iv) val))))

(defn enabled-nodes [activity]
  (filter (fn [n]
            (and (a/running? n)
                 (not (a/isa-InitialNode? n))
                 ((if (a/isa-MergeNode? n) exists? forall?)
                  #(seq (a/->offers %)) (a/->incoming n))))
          (a/->nodes activity)))

(defn offer-one-ctrl-token [node]
  (let [ctrl-t (a/create-ControlToken! nil)
        out-cf (the (a/->outgoing node))
        offer  (a/create-Offer! nil {:offeredTokens [ctrl-t]})]
    (a/->add-heldTokens! node ctrl-t)
    (a/->add-offers! out-cf offer)))

(declare-polyfn exec-node [node])

(defpolyfn exec-node InitialNode [i]
  (offer-one-ctrl-token i))

(defn consume-offers [node]
  (let [offers    (mapcat a/->offers (a/->incoming node))
        tokens    (vec (mapcat a/->offeredTokens offers))
        ctrl-toks (filter a/isa-ControlToken? tokens)
        fork-toks (filter a/isa-ForkedToken? tokens)]
    (mapc edelete! offers)
    (a/->set-heldTokens! node ctrl-toks)
    (doseq+ [ft fork-toks]
      (when-let [bt (a/->baseToken ft)]
        (edelete! bt))
      (a/set-remainingOffersCount! ft (dec (a/remainingOffersCount ft)))
      (when (zero? (a/remainingOffersCount ft))
        (edelete! ft)))
    ctrl-toks))

(def op2fn {(a/eenum-IntegerCalculationOperator-ADD)           +
            (a/eenum-IntegerCalculationOperator-SUBRACT)       -
            (a/eenum-IntegerComparisonOperator-SMALLER)        <
            (a/eenum-IntegerComparisonOperator-SMALLER_EQUALS) <=
            (a/eenum-IntegerComparisonOperator-EQUALS)         =
            (a/eenum-IntegerComparisonOperator-GREATER_EQUALS) >=
            (a/eenum-IntegerComparisonOperator-GREATER)        >
            (a/eenum-BooleanBinaryOperator-AND)                #(and %1 %2)
            (a/eenum-BooleanBinaryOperator-OR)                 #(or  %1 %2)})

(defn eval-exp [exp]
  (a/set-value! (-> exp a/->assignee a/->currentValue)
                (if (a/isa-BooleanUnaryExpression? exp)
                  (not (-> exp a/->operand a/->currentValue a/value))
                  ((op2fn (a/operator exp))
                   (-> exp a/->operand1 a/->currentValue a/value)
                   (-> exp a/->operand2 a/->currentValue a/value)))))

(defn pass-tokens
  ([n] (pass-tokens n nil))
  ([n out-cf]
   (let [in-toks (consume-offers n)]
     (doseq+ [out-cf (if out-cf [out-cf] (a/->outgoing n))]
       (a/->add-offers!
        out-cf (a/create-Offer!
                nil {:offeredTokens in-toks}))))))

(defpolyfn exec-node OpaqueAction [oa]
  (mapc edelete! (consume-offers oa))
  (mapc eval-exp (a/->expressions oa))
  (offer-one-ctrl-token oa))

(defpolyfn exec-node ActivityFinalNode [afn]
  (mapc edelete! (consume-offers afn))
  (mapc #(a/set-running! % false)
        (-> afn a/->activity a/->nodes)))

(defpolyfn exec-node ForkNode [fn]
  (let [in-toks  (consume-offers fn)
        out-cfs  (a/->outgoing fn)
        out-toks (mapv #(a/create-ForkedToken!
                         nil {:baseToken %, :holder fn,
                              :remainingOffersCount (count out-cfs)})
                       in-toks)]
    (doseq+ [out-cf out-cfs]
      (a/->add-offers! out-cf (a/create-Offer!
                               nil {:offeredTokens out-toks})))))

(defpolyfn exec-node JoinNode [jn]
  (pass-tokens jn))

(defpolyfn exec-node MergeNode [mn]
  (pass-tokens mn))

(defpolyfn exec-node DecisionNode [dn]
  (pass-tokens dn (the #(-> % a/->guard a/->currentValue a/value)
                       (a/->outgoing dn))))

(defn execute-activity-diagram [ad]
  (let [activity (the (a/eall-Activities ad))
        trace (a/create-Trace! nil)]
    (a/->set-trace! activity trace)
    (init-variables activity (first (a/eall-Inputs ad)))
    (mapc #(a/set-running! % true) (a/->nodes activity))
    (loop [ens (filter a/isa-InitialNode? (a/->nodes activity))]
      (when (seq ens)
        (doseq+ [node ens]
          (exec-node node)
          (a/->add-executedNodes! trace node))
        (recur (enabled-nodes activity))))
    trace))
