(ns ttc15-model-execution-funnyqt.core
  (:require [funnyqt.emf :refer :all]
            [funnyqt.query :refer [forall? exists? the]]
            [funnyqt.polyfns :refer :all]
            [funnyqt.utils :refer [mapc]]))

(load-ecore-resource "activitydiagram.ecore")
(generate-ecore-model-functions "activitydiagram.ecore" ttc15-model-execution-funnyqt.ad a)

(defn init-variables [activity input]
  (doseq [lv (a/->locals activity)]
    (when-let [init-value (a/->initialValue lv)]
      (a/->set-currentValue! lv init-value)))
  (doseq [iv (and input (a/->inputValues input))]
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
        tokens    (mapcat a/->offeredTokens offers)
        ctrl-toks (filter a/isa-ControlToken? tokens)
        fork-toks (filter a/isa-ForkedToken? tokens)]
    (doseq [ct ctrl-toks]
      (a/->set-holder! ct nil))
    (doseq [ft fork-toks]
      (when-let [bt (a/->baseToken ft)]
        (a/->set-holder! bt nil))
      (a/set-remainingOffersCount! ft (dec (a/remainingOffersCount ft)))
      (when (zero? (a/remainingOffersCount ft))
        (a/->set-holder! ft nil)))
    (mapc edelete! offers)
    tokens))

(def op2fn {(a/enum-IntegerCalculationOperator-ADD)           +
            (a/enum-IntegerCalculationOperator-SUBRACT)       -
            (a/enum-IntegerComparisonOperator-SMALLER)        <
            (a/enum-IntegerComparisonOperator-SMALLER_EQUALS) <=
            (a/enum-IntegerComparisonOperator-EQUALS)         =
            (a/enum-IntegerComparisonOperator-GREATER_EQUALS) >=
            (a/enum-IntegerComparisonOperator-GREATER)        >
            (a/enum-BooleanBinaryOperator-AND)                #(and %1 %2)
            (a/enum-BooleanBinaryOperator-OR)                 #(or  %1 %2)})

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
     (a/->set-heldTokens! n in-toks)
     (doseq [out-cf (if out-cf [out-cf] (a/->outgoing n))]
       (a/->add-offers!
        out-cf (a/create-Offer!
                nil {:offeredTokens in-toks}))))))

(defpolyfn exec-node OpaqueAction [oa]
  (consume-offers oa)
  (mapc eval-exp (a/->expressions oa))
  (offer-one-ctrl-token oa))

(defpolyfn exec-node ActivityFinalNode [afn]
  (consume-offers afn)
  (mapc #(a/set-running! % false)
        (-> afn a/->activity a/->nodes)))

(defpolyfn exec-node ForkNode [fn]
  (let [in-toks  (consume-offers fn)
        out-cfs  (a/->outgoing fn)
        out-toks (mapv #(a/create-ForkedToken!
                         nil {:baseToken %, :holder fn,
                              :remainingOffersCount (count out-cfs)})
                       in-toks)]
    (a/->set-heldTokens! fn in-toks)
    (doseq [out-cf out-cfs]
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
  (let [activity (the (a/all-Activities ad))
        trace (a/create-Trace! nil)]
    (a/->set-trace! activity trace)
    (init-variables activity (first (a/all-Inputs ad)))
    (mapc #(a/set-running! % true) (a/->nodes activity))
    (loop [en (first (filter a/isa-InitialNode? (a/->nodes activity)))]
      (when en
        (exec-node en)
        (a/->add-executedNodes! trace en)
        (recur (first (enabled-nodes activity)))))
    trace))
