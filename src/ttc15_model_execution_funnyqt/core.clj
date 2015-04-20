(ns ttc15-model-execution-funnyqt.core
  (:require [funnyqt.generic :refer [adj adjs]]
            [funnyqt.emf :refer :all]
            [funnyqt.query :refer [forall? exists? the]]
            [funnyqt.polyfns :as pf]))

;;* Load metamodel and generate accessors

(load-ecore-resource "activitydiagram.ecore")
(generate-ecore-model-functions "activitydiagram.ecore"
                                ttc15-model-execution-funnyqt.ad
                                a)

;;* Solution

(defn init-variables [activity input]
  (doseq [lv (a/->locals activity)]
    (when-let [init-value (a/->initialValue lv)]
      (a/->set-currentValue! lv init-value)))
  (doseq [iv (and input (a/->inputValues input))]
    (when-let [val (a/->value iv)]
      (a/->set-currentValue! (a/->variable iv) val))))

(pf/declare-polyfn exec-node [node])

(defn set-nodes-running [activity val]
  (doseq [node (a/->nodes activity)]
    (a/set-running! node val)))

(defn init-activity [activity]
  (set-nodes-running activity true))

(defn enabled-nodes [activity]
  (filter (fn [n]
            (and (a/running? n)
                 (not (a/isa-InitialNode? n))
                 ((if (a/isa-MergeNode? n) exists? forall?)
                  #(seq (a/->offers %))
                  (a/->incoming n))))
          (a/->nodes activity)))

(defn offer-one-ctrl-token [node]
  (let [ctrl-t (a/create-ControlToken! nil)]
    (doseq [out-cf (a/->outgoing node)]
      (let [offer  (a/create-Offer! nil {:offeredTokens [ctrl-t]})]
        (a/->add-heldTokens! node ctrl-t)
        (a/->add-offers! out-cf offer)))))

(pf/defpolyfn exec-node InitialNode [i]
  (offer-one-ctrl-token i))

(defn consume-offers [node]
  (let [offers     (mapcat a/->offers (a/->incoming node))
        tokens     (mapcat a/->offeredTokens offers)
        [ctrl-toks fork-toks] ((juxt filter remove) a/isa-ControlToken? tokens)]
    (doseq [o offers] (edelete! o))
    (doseq [c ctrl-toks] (a/->set-holder! c nil))
    (doseq [ft fork-toks]
      (a/->set-holder! (a/->baseToken ft) nil)
      (a/set-remainingOffersCount! ft (dec (a/remainingOffersCount ft)))
      (when (zero? (a/remainingOffersCount ft))
        (edelete! ft)))
    (concat ctrl-toks (remove #(zero? (a/remainingOffersCount %)) fork-toks))))

(def bin-exp2op {(eclassifier 'IntegerCalculationExpression)
                 {(a/eenum-IntegerCalculationOperator-ADD)     +
                  (a/eenum-IntegerCalculationOperator-SUBRACT) -}
                 (eclassifier 'IntegerComparisonExpression)
                 {(a/eenum-IntegerComparisonOperator-SMALLER)        <
                  (a/eenum-IntegerComparisonOperator-SMALLER_EQUALS) <=
                  (a/eenum-IntegerComparisonOperator-EQUALS)         =
                  (a/eenum-IntegerComparisonOperator-GREATER_EQUALS) >=
                  (a/eenum-IntegerComparisonOperator-GREATER)        >}
                 (eclassifier 'BooleanBinaryExpression)
                 {(a/eenum-BooleanBinaryOperator-AND) #(and %1 %2)
                  (a/eenum-BooleanBinaryOperator-OR)  #(or  %1 %2)}})

(defn eval-exp [exp]
  (a/set-value! (let [a (a/->assignee exp)]
                  (if-let [cv (a/->currentValue a)]
                    cv
                    (let [ncv (if (a/isa-IntegerCalculationExpression? exp)
                                (a/create-IntegerValue! nil)
                                (a/create-BooleanValue! nil))]
                      (a/->set-currentValue! a ncv)
                      ncv)))
                (if (a/isa-BooleanUnaryExpression? exp)
                  (not (-> exp a/->operand a/->currentValue a/value))
                  (try
                    (((-> exp eclass bin-exp2op) (a/operator exp))
                     (-> exp a/->operand1 a/->currentValue a/value)
                     (-> exp a/->operand2 a/->currentValue a/value))
                    (catch Exception e
                      (funnyqt.visualization/print-model (eresource exp) :gtk)
                      (throw e))))))

(defn ^:private pass-tokens
  ([n] (pass-tokens n nil))
  ([n out-cf]
   (let [in-toks (consume-offers n)]
     (a/->set-heldTokens! n in-toks)
     (doseq [out-cf (if out-cf [out-cf] (a/->outgoing n))]
       (a/->add-offers!
        out-cf (a/create-Offer!
                nil {:offeredTokens in-toks}))))))

(pf/defpolyfn exec-node OpaqueAction [oa]
  (consume-offers oa)
  (doseq [exp (a/->expressions oa)]
    (eval-exp exp))
  (offer-one-ctrl-token oa))

(pf/defpolyfn exec-node ActivityFinalNode [afn]
  (consume-offers afn)
  (set-nodes-running (a/->activity afn) false))

(pf/defpolyfn exec-node ForkNode [fn]
  (let [in-toks (consume-offers fn)
        out-cfs (a/->outgoing fn)
        out-toks (mapv #(a/create-ForkedToken!
                         nil {:baseToken %
                              :holder fn
                              :remainingOffersCount (count out-cfs)})
                       in-toks)]
    (doseq [out-cf out-cfs]
      (a/->add-offers! out-cf (a/create-Offer!
                               nil {:offeredTokens out-toks})))))

(pf/defpolyfn exec-node JoinNode [jn]
  (pass-tokens jn))

(pf/defpolyfn exec-node MergeNode [mn]
  (pass-tokens mn))

(pf/defpolyfn exec-node DecisionNode [dn]
  (pass-tokens dn (the #(-> % a/->guard a/->currentValue a/value)
                       (a/->outgoing dn))))

(defn execute-activity-diagram [ad]
  (let [activity (first (a/eall-Activities ad))
        trace (a/create-Trace! nil)]
    (a/->set-trace! activity trace)
    (init-variables activity (first (a/eall-Inputs ad)))
    (init-activity activity)
    (loop [ens (filter a/isa-InitialNode?
                       (a/->nodes activity))]
      (if (seq ens)
        (let [node (first ens)]
          (exec-node node)
          (a/->add-executedNodes! trace node)
          (recur (enabled-nodes activity)))
        trace))))
