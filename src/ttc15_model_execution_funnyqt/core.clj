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

(defn init-variables [activity]
  ;; TODO: Implement me!
  )

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
                 ((if (a/isa-MergeNode? n)
                    exists?
                    forall?)
                  #(seq (a/->offers %))
                  (a/->incoming n))))
          (a/->nodes activity)))

(defn offer-one-ctrl-token [node]
  (let [ctrl-t (a/create-ControlToken! nil)]
    (doseq [out-cf (a/->outgoing node)]
      (let [offer  (a/create-Offer! nil)]
        (a/->add-heldTokens! node ctrl-t)
        (a/->add-offers! out-cf offer)
        (a/->add-offeredTokens! offer ctrl-t)))))

(pf/defpolyfn exec-node InitialNode [i]
  (offer-one-ctrl-token i))

(defn consume-offers [node]
  (let [in-cfs     (a/->incoming node)
        offers     (mapcat a/->offers in-cfs)
        tokens     (mapcat a/->offeredTokens offers)
        [ctrl-toks fork-toks] ((juxt filter remove) a/isa-ControlToken? tokens)]
    (doseq [x (concat offers ctrl-toks)]
      (edelete! x))
    (doseq [ft fork-toks]
      (let [roc (a/remainingOffersCount ft)]
        (when-let [bt (a/->baseToken ft)]
          (edelete! bt))
        (if (<= roc 1)
          (edelete! ft)
          (a/set-remainingOffersCount! ft (dec (a/remainingOffersCount ft))))))
    tokens))

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

(defn var-val [var]
  (if-let [cv (a/->currentValue var)]
    (a/value cv)
    (-> var a/->initialValue a/value)))

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
                  (not (var-val (a/->operand exp)))
                  (((-> exp eclass bin-exp2op) (a/operator exp))
                   (var-val (a/->operand1 exp))
                   (var-val (a/->operand2 exp))))))

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
                              :remainingOffersCount (count out-cfs)})
                       in-toks)]
    (doseq [out-cf out-cfs]
      (a/->add-offers! out-cf (a/create-Offer!
                               nil {:offeredTokens out-toks})))))

(defn ^:private pass-tokens
  ([n] (pass-tokens n nil))
  ([n out-cf]
   (let [in-toks (consume-offers n)]
     (a/->set-heldTokens! n in-toks)
     (doseq [out-cf (if out-cf
                      [out-cf]
                      (a/->outgoing n))]
       (a/->add-offers!
        out-cf (a/create-Offer!
                nil {:offeredTokens in-toks}))))))

(pf/defpolyfn exec-node JoinNode [jn]
  (pass-tokens jn))

(pf/defpolyfn exec-node MergeNode [mn]
  (pass-tokens mn))

(pf/defpolyfn exec-node DecisionNode [dn]
  (pass-tokens dn (the #(var-val (a/->guard %))
                       (a/->outgoing dn))))

(defn execute-activity-diagram [ad]
  (let [activity (first (a/eall-Activities ad))
        trace (a/create-Trace! nil)]
    (a/->set-trace! activity trace)
    (init-variables activity)
    (init-activity activity)
    (loop [ens (filter a/isa-InitialNode?
                       (a/->nodes activity))]
      #_(println "ENs:" ens)
      (when (seq ens)
        (let [node (first ens)]
          (exec-node node)
          (a/->add-executedNodes! trace node)
          (recur (enabled-nodes activity)))))))
