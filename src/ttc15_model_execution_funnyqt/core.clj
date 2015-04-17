(ns ttc15-model-execution-funnyqt.core
  (:require [funnyqt.generic :refer [adjs]]
            [funnyqt.emf :refer :all]
            [funnyqt.query :refer [forall? the]]
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

(defn executable-nodes [activity]
  (filter (fn [n]
            (and (a/running? n)
                 (not (a/isa-InitialNode? n))
                 (forall? #(seq (a/->offers %))
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

(pf/declare-polyfn eval-exp [exp])

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
  (pass-tokens dn (the #(a/value (adjs % :guard :currentValue))
                       (a/->outgoing dn))))

(defn execute-activity-diagram [ad]
  (let [activity (first (a/eall-Activities ad))
        trace (a/create-Trace! nil)]
    (a/->set-trace! activity trace)
    (init-variables activity)
    (init-activity activity)
    (loop [execable-nodes (filter a/isa-InitialNode?
                                  (a/->nodes activity))]
      (when (seq execable-nodes)
        (let [node (first execable-nodes)]
          (exec-node node)
          (a/->add-executedNodes! trace node)
          (recur (executable-nodes activity)))))))
