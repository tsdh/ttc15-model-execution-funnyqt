(ns ttc15-model-execution-funnyqt.core-test
  (:require [clojure.test :refer :all]
            [funnyqt.query :refer [the]]
            [funnyqt.emf :refer :all]
            [funnyqt.visualization :as viz]
            [ttc15-model-execution-funnyqt.core :refer :all]
            [ttc15-model-execution-funnyqt.ad :as a]))


(def tm1 (load-resource "test/model/test1.xmi"))
(def tm2 (load-resource "test/model/test2.xmi"))
(def tm3 (load-resource "test/model/test3.xmi"))
(def tm4 (load-resource "test/model/test4.xmi"))
(def tm5 (doto (new-resource-set)
           (get-resource "test/model/test5.xmi" true)
           (get-resource "test/model/test5_input.xmi" true)))
