(ns ttc15-model-execution-funnyqt.core-test
  (:require [clojure.test :refer :all]
            [funnyqt.emf :refer :all]
            [funnyqt.visualization :as viz]
            [ttc15-model-execution-funnyqt.core :refer :all]
            [ttc15-model-execution-funnyqt.ad :as a]))


(def tm1 (load-resource "test/model/test1.xmi"))
(def tm2 (load-resource "test/model/test2.xmi"))
