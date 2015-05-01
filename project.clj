(defproject ttc15-model-execution-funnyqt "0.1.0-SNAPSHOT"
  :description "The FunnyQT solution to the TTC15 ModelExecution case."
  :url "https://github.com/tsdh/ttc15-model-execution-funnyqt"
  :license {:name "GNU General Public License, Version 3 (or later)"
            :url "http://www.gnu.org/licenses/gpl.html"
            :distribution :repo}
  :dependencies [[funnyqt "0.47.5"]]
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ^:replace ["-Xmx2G"])
