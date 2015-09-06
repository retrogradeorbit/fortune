(ns fortune.test
  (:require [cljs.test :refer-macros [run-all-tests]]
            [fortune.core-test]))

(enable-console-print!)

(defn ^:export run
  []
  (run-all-tests #"fortune.*-test"))
