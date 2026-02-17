(ns elle.main 
    (:require [clojure.pprint :refer [pprint]]
            [dom-top.core :refer [loopr real-pmap]]
            [elle [core :as elle]
             [core-test :refer [read-history]]
             [graph :as g]
             [rw-register :refer :all]
             [util :refer [map-vals]]]
            [jepsen [history :as h]
             [txn :as txn]]
            [clojure.test :refer :all]
            [clj-commons.slingshot :refer [try+ throw+]])
  (:gen-class))

(defn op
  "Generates an operation from a string language like so:

  wx1       set x = 1
  ry1       read y = 1
  wx1wx2    set x=1, x=2"
  ([string version]
   (let [[txn mop] (reduce (fn [[txn [f k v :as mop]] c]
                             (case c
                               \w [(conj txn mop) [:w]]
                               \r [(conj txn mop) [:r]]
                               \x [txn (conj mop :x)]
                               \y [txn (conj mop :y)]
                               \z [txn (conj mop :z)]
                               \a [txn (conj mop :a)]
                               \b [txn (conj mop :b)]
                               \c [txn (conj mop :c)]
                               \d [txn (conj mop :d)]
                               (let [e (if (= \_ c)
                                         nil
                                         (Long/parseLong (str c)))]
                                 [txn [f k e]])))
                           [[] nil]
                           string)
         txn (-> txn
                 (subvec 1)
                 (conj mop))]
     {:process 0, :type :ok, :value txn :version version}))
  ([process type string version]
   (assoc (op string version) :process process :type type :version version)))



(defn -main
  "Entry point for the application."
  [& args]
  (println "Hello World xxx!")
;;   (pprint (version-graphs {:explicit-version-order-keys? true} (h/history [
;;                                          (op "wx1wy5" 0)
;;                                          (op "rx1wy6" 1)
;;                                          (op "ry6" 2)
;;                                          (op "wx9wy8" 3)
;;                                          (op "ry8" 4)])))
  (pprint (h/history [(op 0 :invoke "wx0wy0" 0)
                       (op 0 :ok "wx0wy0" 0)]))
  (let [h1
        (h/history [(op 0 :invoke "wx0wy0" 0)
                    (op 0 :ok "wx0wy0" 0)
                    (op 1 :invoke "rx_wy6" 1)
                    (op 2 :invoke "ry_wy7" 2)
                    (op 1 :ok "rx0wy6" 1)
                    (op 2 :ok "ry0wy7" 2)])   
        h2
        (h/history [(op 0 :invoke "wa0wb0wc0wd0" 0)
                    (op 0 :ok "wa0wb0wc0wd0" 0)
                    (op 1 :invoke "rb_wd6" 4)
                    (op 2 :invoke "wb7" 1)
                    (op 2 :ok "wb7" 1)
                    (op 3 :invoke "rb_rc_wa8" 3)
                    (op 4 :invoke "wc3wd4" 2)
                    (op 4 :ok "wc3wd4" 2)
                    (op 3 :ok "rb7rc0wa8" 3)
                    (op 1 :ok "rb0wd6" 4)]) 
        ]
       (pprint (check {:analyzer wr-graph :explicit-version-order-keys? true} h2))
    )
  
  )
