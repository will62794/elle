(System/setProperty "java.awt.headless" "true")
(ns elle.main 
    ;; (:require [clojure.pprint :refer [pprint]]
    ;;         [dom-top.core :refer [loopr real-pmap]]
    ;;         [elle [core :as elle]
    ;;          [core-test :refer [read-history]]
    ;;          [graph :as g]
    ;;          [rw-register :refer :all]
    ;;          [util :refer [map-vals]]]
    ;;         [jepsen [history :as h]
    ;;          [txn :as txn]]
    ;;         [clojure.test :refer :all]
    ;;         [clj-commons.slingshot :refer [try+ throw+]])
  (:require [clojure.java.io :as io]
          [clojure.tools.cli :as cli]
          [clojure.edn :as edn]
          [clojure.string :as str]
          [clojure.data.json :as json]
          [clojure.pprint :refer [pprint]]
          [clojure.tools.logging :refer [info warn]]
          [jepsen.history :as h]
          [elle.list-append :as elle-list-append]
          [elle.rw-register :as elle-rw-register]
          [elle.consistency-model :as elle-consistency-model])
  (:gen-class)
  (:import (java.io PushbackReader)))


(defn read-edn-history
  "Takes a path to file and loads a history from it, in EDN format. If an
  initial [ or ( is present, loads the entire file in one go as a collection,
  expecting it to be a collection of op maps. If the initial character is {,
  loads it piecewise as op maps."
  [filepath]
  (let [h (with-open [r (PushbackReader. (io/reader filepath))]
            (->> (repeatedly #(edn/read {:eof nil} r))
                 (take-while identity)
                 vec))
        ; Handle the presence or absence of an enclosing vector.
        h (if (and (= 1 (count h))
                   (sequential? (first h)))
            (vec (first h))
            h)]
    h))

(defn vl
  [v]
  (if (string? v)
    (keyword v)
    v)
  (if (vector? v) (vl v)))

(defn value-reader [key value]
  (cond
    (or (= key :type) (= key :f)) (keyword value)
    ;; e.g. {:value [[:append :x 1] [:r :y nil]]}
    (and
     (= key :value)
     (vector? value)  (not (empty? value))
     (vector? (first value)) (not (empty? (first value)))
     (string? (ffirst value)))    (map #(assoc %1 0 (keyword (first %1))) value)
    :else value))

(defn read-json-history
  "Takes a path to file and loads a history from it, in JSON format."
  [filepath]
  (json/read-str (slurp filepath)
                 :key-fn keyword
                 :value-fn value-reader))

(defn str->keywords [s]
  (if (empty? s)
    []
    (mapv keyword (str/split s #","))))

(def models
  {
   "rw-register"             elle-rw-register/check
   "list-append"             elle-list-append/check})

(def history-read-fn
  {"edn"        read-edn-history
   "json"       read-json-history})

(def opts
  "tools.cli options"

   ; General options.
  [["-m" "--model MODEL"
    "(General) A name of consistency model for checking."
    :validate [identity
               (str "Must be one of " (str/join ", " (sort (keys models))))]]
   ["-f" "--format FORMAT"
    "(General) Format of file with history. Either 'edn' or 'json'."
    :parse-fn history-read-fn
    :validate [identity
               (str "Must be one of " (str/join ", " (sort (keys history-read-fn))))]]
   ["-v" "--verbose"
    "(General) Enable verbose mode."]
   ["-h" "--help"
    "(General) Print usage."]

   ; Elle-specific options.
   ["-c" "--consistency-models CONSISTENCY-MODELS"
    "(Elle) A collection of consistency models we expect this history to obey."
    :default [:strict-serializable]
    :parse-fn str->keywords]
   ["-a" "--anomalies ANOMALIES"
    "(Elle) A collection of specific anomalies you'd like to look for."
    :default [:G0]
    :parse-fn str->keywords]
   ["-s" "--cycle-search-timeout CYCLE-SEARCH-TIMEOUT"
    "(Elle) Number of ms for searching a single SCC for a cycle."
    :default 1000
    :parse-fn #(Integer/parseInt %)]
   ["-d" "--directory DIRECTORY"
    "(Elle) Where to output files, if desired."
    :default nil]
   ["-p" "--plot-format PLOT-FORMAT"
    "(Elle) Either 'png' or 'svg'."
    :default :svg
    :parse-fn keyword]
   ["-t" "--plot-timeout PLOT-TIMEOUT"
    "(Elle) How many milliseconds will we wait to render a SCC plot?"
    :default 5000
    :parse-fn #(Integer/parseInt %)]
   ["-b" "--max-plot-bytes MAX-PLOT-BYTES"
    "(Elle) Maximum size of a cycle graph (in bytes of DOT)."
    :default 65536
    :parse-fn #(Integer/parseInt %)]

   ; Jepsen-specific options.
   ["-g" "--group-size GROUP-SIZE"
    "(Jepsen) A group size."
    :default 0
    :parse-fn #(Integer/parseInt %)]
   ["-n" "--allow-negative-balances"
    "(Jepsen) Allow negative balances in a bank model."
    :default false]
   ["-lk" "--sequential-keys"
    "(Jepsen) lin keys."
    :default false]

   ; Knossos-specific options.
   ; None.
   ])

(defn usage [options-summary]
  (->> ["elle-cli - command-line transactional safety checker."
        ""
        "Usage: elle-cli -m model [options] files"
        ""
        "Supported models:"
        "  rw-register - an checker for write-read registers."
        "  list-append - an checker for append and read histories."
        "  bank - a checker for bank histories."
        "  counter - a checker for counter histories."
        "  set - a checker for a set histories."
        "  set-full - a checker for a set histories."
        "  long-fork - a checker for an anomaly in parallel snapshot isolation."
        "  cas-register - a checker for CAS (Compare-And-Set) registers."
        "  mutex - a checker for a mutex histories."
        "  comments - a custom checker for a comments histories (experimental)."
        "  sequential - a custom checker for sequential histories (experimental)."
        ""
        "Options:"
        options-summary
        ""]
       (str/join \newline)))

(defn check-history
  "Check a specified history according to model specified by model name"
  [model-name history options]

  (let [checker-fn (get models model-name)]
    (case model-name
       ; Operations in a histories passed to a Knossos additionally normalized,
       ; see src/knossos/cli.clj:read-history.
      "list-append" (checker-fn options history)
      "rw-register" (checker-fn (assoc options :explicit-version-order-keys? true) history)
      )
    )
  )

(defn read-fn-by-extension
  "Take a path to file and returns a function for reading that file."
  [filepath]
  (let [ext (second (re-find #"\.([a-zA-Z0-9]+)$" filepath))]
    (get history-read-fn ext)))

(defn lazy-contains? [coll key]
  (boolean (some #(= % key) coll)))




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



(defn -test1
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
    ;;    (pprint (check {:analyzer wr-graph :explicit-version-order-keys? true} h2))
    )
  
  )


(defn -main
  [& args]
  (try
    (let [{:keys [options arguments summary errors]} (cli/parse-opts args opts)
          model-name (:model options)
          read-history (:format options)
          results (atom (hash-map))
          help (:help options)]
      (when-not (empty? errors)
        (doseq [e errors]
          (println e))
        (System/exit 1))

      (if (or (nil? model-name) (true? help)) ((println (usage summary) "william schultz")
                                               (System/exit 0)))

      (doseq [filepath arguments]
        (if (not (.exists (io/as-file filepath)))
          (throw (Exception. (format "File not found: %s" filepath))))

        (let [read-history  (or read-history (read-fn-by-extension filepath))
              history       (h/history (read-history filepath))
              analysis      (check-history model-name history options)
              validness     (:valid? analysis)]

          (swap! results assoc filepath validness)

          (if (true? (:verbose options))
            (json/pprint analysis)
            (println filepath "\t" validness))))

      (System/exit ({true 1 false 0} (lazy-contains? (vals @results) false))))

    (catch Throwable t
      (println)
      (.printStackTrace t)
      (System/exit 255))))



