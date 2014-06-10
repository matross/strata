(ns matross.strata
  (:require [clojure.pprint]
            [potemkin :refer [def-map-type]])
  )

(defn stratum
  "Append some identifying information to a value so that
  it can be easily referenced for debugging"
  [id m]
  (vary-meta m assoc :stratum-id id))

(defn stratum-id [m]
  (:stratum-id (meta m)))

(defn some-stratum-contains? [s k]
  (some #(if (contains? % k) %) s))

(defn flatten-strata [s]
  (apply merge s))

(defprotocol IStrata
  (enable-debug [_])
  (disable-debug [_]))

(def-map-type Strata [strata map-fn debug]
  (assoc [_ k v]
         (let [sid (or (stratum-id v) (str (java.util.UUID/randomUUID)))
               s (stratum sid {k v})]
           (Strata. (conj strata s) map-fn debug)))

  (dissoc[_ k]
            (let [new-strata (map-fn #(dissoc % k) strata)]
              (Strata. new-strata map-fn debug)))

  (keys [_]
        (->> strata
             (mapcat keys)
             set))

  (get [this k not-found]
       (if-let [m (some-stratum-contains? strata k)]
         (do
           (if debug
             (println (str "Found key `" k "` in: " (pr-str (stratum-id m)))))
           (get m k))
         (do
           (if debug
             (println (str "Did not find key `" k "`, using not-found value of: " not-found)))
           not-found)))

  IStrata
  (enable-debug [_] (Strata. strata map-fn true))
  (disable-debug [_] (Strata. strata map-fn false)))

(defn strata-fifo
  ([] (Strata. [] mapv false)))

(defn strata-lifo
  ([] (Strata. '() map false)))
