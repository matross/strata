(ns matross.strata
  (:require [clojure.pprint])
  (:import clojure.lang.Associative
           clojure.lang.IFn
           clojure.lang.ILookup
           clojure.lang.IPersistentMap
           clojure.lang.IPersistentStack
           clojure.lang.Seqable
           clojure.lang.MapEntry
           clojure.lang.IPersistentCollection
           clojure.lang.IPersistentVector
           clojure.lang.SeqIterator
           java.util.Map))

(def debug (atom false))

(defn enable-debug [] (reset! debug true))
(defn disable-debug [] (reset! debug false))

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

(deftype Strata [strata map-fn]
  Associative
  (containsKey [this k]
    (if (some-stratum-contains? strata k)
      true
      false))

  (entryAt [this k]
    (if-let [m (some-stratum-contains? strata k)]
      (MapEntry. k (get m k))))

  ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found]
    (if-let [m (some-stratum-contains? strata k)]
      (do
        (if @debug
          (println (str "Found key `" k "` in: " (pr-str (stratum-id m)))))
        (get m k))
      (do
        (if @debug
          (println (str "Did not find key `" k "`, using not-found value of: " not-found)))
        not-found)))

  IFn
  (invoke [this k] (. this valAt k))
  (invoke [this k not-found] (. this valAt k not-found))
  (toString [this] (into {} (. this seq)))

  Map
  (keySet [this]
    (->> strata
         (mapcat keys)
         set))

  Seqable
  (seq [this]
    ; vectors aren't ISeq so we must use `map`, ignoring fifo/lifo behavior
    (map (fn [k] (. this entryAt k)) (. this keySet)))

  IPersistentCollection
  (count [this] (->> strata
                     flatten-strata
                     count))

  (empty [this] (Strata. (empty strata) map-fn))
  (equiv [this o]
    (= (flatten-strata strata) o))

  (cons [this o]
    (let [sid (or (stratum-id o) (str (java.util.UUID/randomUUID)))
          s (stratum sid  (conj {} o))]
      (Strata. (conj strata s) map-fn)))

  IPersistentMap
  (assoc [this k v]
    (. this cons [k v]))

  (assocEx [this k v]
    (if (. this containsKey k)
      (throw (IllegalArgumentException. (str "Already contains key: " k)))
      (. this assoc k v)))

  (without [this k]
    (let [new-strata (map-fn #(dissoc % k) strata)]
      (Strata. new-strata map-fn)))

  Iterable
  (iterator [this] (SeqIterator. (. this seq))))

(defn strata-fifo
  ([] (Strata. [] mapv)))

(defn strata-lifo
  ([] (Strata. '() map)))
