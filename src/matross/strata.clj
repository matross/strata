(ns matross.strata
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

(deftype Strata [strata-l strata-v]
  Associative
  (containsKey [this k]
    (some-stratum-contains? strata-l k))

  (entryAt [this k]
    (if-let [m (some-stratum-contains? strata-l k)]
      (MapEntry. k (get m k))))

  ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found]
    (if-let [m (some-stratum-contains? strata-l k)]
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
  (toString [this] (->> strata-v
                        flatten-strata
                        str))

  Map
  (keySet [this]
    (->> strata-l
         (mapcat keys)
         set))

  Seqable
  (seq [this]
    (map (fn [k] (. this entryAt k)) (. this keySet)))

  IPersistentCollection
  (count [this] (->> strata-l
                     flatten-strata
                     count))

  (empty [this] (Strata. '() []))
  (equiv [this o]
    (= (flatten-strata strata-l) o))

  (cons [this o]
    (let [sid (or (stratum-id o) (str (java.util.UUID/randomUUID)))
          s (stratum sid  (conj {} o))]
      (Strata. (conj strata-l s) (conj strata-v s))))

  IPersistentMap
  (assoc [this k v]
    (. this cons [k v]))

  (assocEx [this k v]
    (if (. this containsKey k)
      (throw (IllegalArgumentException. (str "Already contains key: " k)))
      (. this assoc k v)))

  (without [this k]
    (let [new-strata-l (map #(dissoc % k) strata-l)]
      (Strata. new-strata-l (vector (reverse new-strata-l)))))

  Iterable
  (iterator [this] (SeqIterator. (. this seq))))

(defn strata
  ([] (Strata. '() [])))
