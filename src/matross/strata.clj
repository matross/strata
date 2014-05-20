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

(defrecord Stratum [id value])

(defprotocol IStrata
  (add-stratum [this id m] [this s]))

(deftype Strata [strata-l strata-v]
  IStrata
  (add-stratum [this id m] (. this add-stratum (Stratum. id m)))
  (add-stratum [this s] (Strata. (conj strata-l s) (conj strata-v s)))

  Associative
  (containsKey [this k]
    (some #(contains? (:value %) k) strata-l))

  (entryAt [this k]
    (if-let [s (some #(if (contains? (:value %) k) %) strata-l)]
      (MapEntry. k (k (:value s)))))

  ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k not-found]
    (if-let [s (some #(if (contains? (:value %) k) %) strata-l)]
      (k (:value s))
      not-found))

  IFn
  (invoke [this k] (. this valAt k))
  (invoke [this k not-found] (. this valAt k not-found))
  #_(toString [this] (->> strata-v
                        (map :value)
                        (apply merge)
                        str))

  Map
  (keySet [this]
    (->> strata-l
         (map :value)
         (mapcat keys)
         set))

  Seqable
  (seq [this]
    (map (fn [k] (. this entryAt k)) (. this keySet)))

  IPersistentCollection
  (count [this] (->> strata-l
                     (map :value)
                     (apply merge)
                     count))

  (empty [this] (Strata. '() []))
  (equiv [this o]
    (= (->> strata-l
            (map :value)
            (apply merge)) o))

  (cons [this o]
    (let [s (Stratum. (str (java.util.UUID/randomUUID)) (conj {} o))]
      (. this add-stratum s)))

  IPersistentMap
  (assoc [this k v]
    (. this cons [k v]))

  (assocEx [this k v]
    (if (. this containsKey k)
      (throw (IllegalArgumentException. (str "Already contains key: " k)))
      (. this assoc k v)))

  (without [this k]
    (let [new-strata-l
          (map #(Stratum. (:id %) (dissoc (:value %) k)) strata-l)]
      (Strata. new-strata-l (vector (reverse new-strata-l)))))

  Iterable
  (iterator [this] (SeqIterator. (. this seq))))

(defn strata
  ([] (Strata. '() [])))