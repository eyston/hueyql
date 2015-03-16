

(defn tuple
  [& schema-name-pairs]
  (->> schema-name-pairs
    (mapv (fn [pair]
            (if (map? pair)
              (apply s/optional (-> pair seq first))
              (apply s/one pair))))))
