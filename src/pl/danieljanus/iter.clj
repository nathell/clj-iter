(ns pl.danieljanus.iter
  (:use clojure.contrib.test-is))

(defn- mapmap
  "With one argument, converts a seq consisting of key/value pairs to
a map. With two arguments, transforms a map or a seq, mapping each key
to result of calling TRANSFORM on the respective key/value pair."
  ([l] (mapmap l second))
  ([l transform]
     (loop [map {}
            kvs (seq l)]
       (if kvs
         (let [fst (first kvs)]
           (recur (assoc map (first fst) (transform fst))
                  (next kvs)))
         map))))

(defmacro update-map [map key val]
  `(let [~(symbol (name key)) (~map ~key)]
     (assoc ~map ~key ~val)))

(defn- parse-destructuring-cond-clause [coll [pattern value]]
  (let [numbered-entries (map vector pattern (iterate inc 0))
        keywords (filter #(keyword? (first %)) numbered-entries)
        condition (map (fn [[x n]] `(let [x# (nth ~coll ~n nil)] (and (or (symbol? x#) (keyword? x#))
                                                                  (= (name x#) ~(name x)))))
                       keywords)
        vars (filter #(symbol? (first %)) numbered-entries)
        assignments (map (fn [[x n]] `(~x (nth ~coll ~n))) vars)]
    `((and ~@condition) (let [~@(apply concat assignments)] ~value))))

(defmacro destructuring-cond [coll & clauses]
  `(cond
     ~@(apply concat (map #(parse-destructuring-cond-clause coll %) (partition 2 clauses)))))

(defn- parse-clause [state clause]
  (destructuring-cond clause
    (:for var :in coll)
    (update-map state :colls (cons {:var var, :coll coll} colls))
    (:for var :on coll)
    (update-map state :vars (cons {:var var, :initially coll, :then `(next ~var), :stop `(empty? ~var)} vars))
    (:for var :from from :to to :by by)
    (update-map state :vars (cons {:var var, :initially from, :then `(+ ~var ~by), :stop `(> ~var ~to)} vars))
    (:for var :from from :to to)
    (update-map state :vars (cons {:var var, :initially from, :then `(inc ~var), :stop `(> ~var ~to)} vars))
    (:for var :from from :below below :by by)
    (update-map state :vars (cons {:var var, :initially from, :then `(+ ~var ~by), :stop `(>= ~var ~below)} vars))
    (:for var :from from :below below)
    (update-map state :vars (cons {:var var, :initially from, :then `(inc ~var), :stop `(>= ~var ~below)} vars))
    (:for var :downfrom from :to to)
    (update-map state :vars (cons {:var var, :initially from, :then `(dec ~var), :stop `(< ~var ~to)} vars))
    (:for var :from from :by by)
    (update-map state :vars (cons {:var var, :initially from, :then `(+ ~var ~by)} vars))
    (:for var :from from)
    (update-map state :vars (cons {:var var, :initially from, :then `(inc ~var)} vars))
    (:for var :downfrom from)
    (update-map state :vars (cons {:var var, :initially from, :then `(dec ~var)} vars))
    (:for var :initially initially :then then :stop stop)
    (update-map state :vars (cons {:var var, :initially initially, :then then, :stop stop} vars))
    (:for var :initially initially :then then)
    (update-map state :vars (cons {:var var, :initially initially, :then then} vars))
    (:for var := expr)
    (update-map state :letvars (cons [var expr] letvars))
    (:stop-if condition)
    (update-map state :stop (cons condition stop))
    (:repeat times)
    (let [v (gensym)]
      (update-map state :vars (cons {:var v, :initially 0, :then `(inc ~v), :stop `(= ~v ~times)} vars)))
    (:collect value :if condition)
    (update-map (update-map state :collect value) :collect-if condition)
    (:collect value)
    (update-map (update-map state :collect value) :collect-if 'true)
    (:sum value :if condition)
    (update-map state :accum [`(if ~condition ~value 0) '+ 0])
    (:sum value)
    (update-map state :accum [value '+ 0])
    (:multiply value :if condition)
    (update-map state :accum [`(if ~condition ~value 1) '* 1])
    (:multiply value)
    (update-map state :accum [value '* 1])
    ()
    (update-map state :do (cons clause do))))

(defmacro iter [& clauses]
  (let [parsed (reduce parse-clause {:vars [], :letvars [], :colls [], :do []} clauses)
        collvar-names (mapmap (map #(vector % (gensym)) (map :var (:colls parsed))))
        collvars (map (fn [x] {:var (collvar-names (:var x))
                               :initially (:coll x)
                               :then `(next ~(collvar-names (:var x)))
                               :stop `(empty? ~(collvar-names (:var x)))})
                      (:colls parsed))
        collected (gensym "collected")
        loopvars (concat collvars (:vars parsed))]
    `(loop [~@(cond
                (:collect parsed) `(~collected nil)
                (:accum parsed) `(~collected ~(nth (:accum parsed) 2))
                true `())
            ~@(reduce concat (map #(list (:var %) (:initially %)) loopvars))]
       (let [~@(reduce concat
                       (concat
                        (map (fn [[k v]] (list k `(first ~v))) collvar-names)
                        (:letvars parsed)))]
         (if (or ~@(remove not (concat (map :stop loopvars) (:stop parsed))))
           ~(cond
              (:collect parsed) `(reverse ~collected)
              (:accum parsed) collected)
           (do
             ~@(reverse (:do parsed))
             (recur ~@(cond
                        (:collect parsed) `((if ~(:collect-if parsed) (cons ~(:collect parsed) ~collected) ~collected))
                        (:accum parsed) (let [[value combine _] (:accum parsed)]
                                             `((~combine ~collected ~value)))
                        true `())
                    ~@(map :then loopvars))))))))

(deftest test-iter
  (is (= (iter (for x from 1)
               (for y in [31 41 59])
               (collect (+ x y)))
         (seq [32 43 62])))
  (is (= (iter (for x from 1 to 3)
               (collect x))
         (seq [1 2 3])))
  (is (= (iter (for x from 1 to 5 by 2)
               (collect x))
         (seq [1 3 5])))
  (is (= (set (iter (for [k v] in {:foo 1, :bar 42})
                    (collect k)))
         #{:foo, :bar}))
  (is (= (with-out-str (iter (for x from 1 to 5 by 2)
                             (print x)))
         "135"))
  ;; Note: Due to the way clj-iter works, the following will
  ;; return a Fibonacci sequence instead of a sequence of powers of 2
  ;; like it would in CL.  In clj-iter, all the FOR bindings are
  ;; established simultaneously upon each iteration.
  (is (= (iter (for a from 0 to 10)
               (for x initially 0 then y)
               (for y initially 1 then (+ x y))
               (collect x))
         (seq [0 1 1 2 3 5 8 13 21 34 55])))
  (is (= (iter (for x from 0 to 10)
               (sum x))
         55))
  (is (= (iter (for x from 0 to 10)
               (sum x if (even? x)))
         30))
  (is (= (iter (for x from 0 below 10)
               (sum x if (even? x)))
         20))
  (is (= (iter (repeat 4) (multiply 3))
         81))
  (is (= (iter (for x from 1 to 10)
               (multiply x if (even? x)))
         (* 2 4 6 8 10)))
  (is (= (iter (for x from 1 to 5)
               (for y = (inc x))
               (collect (+ x y)))
         (seq [3 5 7 9 11])))
  (is (= (iter (for x from 1 to 5)
               (for y in [1 2 3])
               (collect [x y]))
         (seq [[1 1] [2 2] [3 3]])))
  (is (nil? (iter (for x in [1 2 3]) (do)))))
