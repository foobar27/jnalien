(ns jnalien.core
  (:require [clojure.spec.alpha :as s])
  (:import [com.sun.jna Native Pointer Memory StringArray]))

(defmulti native-type->spec
  (fn [native-type] native-type))
(defmulti native-type->class
  (fn [native-type] native-type))
(defmulti unwrap-native-value
  (fn [native-type value] native-type))
(defmulti wrap-native-value
  (fn [native-type value] native-type))

(defmethod native-type->spec String [_] string?)
(defmethod native-type->class String [_] String)
(defmethod wrap-native-value String [_ x] (str x))
(defmethod unwrap-native-value String [_ x] x)

(defmethod native-type->spec Integer [_] int?)
(defmethod native-type->class Integer [_] Integer)
(defmethod wrap-native-value Integer [_ x] (int x))
(defmethod unwrap-native-value Integer [_ x] x)

(defmethod native-type->spec Void [_] (constantly true))
(defmethod native-type->class Void [_] Void)
(defmethod wrap-native-value Void [_ x] nil)
(defmethod unwrap-native-value Void [_ x] nil)

(defmulti nullptr (fn [native-type] native-type))

(defmulti complex-native-type->spec
  (fn [native-type] (first native-type)))
(defmulti complex-native-type->class
  (fn [native-type] (first native-type)))
(defmulti unwrap-complex-native-value
  (fn [native-type value] (first native-type)))
(defmulti wrap-complex-native-value
  (fn [native-type value] (first native-type)))

(defmethod native-type->spec :default [native-type]
  (if (sequential? native-type)
    (complex-native-type->spec native-type)
    (throw (IllegalArgumentException. (str "Unimplemented native-type->spec for " native-type)))))

(defmethod native-type->class :default [native-type]
  (if (sequential? native-type)
    (complex-native-type->class native-type)
    (throw (IllegalArgumentException. (str "Unimplemented native-type->class for " native-type)))))

(defmethod unwrap-native-value :default [native-type value]
  (if (sequential? native-type)
    (unwrap-complex-native-value native-type value)
    (throw (IllegalArgumentException. (str "Unimplemented unwrap-native-value for " native-type)))))

(defmethod wrap-native-value :default [native-type value]
  (if (sequential? native-type)
    (wrap-complex-native-value native-type value)
    (throw (IllegalArgumentException. (str "Unimplemented unwrap-native-value for " native-type)))))

;;
;; Transformed Input
;;

(defn transform-input [transformed-native-type spec transformation ]
  [::transformed-input transformed-native-type spec transformation])

(defmethod complex-native-type->spec ::transformed-input
  [[_ transformed-native-type spec transformation]]
  spec)

(defmethod complex-native-type->class ::transformed-input
  [[_ transformed-native-type spec transformation]]
  (native-type->class transformed-native-type))

(defmethod unwrap-complex-native-value ::transformed-input
  [[_ transformed-native-type spec transformation] value]
  (unwrap-native-value transformed-native-type(transformation value)))

;;
;; Pointers
;;

(defrecord WrappedPointer [native-type ^Pointer value])
(s/fdef defpointer
  :args (s/cat :kw qualified-keyword?))
(defmacro defpointer [kw]
  `(do
     (defmethod native-type->spec ~kw
       [_#]
       (s/and (fn [x#] (instance? WrappedPointer x#))
              (fn [ptr#]
                (= (.native-type ptr#) ~kw))))
     (defmethod native-type->class ~kw
       [_#]
       Pointer)
     (defmethod wrap-native-value ~kw
       [_# x#] (->WrappedPointer ~kw x#))
     (defmethod unwrap-native-value ~kw
       [_# x#] (.value x#))
     (defmethod nullptr ~kw
       [_#] (->WrappedPointer ~kw (Pointer/NULL)))))

;;
;; Arrays
;;

;; TODO I don't really like this solution. It would be nicer to alias
;; the array like vec does, while still transforming the values in
;; both directions
;; TODO not necesary for all the types?
;; TODO spec this function
(defn- copy-input-output-array-to-vec [a]
  (let [native-element-type (.native-element-type a)]
    (if (= String native-element-type)
      (vec (second (.native-value a)))
      (into [] (map #(wrap-native-value native-element-type %) (.native-value a))))))

(defprotocol InputOutputArray
  (native-element-type [this])
  (native-value [this]))

(deftype InputOutputArrayImpl [net sz nv]
  
  InputOutputArray
  (native-element-type [_] net)
  (native-value [_] nv)
  
  clojure.lang.Counted
  (count [_] sz)
  
  clojure.lang.IDeref
  (deref [this]
    (copy-input-output-array-to-vec this)))

(def ^:private primitive-array-ctors
  {Boolean boolean-array
   Character char-array
   Byte byte-array
   Short short-array
   Integer int-array
   Long long-array
   Float float-array
   Double double-array})
(defn- input-output-array-ctor [native-type]
  (or (primitive-array-ctors native-type)
      (primitive-array-ctors (native-type->class native-type))))

(defn ->input-output-array [[_ native-element-type] n-or-seq]
  (let [array-ctor (input-output-array-ctor native-element-type)
        native-element-class  (native-type->class native-element-type)
        _ (when-not (or array-ctor
                        (#{String Pointer} native-element-class))
            (throw (IllegalArgumentException.
                    "I only know how to do primitive, pointer and string arrays for now.")))
        n-or-seq (if (number? n-or-seq)
                   (if (= native-element-type String)
                     (repeatedly n-or-seq (fn [] nil))
                     n-or-seq)
                   (map (fn [x]
                          (if array-ctor
                            (unwrap-native-value native-element-type x)
                            (condp = native-element-class
                              Pointer (Pointer/nativeValue (unwrap-native-value native-element-type x))
                              String x)))
                        n-or-seq))
        n (if (number? n-or-seq) n-or-seq (count n-or-seq))]
    (->InputOutputArrayImpl native-element-type
                            n
                            (if (= String native-element-class)
                              (let [array (into-array String n-or-seq)]
                                [(StringArray. array) array])
                              (if array-ctor
                                (array-ctor n-or-seq)
                                (long-array n-or-seq))))))

(s/fdef implicit-array-size
  :args (s/cat :kw keyword?))
(defn implicit-array-size [kw]
  (fn [args]
    (count (get args kw))))

(defn input-output-array [native-element-type]
  [::input-output-array native-element-type])

(defn input-array [native-element-type]
  (let [input-output-array-type (input-output-array native-element-type)]
    (transform-input input-output-array-type
                     (s/coll-of (native-type->spec native-element-type))
                     (fn [x] (->input-output-array input-output-array-type x)))))

(defmethod complex-native-type->spec ::input-output-array
  [[_ native-element-type]]
  (s/and #(satisfies? InputOutputArray %1)
         #(native-element-type (.native-element-type %))))

(defmethod complex-native-type->class ::input-output-array
  [[_ native-element-type]]
  Pointer)

(defmethod unwrap-complex-native-value ::input-output-array
  [[_ native-element-type] value]
  (if (= native-element-type String)
    (first (.native-value value))
    (.native-value value)))

(defmethod wrap-complex-native-value ::input-output-array
  [[_ native-element-type] value]
  (throw (IllegalArgumentException. "Arrays not supported in return types")))


;;
;; Enums
;;

;; TODO do some sanity checks (duplicates & co)
(defn- parse-enum-arguments [args]
  ;; example input: [:a :b :c 10 :d 11 :e :f :g 20 :h]
  (if-let [partitioned (seq (partition-by keyword? args))]
    ;; Example: ((:a :b :c) (10) (:d) (11) (:e :f :g) (20) (:h))
    ;; Take care that the list always starts with a number-group
    (->> (if (-> partitioned first first keyword?)
           (concat [[0]] partitioned)
           partitioned)
         ;; Example: ([0] (:a :b :c) (10) (:d) (11) (:e :f :g) (20) (:h))
         (partition 2)
         ;; Example: (([0] (:a :b :c)) ((10) (:d)) ((11) (:e :f :g)) ((20) (:h)))
         ;; Inject the starting index (incrementally) into the keyword list
         (map (fn [[[start] kws]]
                (interleave (map #(+ % start) (range))
                            kws)))
         ;; Example: ((0 :a 1 :b 2 :c) (10 :d) (11 :e 12 :f 13 :g) (20 :h))
         ;; Flatten, then re-group by 2
         flatten
         (partition 2)
         ;; Example: ((0 :a) (1 :b) (2 :c) (10 :d) (11 :e) (12 :f) (13 :g) (20 :h))
         )))

(defmacro defenum [kw & args]
  (let [args (parse-enum-arguments args)]
    `(let [id->kw# ~(apply assoc {} (flatten args))
           kw->id# ~(apply assoc {} (flatten (map (fn [[x y]] [y x]) args)))
           kw-set# ~(into #{} (map second args))]
       (do
         (defmethod native-type->spec ~kw
           [_#]
           kw-set#)
         (defmethod native-type->class ~kw
           [_#]
           Integer)
         (defmethod wrap-native-value ~kw
           [_# x#]
           (get id->kw# x#))
         (defmethod unwrap-native-value ~kw
           [_# x#]
           (get kw->id# x#))))))

;;
;; Flags
;;

(defn- parse-flags-arguments [args]
  (partition 2 args))

(s/fdef defflags
  :args (s/cat :kw qualified-keyword?
               :args (s/+ (s/cat :k int?
                                 :v qualified-keyword?))))
(defmacro defflags [kw & args]
  (let [args (parse-flags-arguments args)]
    `(let [all-flags# ~(into {} (for [[k v] args]
                                  [v k]))]
       (defmethod native-type->spec ~kw
         [_#]
         (s/coll-of all-flags#))
       (defmethod native-type->class ~kw
         [_#]
         Integer)
       (defmethod wrap-native-value ~kw
         [_# i#]
         (reduce (fn [a# [k# v#]]
                   (if (pos? (bit-and i# v#))
                     (conj a# k#)
                     a#))
                 #{}
                 all-flags#))
       (defmethod unwrap-native-value ~kw
         [_# s#]
         (reduce (fn [a# x#]
                   (if-let [v# (get all-flags# x#)]
                     (bit-or a# v#)
                     (throw (IllegalArgumentException.
                             (str "Unknown " ~kw " flag: " x# ", possible flags: " (keys all-flags#))))))
                 0
                 s#)))))

;;
;; Implementation
;;

(defmacro defn-native [lib-name return-type clj-fn-symbol native-fn-symbol & named-args-flat]
  (let [native-fn-name (name native-fn-symbol)
        explicit-arg-values-sym (gensym "explicit-arg-values")
        ->named-arg (fn [k v]
                      {:keyword k
                       :symbol (-> k name gensym)
                       :native-type v})
        named-args (for [[k v] (partition 2 named-args-flat)]
                     (if (vector? v)
                       (merge (->named-arg k (first v))
                              (into {} (map vec (partition 2 (rest v)))))
                       (->named-arg k v)))]
    `(do
       (s/fdef ~clj-fn-symbol
         :args (s/cat ~@(apply concat
                               (for [{:keys [keyword native-type implicit]} named-args
                                     :when (nil? implicit)]
                                 [keyword `(native-type->spec ~native-type)])))
         :ret (native-type->spec ~return-type))
       (let [function# (com.sun.jna.Function/getFunction ~lib-name ~native-fn-name)]
         (defn ~clj-fn-symbol [~@(for [{:keys [symbol implicit]} named-args
                                       :when (nil? implicit)]
                                   symbol)]
           (let [~explicit-arg-values-sym (into {}
                                                [~@(for [{:keys [keyword symbol implicit]} named-args
                                                         :when (nil? implicit)]
                                                     [keyword symbol])])]
             (->> (.invoke function#
                           (native-type->class ~return-type)
                           (to-array [~@(for [{:keys [native-type symbol implicit]} named-args]
                                          (if implicit
                                            `(unwrap-native-value ~native-type (~implicit ~explicit-arg-values-sym))
                                            `(unwrap-native-value ~native-type ~symbol)))]))
                  (wrap-native-value ~return-type))))))))
