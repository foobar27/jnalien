(ns jnalien.core
  (:require [clojure.spec.alpha :as s])
  (:import [com.sun.jna Native Pointer Memory]))

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
       [_# x#] (.value x#))))

(defrecord WrappedArray [native-element-type value])

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



;; TODO dispatch int-array, object-array etc
;; TODO recursively un/wrap array elements
;; TODO how do I know how many elements are in the array?

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

(defmacro defn-native [lib-name return-type clj-fn-symbol native-fn-symbol & named-args-flat]
  (let [native-fn-name (name native-fn-symbol)
        named-args (for [[k v] (partition 2 named-args-flat)]
                     {:keyword k
                      :symbol (-> k name gensym)
                      :native-type v})]
    `(do
       (s/fdef ~clj-fn-symbol
         :args (s/cat ~@(apply concat
                               (for [{:keys [keyword native-type]} named-args]
                                 [keyword `(native-type->spec ~native-type)])))
         :ret (native-type->spec ~return-type))
       (let [function# (com.sun.jna.Function/getFunction ~lib-name ~native-fn-name)]
         (defn ~clj-fn-symbol [~@(map :symbol named-args)]
           (->> (.invoke function#
                         (native-type->class ~return-type)
                         (to-array [~@(for [{:keys [native-type symbol]} named-args]
                                        `(unwrap-native-value ~native-type ~symbol))]))
                (wrap-native-value ~return-type)))))))

