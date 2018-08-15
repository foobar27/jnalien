(ns jnalien.core-test
  (:require [clojure.test :refer :all]
            [jnalien.core :refer [defpointer defenum]])
  (:import [com.sun.jna Pointer]))

(System/getProperty "java.library.path")

(defpointer ::MyPointer)

(defenum ::MyEnum
  :local
  :global)

(defmacro defn-native [& args]
  `(jnalien.core/defn-native "example" ~@args))

(defn-native String next-log nextLog)

(defn-native Void fn-void-void fnVoidVoid)

(defn-native Void fn-void-int fnVoidInt
  :x Integer)

(defn-native Integer fn-int-int fnIntInt
  :x Integer)

(defn-native ::MyEnum fn-enum-enum fnEnumEnum
  :x ::MyEnum)

(defn-native ::MyPointer create-random-my-pointer createRandomMyPointer)

(defn-native ::MyPointer create-my-pointer createMyPointer
  :i Integer
  :s String)

(defn-native Void dispose-my-pointer disposeMyPointer
  :ptr ::MyPointer)

(defn-native Integer my-pointer-get-i myPointerGetI
  :ptr ::MyPointer)

(defn-native String my-pointer-get-s myPointerGetS
  :ptr ::MyPointer)

;; (defn-native randomize-my-pointer-array randomizeMyPointerArray
;;   :n Integer
;;   :output (native-array ::MyPointer))

;; (defn-native ::MyPointer get-my-pointer-in-array getMyPointerInArray
;;   :array (native-array ::MyPointer)
;;   :index Integer)

;; (defn-native Void set-my-pointer-in-array setMyPointerInArray
;;   :array (native-array :MyPointer)
;;   :index Integer
;;   :pointer ::MyPointer)

;; (defn-native Integer sum sum
;;   :array (native-array Integer)
;;   :n Integer)

;; (defn-native fill-multiples fillMultiples
;;   :n Integer
;;   :k Integer
;;   :output (native-arrray Integer))

;; (defn-native concat-native concat
;;   :array (native-array String)
;;   :n Integer)

;; (defn-native create-multiples-as-string
;;   :n Integer
;;   :k Integer
;;   :output (native-array String))

;; (let [ptr (create-random-my-pointer)]
;;   (println (my-pointer-get-i ptr)
;;            (my-pointer-get-s ptr))
;;   (dispose-my-pointer ptr))



