(ns jnalien.core-test
  (:require [clojure.test :refer :all]
            [jnalien.core :refer [defpointer defenum native-array input-array
                                  nullptr ->native-array implicit-array-size]])
  (:import [com.sun.jna Pointer]))

(defpointer ::MyPointer)

(nullptr ::MyPointer)

(defenum ::MyEnum
  :local
  :global)

(defmacro defn-native [& args]
  `(jnalien.core/defn-native "example" ~@args))

(defn-native String next-log nextLog)
(defn-native Void clear-log nextLog)

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

(defn-native Void randomize-my-pointer-array randomizeMyPointerArray
  :n [Integer :implicit (implicit-array-size :output)]
  :output (native-array ::MyPointer))

(defn-native ::MyPointer get-my-pointer-in-array getMyPointerInArray
  :array (native-array ::MyPointer)
  :index Integer)

(defn-native Void set-my-pointer-in-array setMyPointerInArray
  :array (native-array :MyPointer)
  :index Integer
  :pointer ::MyPointer)

(defn-native Integer sum sum
  :array (native-array Integer)
  :n [Integer :implicit (implicit-array-size :array)])

(defn-native Integer sum-input-argument sum
  :array (input-array Integer)
  :n [Integer :implicit (implicit-array-size :array)])

(defn-native Void fill-multiples fillMultiples
  :n [Integer :implicit (implicit-array-size :output)]
  :k Integer
  :output (native-array Integer))

(defn-native Void invert-enum-array invertEnumArray
  :n [Integer :implicit (implicit-array-size :array)]
  :array (native-array ::MyEnum))

(defn-native String concat-native concat
  :array (native-array String)
  :n [Integer :implicit (implicit-array-size :array)])

(defn-native Void create-multiples-as-string createMultiplesAsString
  :n [Integer :implicit (implicit-array-size :output)]
  :k Integer
  :output (native-array String))

;;
;; Test infrastructure
;;

(defn- drain-log []
  (doall
   (take-while #(not (= % "EMPTY"))
               (repeatedly #(next-log)))))

(defmacro with-log [& body]
  `(do (clear-log)
       ~@body))

(defn- my-pointer->str [p]
  (let [i (my-pointer-get-i p)
        s (my-pointer-get-s p)]
    (str "MyPointer{" i ", " s "}")))

;;
;; Actual tests
;;

(deftest function-tests
  (testing "void -> void"
    (with-log
      (fn-void-void)
      (is (= ["fnVoidVoid()"] (drain-log)))))
  (testing "void -> int"
    (with-log
      (fn-void-int 5)
      (is (= ["fnVoidInt(5)"] (drain-log)))))
  (testing "int -> int"
    (with-log
      (let [value (fn-int-int 5)]
        (is (= [(str "fnIntInt(5)=" value)] (drain-log))))))
  (testing "enum -> enum"
    (with-log
      (let [value (fn-enum-enum :global)
            int-value ({:local 0 :global 1} value)]
        (is (= [(str "fnEnumEnum(1)=" int-value)]
               (drain-log))))))
  (testing "random MyPointer"
    (with-log
      (let [p (create-random-my-pointer)
            i (my-pointer-get-i p)
            s (my-pointer-get-s p)
            my-pointer (str "MyPointer{" i ", " s "}")]
        (dispose-my-pointer p)
        (is (= [(str "createRandomMyPointer()=" my-pointer)
                (str "myStructGetI(" my-pointer ")=" i)
                (str "myStructGetS(" my-pointer ")=" s)
                (str "disposeMyPointer(" my-pointer ")")]
               (drain-log))))))
  (testing "array of pointers"
    (with-log
      (let [a (->native-array (native-array ::MyPointer) (repeatedly 5 #(create-my-pointer 0 "")))]
        (is (= (repeatedly 5 (fn [] "MyPointer{0, }"))
               (map my-pointer->str @a)))
        (drain-log) ;; ignore result => tested in previous tests
        (randomize-my-pointer-array a)
        (let [internal-strings (take-while #(.startsWith % "createRandomMyPointer()=") (drain-log))]
          (drain-log) ;; ignore remaining internal logs
          (let [strs (map #(str "createRandomMyPointer()=" (my-pointer->str %)) @a)]
            (is (= strs
                   internal-strings)))))))
  (testing "sum of integers"
    (is (= 45
           (sum (->native-array (native-array Integer) (range 0 10))))))
  (testing "sum of integers (input argument)"
    (is (= 45
           (sum-input-argument (range 0 10)))))
  (testing "fill multiples"
    (is (= [0 10 20 30 40])
        (let [a (->native-array (native-array Integer) 5)]
          (fill-multiples 10 a)
          @a)))
  (testing "invert enum array"
    (is (= [:local :global :local :global]
           (let [a (->native-array (native-array ::MyEnum) [:global :local :global :local])]
             (invert-enum-array a)
             @a))))
  (testing "concat strings"
    (is (= "Hello world!"
           (concat-native (->native-array (native-array String) ["Hello", " ", "world", "!"])))))
  (testing create-multiples-as-string
    (is (= ["0" "10" "20" "30" "40"]
           (let [a (->native-array (native-array String) 5)]
             (create-multiples-as-string 10 a)
             @a)))))
