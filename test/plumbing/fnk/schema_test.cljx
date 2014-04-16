(ns plumbing.fnk.schema-test
  #+clj (:use clojure.test)
  #+cljs
  (:require-macros
   [cemerick.cljs.test :refer [is deftest testing]])
  (:require
   [schema.core :as s]
   [schema.test :as schema-test]
   [plumbing.core :as p #+cljs :include-macros #+cljs true]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as fnk-s]))

(deftest explicit-schema-key-map-test
  (is (= {:foo true :bar false}
         (fnk-s/explicit-schema-key-map
          {:foo s/Any (s/optional-key :bar) s/Any s/Keyword s/Keyword}))))

(deftest split-schema-keys-test
  (is (= [[:foo :bar] [:baz :bat]]
         (fnk-s/split-schema-keys
          (array-map :foo true :baz false :bar true :bat false)))))

(deftest merge-on-with-test
  (is (= {0 5 4 9 9 12}
         (#'plumbing.fnk.schema/merge-on-with #(quot % 2) min + {1 2 4 9 9 4} {9 8 0 3}))))

#+clj ;; TODO: Make cljs compatible
(deftest union-input-schemata-test
  (is (= {:a s/Any}
         (fnk-s/union-input-schemata {:a s/Any} {:a s/Any})))
  (is (= {:a s/Str}
         (fnk-s/union-input-schemata {:a s/Str} {(s/optional-key :a) s/Str})))
  (is (= {:a s/Str}
         (fnk-s/union-input-schemata {(s/optional-key :a) s/Str} {:a s/Any})))
  (is (= {:a (s/both s/Str Object)}
         (fnk-s/union-input-schemata {(s/optional-key :a) s/Str} {:a Object})))
  (is (= {:a {(s/optional-key :a1) s/Str
              :a2 Object
              :a3 s/Str}
          (s/optional-key :b) Object}
         (fnk-s/union-input-schemata {:a {(s/optional-key :a1) s/Str
                                          (s/optional-key :a2) Object}
                                      (s/optional-key :b) Object}
                                     {:a {:a2 Object :a3 s/Str}}))))

#+clj ;; TODO: Make cljs compatible
(deftest required-toplevel-keys-test
  (is (= #{:a :b}
         (set (fnk-s/required-toplevel-keys {:a {:a1 s/Str} :b Long (s/optional-key :c) Object})))))

#+clj ;; TODO: Make cljs compatible
(deftest guess-expr-output-schema-test
  (is (= `s/Any (@#'fnk-s/guess-expr-output-schema "foo")))
  (is (= {:a `s/Any :b `s/Any} (@#'fnk-s/guess-expr-output-schema {:a (+ 1 1) :b false})))
  (is (= `s/Any (@#'fnk-s/guess-expr-output-schema {'a (+ 1 1)}))))

(deftest compose-schemata-test
  (is (= [{:a s/Any :c s/Any :d s/Any}
          {:x s/Any}]
         (fnk-s/compose-schemata
          [{:a s/Any :b {:b1 s/Any} :c s/Any}
           {:x s/Any}]
          [{:c s/Any :d s/Any}
           {:b {:b1 s/Any}}])))

  (is (= [{:a s/Any (s/optional-key :e) s/Any :c s/Any :d s/Any}
          {:x s/Any}]
         (fnk-s/compose-schemata
          [{:a s/Any
            :b {:b1 s/Any}
            (s/optional-key :c) s/Any
            (s/optional-key :e) s/Any
            (s/optional-key :f) s/Any}
           {:x s/Any}]
          [{:c s/Any :d s/Any}
           {:b {:b1 s/Any} :c s/Any :f s/Any}])))

  (is (thrown? Exception
               (fnk-s/compose-schemata
                [{:a s/Any :b {:b1 s/Any} :c s/Any}
                 {:x s/Any}]
                [{:c s/Any :d s/Any}
                 {:b s/Any}]))))

#+clj ;; TODO: Make cljs compatible
(deftest sequence-schemata-test
  (is (= [{:a s/Any (s/optional-key :b) s/Any} {:c s/Any :o2 {:o21 s/Any}}]
         (fnk-s/sequence-schemata [{:a s/Any} {:c s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any} {:o21 s/Any}]])))
  (is (thrown? IllegalArgumentException
               (fnk-s/sequence-schemata [{:a s/Any} {:c s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any :o2 s/Any} {:o21 s/Any}]])))
  (is (thrown? IllegalArgumentException
               (fnk-s/sequence-schemata [{:a s/Any} {:c s/Any :o2 s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any} {:o21 s/Any}]])))
  (is (thrown? IllegalArgumentException
               (fnk-s/sequence-schemata [{:a s/Any :o2 s/Any} {:c s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any} {:o21 s/Any}]]))))

#+clj ;; TODO: Make cljs compatible
(deftest fnk-input-schemata-test
  (are [in fnk-form] (= in (pfnk/input-schema fnk-form))
       {:x s/Any :y s/Any s/Keyword s/Any}
       (p/fnk [x y])

       {:x s/Any (s/optional-key :y) s/Any :z s/Any s/Keyword s/Any}
       (p/fnk [x {y 2} z])

       {:x s/Any (s/optional-key :y) s/Any :z s/Any :q {:r s/Any s/Keyword s/Any} s/Keyword s/Any}
       (p/fnk [x {y 2} z [:q r] :as m & more])

       {(s/optional-key :x) s/Any :y {:alias s/Any s/Keyword s/Any} s/Keyword s/Any}
       (p/fnk [ {x 1} [:y alias]])

       {(s/optional-key :o1) s/Any
        :o2 s/Any
        :o3 {:x s/Any (s/optional-key :y) s/Any :z s/Any :q {:r s/Any s/Keyword s/Any} s/Keyword s/Any}
        s/Keyword s/Any}
       (p/fnk [{o1 1} o2 [:o3 x {y 2} z [:q r]]]))
  (is (= [1 2] ((eval `(p/fnk [[:x ~'x] [:y ~'y]] [~'x ~'y])) {:x {:x 1} :y {:y 2}})))
  (is (thrown? Throwable (eval `(p/fnk [{:y ~'x} {:y ~'y}] [~'x ~'y]))))
  (is (thrown? Throwable (eval `(p/fnk [{:x ~'x} {:y ~'x}] [~'x]))))
  (is (thrown? Throwable (eval `(p/fnk [[:x ~'x] ~'x] [~'x]))))
  (is (thrown? Throwable (eval `(p/fnk [{~'x 1} ~'x] [~'x])))))

#+clj ;; TODO: Make cljs compatible
(deftest fnk-out-schemata-test
  ;; Are somehow breaks the metadata on fnk forms.
  (is (= s/Any (pfnk/output-schema (p/fnk []))))
  (is (= s/Any (pfnk/output-schema (p/fnk [] (hash-map :x :y)))))
  (is (= {:o1 s/Any :o2 {:i s/Any :j {:q s/Any}}} (pfnk/output-schema (p/fnk [x] {:o1 x :o2 {:i x :j {:q 2}}}))))
  (is (= {:o1 s/Any :o2 s/Any} (pfnk/output-schema (p/fnk f :- {:o1 s/Any :o2 s/Any} [x]))))
  (is (= {:o1 s/Any :o2 s/Any} (pfnk/output-schema (p/fnk f :- {:o1 s/Any :o2 s/Any} [x]
                                                     {:o1 x :o2 {:i x :j {:q 2}}}))))
  (is (fn? (eval `(p/fnk f :- {:o1 s/Any} [] {:o1 2})))))

(use-fixtures :once schema-test/validate-schemas)
